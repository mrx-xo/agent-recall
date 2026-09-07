#!/usr/bin/env python3
"""Bulk-summarize agent-shell transcripts through OpenRouter.

Writes TIMESTAMP.summary.md next to TIMESTAMP.md, which is the entire
contract agent-recall cares about: `agent-recall--needs-summary-p` is just
a `file-exists-p` on that path, so anything that produces the file works.
No Emacs, no ACP, no agent-shell config involved.

Transcripts are cleaned the same way `agent-recall--clean-transcript-string`
cleans them (header plus User/Agent turns, dropping Agent's Thoughts and Tool
Call sections) which drops about a third of the tokens before they are billed.

Usage:
    ./summarize-transcripts.py --limit 10            # trial run
    ./summarize-transcripts.py --dry-run             # count and estimate only
    ./summarize-transcripts.py --jobs 8              # the real thing

The key comes from $OPENROUTER_API_KEY, else from OpenCode's auth store. It is
never logged or echoed.
"""

import argparse
import json
import os
import pathlib
import random
import re
import sys
import threading
import time
import urllib.error
import urllib.request
from concurrent.futures import ThreadPoolExecutor

API_URL = "https://openrouter.ai/api/v1/chat/completions"
DEFAULT_MODEL = "qwen/qwen3.7-flash"

# Lifted verbatim from `agent-recall--summarize-prompt` so summaries written
# here are indistinguishable from ones the Emacs path produces.
PROMPT = """Summarize the following agent-shell conversation transcript.
Produce a structured summary in this exact format:

# Summary

**Topic:** One-line description of what the conversation was about
**Problem:** The problem or goal the user was trying to solve
**Outcome:** What was achieved or decided
**Tags:** comma-separated lowercase keywords for search

## Details
A concise 2-3 paragraph summary covering the key points, decisions made,
and any solutions or code changes produced.

IMPORTANT: Output ONLY the summary in the format above, nothing else.
Do not include any preamble or commentary.

Here is the transcript:

"""

HEADING = re.compile(r"^## ")
KEEP = re.compile(r"^## (User|Agent) \(")


def load_key():
    key = os.environ.get("OPENROUTER_API_KEY")
    if key:
        return key.strip()
    store = pathlib.Path.home() / ".local/share/opencode/auth.json"
    try:
        entry = json.loads(store.read_text()).get("openrouter") or {}
        key = entry.get("key") or entry.get("apiKey") or entry.get("api_key")
    except Exception as exc:
        sys.exit(f"cannot read an OpenRouter key: {exc}")
    if not key:
        sys.exit("no OpenRouter key found; set $OPENROUTER_API_KEY")
    return key.strip()


def clean(text):
    """Header plus User/Agent turns; thoughts and tool calls dropped."""
    out, keeping = [], True
    for line in text.split("\n"):
        if HEADING.match(line):
            keeping = bool(KEEP.match(line))
        if keeping:
            out.append(line)
    return "\n".join(out)


def summary_path(p):
    return p.with_suffix(".summary.md")


def find_transcripts(roots):
    seen = []
    for root in roots:
        for p in pathlib.Path(root).rglob("*/.agent-shell/transcripts/*.md"):
            if ".summary." in p.name:
                continue
            seen.append(p)
    return sorted(set(seen))


class Tally:
    def __init__(self):
        self.lock = threading.Lock()
        self.done = self.failed = self.skipped = 0
        self.tok_in = self.tok_out = 0
        self.cost = 0.0

    def add(self, usage, cost):
        with self.lock:
            self.done += 1
            self.tok_in += usage.get("prompt_tokens", 0)
            self.tok_out += usage.get("completion_tokens", 0)
            self.cost += cost


def call_api(key, model, body_text, retries=4):
    payload = json.dumps({
        "model": model,
        "messages": [{"role": "user", "content": PROMPT + body_text}],
        "max_tokens": 900,
        "temperature": 0.2,
        "usage": {"include": True},
        # Summarizing needs no chain of thought, and reasoning tokens bill as
        # output. Left on, qwen3.7-flash spends the whole max_tokens budget
        # thinking and returns content=None with finish_reason "length".
        "reasoning": {"enabled": False},
    }).encode()
    req = urllib.request.Request(
        API_URL, data=payload,
        headers={
            "Authorization": f"Bearer {key}",
            "Content-Type": "application/json",
            "HTTP-Referer": "https://github.com/mrx-xo/agent-recall",
            "X-Title": "agent-recall bulk summarize",
        },
    )
    last = None
    for attempt in range(retries):
        try:
            with urllib.request.urlopen(req, timeout=180) as r:
                return json.loads(r.read())
        except urllib.error.HTTPError as exc:
            last = f"HTTP {exc.code}"
            if exc.code not in (408, 429, 500, 502, 503, 504):
                raise RuntimeError(f"{last}: {exc.read()[:200].decode(errors='replace')}")
        except Exception as exc:
            last = str(exc)
        time.sleep((2 ** attempt) + random.random())
    raise RuntimeError(f"gave up after {retries} attempts: {last}")


def process(path, key, model, max_chars, tally, verbose):
    dest = summary_path(path)
    if dest.exists():
        with tally.lock:
            tally.skipped += 1
        return
    try:
        body = clean(path.read_text(encoding="utf-8", errors="replace")).strip()
        if not body:
            with tally.lock:
                tally.skipped += 1
            return
        if len(body) > max_chars:
            with tally.lock:
                tally.skipped += 1
            print(f"  skip (too big, {len(body):,} chars): {path.name}", flush=True)
            return

        resp = call_api(key, model, body)
        choice = resp["choices"][0]
        text = (choice["message"].get("content") or "").strip()
        if not text:
            raise RuntimeError(
                f"empty completion (finish_reason={choice.get('finish_reason')})")

        usage = resp.get("usage", {}) or {}
        cost = float(usage.get("cost", 0.0) or 0.0)

        # Write via a temp file so an interrupted run never leaves a partial
        # summary behind — a partial would be silently skipped on the rerun.
        tmp = dest.with_suffix(".summary.md.partial")
        tmp.write_text(text + "\n", encoding="utf-8")
        tmp.rename(dest)

        tally.add(usage, cost)
        if verbose:
            print(f"  ok  ${cost:.5f}  {usage.get('prompt_tokens', 0):>7,} tok  {path.name}",
                  flush=True)
    except Exception as exc:
        with tally.lock:
            tally.failed += 1
        print(f"  FAIL {path}: {exc}", flush=True)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--root", action="append", default=None,
                    help="search root (repeatable); default is $HOME")
    ap.add_argument("--model", default=DEFAULT_MODEL)
    ap.add_argument("--jobs", type=int, default=8)
    ap.add_argument("--limit", type=int, default=0, help="only process N transcripts")
    ap.add_argument("--max-chars", type=int, default=600_000,
                    help="skip transcripts larger than this after cleaning")
    ap.add_argument("--dry-run", action="store_true")
    ap.add_argument("--quiet", action="store_true")
    args = ap.parse_args()

    roots = args.root or [str(pathlib.Path.home())]
    everything = find_transcripts(roots)
    todo = [p for p in everything if not summary_path(p).exists()]

    print(f"transcripts found : {len(everything):,}")
    print(f"already summarized: {len(everything) - len(todo):,}")
    print(f"to do             : {len(todo):,}")

    if args.dry_run:
        chars = 0
        big = 0
        for p in todo:
            try:
                c = len(clean(p.read_text(encoding="utf-8", errors="replace")))
            except Exception:
                continue
            if c > args.max_chars:
                big += 1
                continue
            chars += c
        tok = chars / 4
        print(f"cleaned chars     : {chars:,}  (~{tok/1e6:.1f}M tokens)")
        print(f"oversized, skipped: {big}")
        print(f"rough input cost  : ${tok / 1e6 * 0.03:.2f} at $0.03/M")
        return

    if args.limit:
        todo = todo[:args.limit]
        print(f"limited to        : {len(todo):,}")
    if not todo:
        print("nothing to do")
        return

    key = load_key()
    tally = Tally()
    started = time.time()
    print(f"model             : {args.model}")
    print(f"workers           : {args.jobs}\n")

    with ThreadPoolExecutor(max_workers=args.jobs) as pool:
        for p in todo:
            pool.submit(process, p, key, args.model, args.max_chars,
                        tally, not args.quiet)

    elapsed = time.time() - started
    print(f"\ndone {tally.done}  failed {tally.failed}  skipped {tally.skipped}")
    print(f"tokens in {tally.tok_in:,}  out {tally.tok_out:,}")
    print(f"cost  ${tally.cost:.4f}")
    print(f"time  {elapsed:.1f}s")
    if tally.done:
        rate = tally.cost / tally.done
        print(f"per transcript ${rate:.5f}  ->  {len(everything)} would be "
              f"about ${rate * len(everything):.2f}")


if __name__ == "__main__":
    main()
