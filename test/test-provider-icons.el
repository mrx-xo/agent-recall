;;; test-provider-icons.el --- Tests for provider logo indicators -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for provider derivation (agent name / model id -> provider
;; symbol), entry-provider fallback precedence, and the icon prefix
;; gating.  Image rendering itself is graphic-display-only and is not
;; exercised in batch.
;; Run with:
;;   emacs --batch -l agent-recall.el -l test/test-provider-icons.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'agent-recall)

;; ---------------------------------------------------------------------------
;; Name -> provider mapping
;; ---------------------------------------------------------------------------

(ert-deftest test-provider-for-name-anthropic ()
  "Claude/Anthropic names map to `anthropic'."
  (should (eq 'anthropic (agent-recall--provider-for-name "Claude Code")))
  (should (eq 'anthropic (agent-recall--provider-for-name "claude-opus-4-8")))
  (should (eq 'anthropic (agent-recall--provider-for-name "Anthropic"))))

(ert-deftest test-provider-for-name-openai ()
  "Codex/OpenAI/GPT names map to `openai'."
  (should (eq 'openai (agent-recall--provider-for-name "Codex")))
  (should (eq 'openai (agent-recall--provider-for-name "gpt-4o")))
  (should (eq 'openai (agent-recall--provider-for-name "ChatGPT")))
  (should (eq 'openai (agent-recall--provider-for-name "OpenAI"))))

(ert-deftest test-provider-for-name-gemini ()
  "Gemini/Google names map to `gemini'."
  (should (eq 'gemini (agent-recall--provider-for-name "Gemini")))
  (should (eq 'gemini (agent-recall--provider-for-name "gemini-2.5-pro"))))

(ert-deftest test-provider-for-name-opencode ()
  "OpenCode names map to `opencode'."
  (should (eq 'opencode (agent-recall--provider-for-name "OpenCode")))
  (should (eq 'opencode (agent-recall--provider-for-name "opencode-go"))))

(ert-deftest test-provider-for-name-unknown-and-nil ()
  "Unknown names and non-strings return nil."
  (should-not (agent-recall--provider-for-name "some-local-llm"))
  (should-not (agent-recall--provider-for-name nil))
  (should-not (agent-recall--provider-for-name 42)))

;; ---------------------------------------------------------------------------
;; Entry provider precedence: :agent > header > model metadata
;; ---------------------------------------------------------------------------

(ert-deftest test-entry-provider-prefers-cached-agent ()
  "The cached `:agent' plist value wins without touching the file."
  (should (eq 'openai
              (agent-recall--entry-provider
               "/nonexistent/transcript.md" '(:agent "Codex")))))

(ert-deftest test-entry-provider-nil-when-nothing-matches ()
  "No agent, no readable file, no session id -> nil."
  (should-not (agent-recall--entry-provider
               "/nonexistent/transcript.md" '(:agent "mystery-agent"))))

;; ---------------------------------------------------------------------------
;; Icon prefix gating
;; ---------------------------------------------------------------------------

(ert-deftest test-provider-icon-empty-when-disabled ()
  "With the feature off, the prefix is the empty string."
  (let ((agent-recall-show-provider-icons nil))
    (should (equal "" (agent-recall--provider-icon
                       "/x.md" '(:agent "Claude Code"))))))

(ert-deftest test-provider-icon-empty-for-unknown-provider ()
  "Enabled but unrecognized provider still yields an empty prefix."
  (let ((agent-recall-show-provider-icons t))
    (should (equal "" (agent-recall--provider-icon
                       "/x.md" '(:agent "mystery-agent"))))))

(ert-deftest test-provider-icon-nonempty-when-enabled-and-known ()
  "Enabled with a known provider yields a non-empty propertized prefix."
  (let ((agent-recall-show-provider-icons t))
    (let ((s (agent-recall--provider-icon "/x.md" '(:agent "Claude Code"))))
      (should (stringp s))
      (should (> (length s) 0))
      ;; Either an image `display' (graphic) or a faced initial (tty).
      (should (or (get-text-property 0 'display s)
                  (get-text-property 0 'face s))))))

(provide 'test-provider-icons)
;;; test-provider-icons.el ends here
