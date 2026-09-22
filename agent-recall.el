;;; agent-recall.el --- Search and browse agent-shell conversation transcripts -*- lexical-binding: t -*-

;; Author: Marcos Andrade <https://github.com/Marx-A00>
;; URL: https://github.com/Marx-A00/agent-recall
;; Version: 0.7.0
;; Package-Requires: ((emacs "29.1") (agent-shell "0.1.0"))
;; Keywords: tools, convenience, ai

;; This file is NOT part of GNU Emacs.

;; MIT License
;;
;; Copyright (c) 2026 Marcos Andrade
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;;; Commentary:
;;
;; agent-recall provides search, browsing, and session resume capabilities
;; for agent-shell conversation transcripts.
;;
;; agent-shell (https://github.com/xenodium/agent-shell) automatically
;; saves full conversation transcripts as Markdown or org-mode files in
;; `.agent-shell/transcripts/' directories within your projects, or in
;; directories configured via `agent-recall-extra-transcript-dirs'.  Over
;; time these accumulate into a rich knowledge base of AI interactions,
;; but there's no built-in way to search across them or resume past
;; conversations.
;;
;; agent-recall maintains a persistent index of all transcripts,
;; provides fast full-text search powered by ripgrep, and can resume
;; past agent-shell sessions from any transcript.  The index grows
;; automatically as you use agent-shell (via a mode hook) and can
;; be rebuilt from scratch with `agent-recall-reindex'.
;;
;; Quick start:
;;
;;   ;; First-time setup: build the index
;;   (setq agent-recall-search-paths '("~/projects" "~/work"))
;;   M-x agent-recall-reindex
;;
;;   ;; Search all transcripts
;;   M-x agent-recall-search
;;
;;   ;; Browse transcripts by project and date
;;   M-x agent-recall-browse
;;
;;   ;; Resume a past conversation
;;   M-x agent-recall-resume
;;
;;   ;; See stats about your transcript collection
;;   M-x agent-recall-stats
;;
;;   ;; Auto-activate transcript-mode when visiting transcripts
;;   (global-agent-recall-transcript-mode 1)
;;
;; Session resume setup (optional):
;;
;;   ;; Embed session IDs in new transcripts for instant resume
;;   (add-hook 'agent-shell-mode-hook #'agent-recall-track-sessions)
;;
;;   ;; Backfill session IDs into existing transcripts
;;   M-x agent-recall-backfill          ; dry-run (preview only)
;;   C-u C-u M-x agent-recall-backfill  ; write session IDs

;;; Code:

(require 'agent-shell)
(require 'cl-lib)
(require 'grep)
(require 'iso8601)
(require 'json)
(require 'map)
(require 'seq)
(require 'subr-x)

(defvar deadgrep-extra-arguments)
(defvar counsel-rg-base-command)
(declare-function evil-define-key* "evil-core" (state keymap key def &rest bindings))
(declare-function deadgrep "deadgrep" (search-term &optional directory))
(declare-function counsel-rg "counsel" (&optional initial-input initial-directory extra-rg-args rg-prompt))
(defvar consult-ripgrep-args)
(declare-function consult-ripgrep "consult" (&optional dir initial))
(declare-function agent-recall-consult--browse-read "agent-recall-consult")
(declare-function agent-recall-consult--browse-preview-state "agent-recall-consult")
(declare-function agent-recall-consult--suspend-available-p "agent-recall-consult")
(declare-function ivy-read "ivy")
(declare-function ivy-state-current "ivy")
(declare-function ivy--get-window "ivy")
(defvar ivy-last)
(defvar ivy-update-fns-alist)
(defvar ivy-unwind-fns-alist)
(defvar embark-keymap-alist)
(declare-function agent-shell-select-config "agent-shell"
                  (&key prompt))
(declare-function agent-shell--config-option-set-thought-level-id
                  "agent-shell" (&rest arguments))

;;;; Customization

(defgroup agent-recall nil
  "Search and browse agent-shell conversation transcripts."
  :group 'tools
  :prefix "agent-recall-")

(defface agent-recall-header-key
  '((t :inherit warning))
  "Face for keybinding letters in the transcript header line."
  :group 'agent-recall)

(defface agent-recall-header-label
  '((t :inherit default))
  "Face for labels in the transcript header line."
  :group 'agent-recall)

(defface agent-recall-label
  '((t :inherit font-lock-keyword-face))
  "Face for user-assigned session labels in pickers and candidates."
  :group 'agent-recall)

(defface agent-recall-provider-anthropic
  '((t :foreground "#D97757" :weight bold))
  "Fallback face for the Anthropic provider indicator on text terminals."
  :group 'agent-recall)

(defface agent-recall-provider-openai
  '((t :inherit default :weight bold))
  "Fallback face for the OpenAI provider indicator on text terminals."
  :group 'agent-recall)

(defface agent-recall-provider-gemini
  '((t :foreground "#9B72CB" :weight bold))
  "Fallback face for the Gemini provider indicator on text terminals."
  :group 'agent-recall)

(defcustom agent-recall-search-paths nil
  "Root directories to scan when rebuilding the transcript index.
Used only by `agent-recall-reindex'.  Each directory is recursively
searched for `.agent-shell/transcripts/' subdirectories up to
`agent-recall-max-depth' levels deep.

Must be set before calling `agent-recall-reindex'.  Example:

  (setq agent-recall-search-paths \\='(\"~/projects\" \"~/work\"))"
  :type '(repeat directory)
  :group 'agent-recall)

(defcustom agent-recall-max-depth 6
  "Maximum directory depth when scanning for transcript directories.
Used only by `agent-recall-reindex'.  Increase if your projects are
deeply nested.  Lower values speed up the reindex scan."
  :type 'integer
  :group 'agent-recall)

(defcustom agent-recall-transcript-dir-name ".agent-shell/transcripts"
  "Relative path that identifies transcript directories within projects.
This is the conventional path used by agent-shell."
  :type 'string
  :group 'agent-recall)

(define-obsolete-variable-alias 'agent-recall-file-pattern
  'agent-recall-file-patterns "0.6.0")

(defcustom agent-recall-file-patterns '("*.md" "*.org")
  "List of glob patterns matching transcript files.
Used by `agent-recall-reindex' and all search backends to find
transcripts in supported formats."
  :type '(repeat string)
  :group 'agent-recall)

(defun agent-recall--file-patterns ()
  "Return `agent-recall-file-patterns' normalized to a list.
Legacy Customize values set via the obsolete `agent-recall-file-pattern'
alias may still be a string; consumers expect a list of globs."
  (if (stringp agent-recall-file-patterns)
      (list agent-recall-file-patterns)
    agent-recall-file-patterns))

(defcustom agent-recall-extra-transcript-dirs nil
  "Additional directories containing transcript files to index directly.
Unlike `agent-recall-search-paths' (which scans recursively for
`.agent-shell/transcripts/' subdirectories), these directories are
indexed as-is.  Use this for transcripts stored outside the conventional
layout, e.g. `org-mode' transcripts from `agent-shell-org-transcript'.

Each entry is a plist (:dir DIR :project PROJECT) where:
  :dir      - the directory path (required)
  :project  - display name (optional; derived from file properties if nil)

Example:
  (setq agent-recall-extra-transcript-dirs
        \\='((:dir \"~/org/agent-shell/\")))"
  :type '(repeat (plist :key-type symbol :value-type string))
  :group 'agent-recall)

(defcustom agent-recall-rg-executable "rg"
  "Path or name of the ripgrep executable."
  :type 'string
  :group 'agent-recall)

(defcustom agent-recall-search-extra-args '("--follow" "--sort=modified")
  "Extra arguments passed to ripgrep during searches.
Useful for controlling sort order, context lines, etc."
  :type '(repeat string)
  :group 'agent-recall)

(defcustom agent-recall-search-context-lines 2
  "Number of context lines shown around each search match.
Passed as -C to ripgrep."
  :type 'integer
  :group 'agent-recall)

(defcustom agent-recall-search-function 'grep
  "Search backend used by `agent-recall-search'.
Determines how search results are displayed.

Possible values:
  `grep'             - built-in grep-mode (default, always available)
  `deadgrep'         - deadgrep buffer (requires `deadgrep' package)
  `counsel-rg'       - ivy/counsel live search (requires `counsel')
  `consult-ripgrep'  - vertico/consult live search (requires `consult')

Each backend receives the search query and the list of indexed
transcript directories.  If the chosen backend is not installed,
falls back to `grep'."
  :type '(choice (const :tag "grep-mode (built-in)" grep)
                 (const :tag "deadgrep" deadgrep)
                 (const :tag "counsel-rg (ivy)" counsel-rg)
                 (const :tag "consult-ripgrep (vertico)" consult-ripgrep))
  :group 'agent-recall)

(defcustom agent-recall-index-file
  (expand-file-name "agent-recall/index.el"
                    (if (boundp 'no-littering-var-directory)
                        no-littering-var-directory
                      user-emacs-directory))
  "Path to the persistent transcript index file.
The index stores metadata (file paths, project names, timestamps,
session IDs, session titles, and previews) for all known transcripts.  It is updated
automatically when new agent-shell sessions are created (via the
`agent-recall-track-sessions' hook) and can be rebuilt from scratch
with `agent-recall-reindex'."
  :type 'file
  :group 'agent-recall)

(defcustom agent-recall-browse-sort 'date-desc
  "Sort order for `agent-recall-browse'.
Possible values:
  `date-desc'     - newest first by creation date (default)
  `date-asc'      - oldest first by creation date
  `modified-desc' - most recently modified first
  `modified-asc'  - least recently modified first
  `project'       - group by project name"
  :type '(choice (const :tag "Newest first (created)" date-desc)
                 (const :tag "Oldest first (created)" date-asc)
                 (const :tag "Recently modified first" modified-desc)
                 (const :tag "Least recently modified first" modified-asc)
                 (const :tag "By project" project))
  :group 'agent-recall)

(defcustom agent-recall-resume-continue-transcript t
  "Whether resumed sessions append to the original transcript file.
When non-nil (the default), resuming a session continues writing to
the same transcript file, keeping the full conversation in one place.
When nil, agent-shell creates a new transcript file as usual."
  :type 'boolean
  :group 'agent-recall)

(defcustom agent-recall-metadata-file
  (expand-file-name "agent-recall/metadata.el"
                    (if (boundp 'no-littering-var-directory)
                        no-littering-var-directory
                      user-emacs-directory))
  "Path to the persistent session metadata file.
Stores a per-session alist of arbitrary metadata (model, effort,
permission mode, and anything added via
`agent-recall-capture-functions'), keyed by session ID.  Kept
separate from `agent-recall-index-file' so that
`agent-recall-reindex' never wipes it."
  :type 'file
  :group 'agent-recall)

(defcustom agent-recall-resume-restore-preferences t
  "Whether resuming a session restores its saved preferences.
Preferences (model, effort, permission mode, and any custom data
captured via `agent-recall-capture-functions') are recorded per
session in `agent-recall-metadata-file' and re-applied on resume.
When set to `ask', prompts before restoring."
  :type '(choice (const :tag "Always" t)
                 (const :tag "Ask each time" ask)
                 (const :tag "Never" nil))
  :group 'agent-recall)

(defcustom agent-recall-show-provider-icons nil
  "When non-nil, prefix picker candidates with the AI provider's logo.
The provider (Anthropic, OpenAI, or Gemini) is derived from each
transcript's agent/model.  Graphic displays show a real SVG logo; text
terminals fall back to a colored initial.  After enabling, run
\\[agent-recall-reindex] so the provider is cached in the index and
picker building stays fast."
  :type 'boolean
  :group 'agent-recall)

(defcustom agent-recall-icon-directory
  (expand-file-name
   "icons"
   (file-name-directory (or load-file-name buffer-file-name default-directory)))
  "Directory containing provider logo SVG assets (e.g. `openai.svg')."
  :type 'directory
  :group 'agent-recall)

(defcustom agent-recall-provider-icon-height nil
  "Height in pixels for provider logos in pickers.
When nil, each logo is sized to the default font height."
  :type '(choice (const :tag "Match font height" nil)
                 (integer :tag "Pixels"))
  :group 'agent-recall)

(defcustom agent-recall-claude-config-dir
  (expand-file-name ".claude" (getenv "HOME"))
  "Path to the Claude CLI configuration directory.
Used for retroactive session matching.  Contains `projects/'
subdirectory with session data."
  :type 'directory
  :group 'agent-recall)

(defcustom agent-recall-session-match-window 120
  "Maximum seconds between transcript and session timestamps for matching.
The session `created' timestamp is always slightly after the transcript
`Started' timestamp due to ACP bootstrap delay (typically 20-60s).
Increase this if matching fails due to slow initialization."
  :type 'integer
  :group 'agent-recall)

(defcustom agent-recall-summarize-timeout 120
  "Maximum seconds to wait for a single transcript summarization.
If the ACP does not return a result within this time, the transcript
is skipped and processing continues with the next one."
  :type 'integer
  :group 'agent-recall)

(defcustom agent-recall-auto-transcript-mode t
  "Whether agent-recall commands automatically enable transcript-mode.
When non-nil, opening a transcript via `agent-recall-browse',
`agent-recall-search', or `agent-recall-search-live' activates
`agent-recall-transcript-mode'.  Set to nil to browse transcripts
as plain files.  You can always toggle transcript-mode manually
with \\[agent-recall-transcript-mode]."
  :type 'boolean
  :group 'agent-recall)

(defcustom agent-recall-browse-preview t
  "Whether to show live preview in `agent-recall-browse' when available.
When non-nil, uses consult or ivy for live transcript preview as you
navigate candidates.  When nil, falls back to plain `completing-read'."
  :type 'boolean
  :group 'agent-recall)

;;;; Internal State

(defvar agent-recall--index nil
  "In-memory hash-table of indexed transcripts.
Keys are absolute file paths, values are plists
\(:project :dir :timestamp :session-id :title :preview).")

(defvar agent-recall--index-loaded-p nil
  "Non-nil if the index has been loaded from disk this Emacs session.")

(defvar agent-recall--symlink-dir nil
  "Path to temporary symlink directory for multi-dir search backends.")

(defvar agent-recall--browse-history nil
  "History list for `agent-recall-browse' selections.")

(cl-defstruct
    (agent-recall--navigation-session
     (:constructor agent-recall--navigation-session-create))
  "Origin information for a transcript navigation session."
  id kind backend minibuffer origin-window selected-file selected-line
  transcript-buffer origin-buffer origin-marker valid-function
  resume-function abort-function state)

(defvar agent-recall--navigation-session-counter 0
  "Monotonic counter used to create navigation session identifiers.")

(defvar agent-recall--navigation-sessions nil
  "Live transcript navigation sessions, newest first.")

(defvar-local agent-recall--navigation-origins nil
  "Navigation sessions which can return from the current transcript.")

(defvar-local agent-recall--picker-navigation-session nil
  "Navigation session owned by the current picker minibuffer.")

(defvar-local agent-recall--search-origin-kind nil
  "Persistent search backend represented by the current result buffer.")

(defvar agent-recall--pending-search-origin nil
  "Result buffer origin awaiting a transcript file visit.")

(defvar agent-recall--pending-search-origin-timer nil
  "Timer which expires `agent-recall--pending-search-origin'.")

(defvar agent-recall--session-id-cache (make-hash-table :test 'equal)
  "Cache mapping transcript file paths to session IDs.
Values are session ID strings, or the symbol `none' for unresolvable.")

(defvar-local agent-recall--pending-session-id nil
  "Session ID captured from `init-session' event, waiting to be written.")

(defvar-local agent-recall--session-id-written-p nil
  "Non-nil if session ID has already been written to this buffer's transcript.")

(defvar-local agent-recall--transcript-session-id nil
  "The session ID associated with the current transcript buffer.")

(defvar-local agent-recall--search-buffer-p nil
  "Non-nil in buffers created by agent-recall search commands.")

(defvar agent-recall--navigation-orphan-timer nil
  "Timer used to abort suspended pickers whose transcript disappeared.")

(defun agent-recall--canonical-file (file)
  "Return the canonical absolute name of FILE."
  (when file
    (condition-case nil
        (file-truename (expand-file-name file))
      (file-error (expand-file-name file)))))

(defun agent-recall--summary-parent-file (file)
  "Return the transcript FILE belongs to.
Summaries live beside their transcript as TIMESTAMP.summary.EXT, so a
hit inside one is a hit on TIMESTAMP.EXT.  Non-summary files are
returned unchanged."
  (when file
    (save-match-data
      (if (string-match "\\.summary\\(\\.[^./]+\\)\\'" file)
          (concat (substring file 0 (match-beginning 0))
                  (match-string 1 file))
        file))))

(defun agent-recall--candidate-key (candidate)
  "Return the property-free completion identity for CANDIDATE."
  (and candidate (substring-no-properties candidate)))

(defun agent-recall--candidate-file (candidate)
  "Return the canonical file payload stored on CANDIDATE."
  (and candidate (get-text-property 0 'agent-recall-file candidate)))

(defun agent-recall--candidate-line (candidate)
  "Return the optional line payload stored on CANDIDATE."
  (and candidate (get-text-property 0 'agent-recall-line candidate)))

(defun agent-recall--candidate-kind (candidate)
  "Return the origin kind stored on CANDIDATE."
  (and candidate (get-text-property 0 'agent-recall-origin-kind candidate)))

(defun agent-recall--candidate-identity (file &optional line kind)
  "Return a stable payload identity for FILE, LINE, and KIND."
  (format "%s\0%s\0%s"
          (agent-recall--canonical-file file)
          (or line "")
          (or kind "")))

(defun agent-recall--make-candidate (display file &optional line kind)
  "Make a completion candidate from DISPLAY with FILE, LINE, and KIND payload."
  (let* ((canonical (agent-recall--canonical-file file))
         (candidate (copy-sequence display))
         (props (list 'agent-recall-file canonical
                      'agent-recall-line line
                      'agent-recall-origin-kind kind
                      'agent-recall-identity
                      (agent-recall--candidate-identity canonical line kind))))
    (when (> (length candidate) 0)
      (add-text-properties 0 (length candidate) props candidate))
    candidate))

(defun agent-recall--disambiguate-candidates (candidates)
  "Return CANDIDATES with unique raw completion strings.
Duplicate visible labels receive a canonical path suffix.  Candidate
payload properties remain authoritative even when a completion UI strips
other display properties."
  (let ((counts (make-hash-table :test 'equal))
        (seen (make-hash-table :test 'equal)))
    (dolist (candidate candidates)
      (cl-incf (gethash (agent-recall--candidate-key candidate) counts 0)))
    (mapcar
     (lambda (candidate)
       (let* ((label (agent-recall--candidate-key candidate))
              (file (agent-recall--candidate-file candidate))
              (line (agent-recall--candidate-line candidate))
              (kind (agent-recall--candidate-kind candidate))
              (base (if (> (gethash label counts 0) 1)
                        (concat candidate
                                (propertize
                                 (format "  <%s%s>"
                                         (abbreviate-file-name file)
                                         (if line (format ":%d" line) ""))
                                 'face 'shadow))
                      candidate))
              (raw (agent-recall--candidate-key base))
              (ordinal (1+ (gethash raw seen 0))))
         (puthash raw ordinal seen)
         (when (> ordinal 1)
           (setq base (concat base
                              (propertize (format " #%d" ordinal) 'face 'shadow))))
         (agent-recall--make-candidate base file line kind)))
     candidates)))

(defun agent-recall--candidate-lookup (candidate candidates)
  "Return the original CANDIDATE member from CANDIDATES, if present."
  (and candidate
       (seq-find (lambda (item)
                   (equal (agent-recall--candidate-key item)
                          (agent-recall--candidate-key candidate)))
                 candidates)))

(cl-defun agent-recall--navigation-new-session
    (kind backend &key origin-window valid-function resume-function abort-function
          origin-buffer origin-marker)
  "Create and register a navigation session of KIND for BACKEND.
ORIGIN-WINDOW is the picker or result window.  VALID-FUNCTION,
RESUME-FUNCTION, and ABORT-FUNCTION implement backend operations.
ORIGIN-BUFFER and ORIGIN-MARKER identify a persistent result location."
  (let ((session
         (agent-recall--navigation-session-create
          :id (format "agent-recall-%d-%d"
                      (emacs-pid)
                      (cl-incf agent-recall--navigation-session-counter))
          :kind kind
          :backend backend
          :origin-window origin-window
          :origin-buffer origin-buffer
          :origin-marker origin-marker
          :valid-function valid-function
          :resume-function resume-function
          :abort-function abort-function
          :state 'picker)))
    (push session agent-recall--navigation-sessions)
    (add-hook 'delete-frame-functions #'agent-recall--navigation-frame-deleted)
    session))

(defun agent-recall--navigation-session-valid-p (session)
  "Return non-nil when SESSION still identifies a usable origin."
  (and (agent-recall--navigation-session-p session)
       (not (eq (agent-recall--navigation-session-state session) 'closed))
       (pcase (agent-recall--navigation-session-backend session)
         ('suspended
          (when-let ((valid (agent-recall--navigation-session-valid-function
                             session)))
            (condition-case nil
                (funcall valid session)
              (error nil))))
         ('persistent
          (let ((buffer (agent-recall--navigation-session-origin-buffer session))
                (marker (agent-recall--navigation-session-origin-marker session)))
            (and (buffer-live-p buffer)
                 (markerp marker)
                 (eq (marker-buffer marker) buffer))))
         (_ nil))))

(defun agent-recall--navigation-detach (session)
  "Detach SESSION from its transcript buffer."
  (when-let ((buffer (agent-recall--navigation-session-transcript-buffer session)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq agent-recall--navigation-origins
              (delq session agent-recall--navigation-origins))
        (unless agent-recall--navigation-origins
          (remove-hook 'kill-buffer-hook
                       #'agent-recall--navigation-transcript-killed t))))
    (setf (agent-recall--navigation-session-transcript-buffer session) nil)))

(defun agent-recall--navigation-attach (session buffer)
  "Attach SESSION as the newest navigation origin in BUFFER."
  (agent-recall--navigation-detach session)
  (setf (agent-recall--navigation-session-transcript-buffer session) buffer)
  (with-current-buffer buffer
    (setq agent-recall--navigation-origins
          (cons session (delq session agent-recall--navigation-origins)))
    (add-hook 'kill-buffer-hook #'agent-recall--navigation-transcript-killed nil t)))

(defun agent-recall--navigation-cleanup (session)
  "Remove all live references owned by navigation SESSION."
  (when (agent-recall--navigation-session-p session)
    (agent-recall--navigation-detach session)
    (when-let ((minibuffer (agent-recall--navigation-session-minibuffer session)))
      (when (buffer-live-p minibuffer)
        (with-current-buffer minibuffer
          (when (eq agent-recall--picker-navigation-session session)
            (setq agent-recall--picker-navigation-session nil)))))
    (when-let ((marker (agent-recall--navigation-session-origin-marker session)))
      (set-marker marker nil))
    (setf (agent-recall--navigation-session-state session) 'closed)
    (setq agent-recall--navigation-sessions
          (delq session agent-recall--navigation-sessions))
    (unless agent-recall--navigation-sessions
      (remove-hook 'delete-frame-functions #'agent-recall--navigation-frame-deleted))
    (agent-recall--navigation-schedule-orphan-abort)))

(defun agent-recall--navigation-current-origin ()
  "Return the newest valid navigation origin in the current buffer."
  (let ((session (car agent-recall--navigation-origins)))
    (and (agent-recall--navigation-session-valid-p session) session)))

(defun agent-recall--navigation-has-origin-p ()
  "Return non-nil when the current transcript has a valid return origin."
  (and (agent-recall--navigation-current-origin) t))

(defun agent-recall--navigation-restore-persistent (session)
  "Return to the persistent result buffer recorded by SESSION."
  (let ((buffer (agent-recall--navigation-session-origin-buffer session))
        (marker (agent-recall--navigation-session-origin-marker session))
        (window (agent-recall--navigation-session-origin-window session)))
    (unwind-protect
        (progn
          (agent-recall--navigation-detach session)
          (if (window-live-p window)
              (progn
                (select-window window)
                (set-window-buffer window buffer))
            (pop-to-buffer buffer))
          (goto-char marker))
      (agent-recall--navigation-cleanup session))))

(defun agent-recall--navigation-request-abort (session)
  "Abort SESSION and clean it up if its backend does not unwind."
  (agent-recall--navigation-detach session)
  (condition-case err
      (if-let ((abort (agent-recall--navigation-session-abort-function session)))
          (unless (funcall abort session)
            (agent-recall--navigation-schedule-orphan-abort))
        (agent-recall--navigation-cleanup session))
    (error
     (agent-recall--navigation-cleanup session)
     (signal (car err) (cdr err)))))

(defun agent-recall--navigation-abort-orphan ()
  "Abort the newest suspended picker which has lost its transcript."
  (setq agent-recall--navigation-orphan-timer nil)
  (when-let ((session
              (seq-find
               (lambda (item)
                 (and (eq (agent-recall--navigation-session-backend item)
                          'suspended)
                      (memq (agent-recall--navigation-session-state item)
                            '(suspended orphaned))
                      (not (buffer-live-p
                            (agent-recall--navigation-session-transcript-buffer
                             item)))))
               agent-recall--navigation-sessions)))
    (agent-recall--navigation-request-abort session)))

(defun agent-recall--navigation-schedule-orphan-abort ()
  "Schedule cleanup of any suspended picker without a transcript."
  (when (and
         (seq-some
          (lambda (item)
            (and (eq (agent-recall--navigation-session-backend item)
                     'suspended)
                 (memq (agent-recall--navigation-session-state item)
                       '(suspended orphaned))
                 (not (buffer-live-p
                       (agent-recall--navigation-session-transcript-buffer
                        item)))))
          agent-recall--navigation-sessions)
         (not (timerp agent-recall--navigation-orphan-timer)))
    (setq agent-recall--navigation-orphan-timer
          (run-at-time 0.05 nil #'agent-recall--navigation-abort-orphan))))

(defun agent-recall--navigation-transcript-killed ()
  "Arrange cleanup when a transcript with navigation origins is killed."
  (let ((origins agent-recall--navigation-origins))
    (setq agent-recall--navigation-origins nil)
    (dolist (session origins)
      (setf (agent-recall--navigation-session-transcript-buffer session) nil)
      (if (eq (agent-recall--navigation-session-backend session) 'suspended)
          (agent-recall--navigation-schedule-orphan-abort)
        (agent-recall--navigation-cleanup session)))))

(defun agent-recall--navigation-frame-deleted (frame)
  "Clean navigation sessions whose picker or origin belonged to FRAME."
  (dolist (session (copy-sequence agent-recall--navigation-sessions))
    (let ((origin (agent-recall--navigation-session-origin-window session))
          (minibuffer (agent-recall--navigation-session-minibuffer session)))
      (when (or (and (windowp origin) (eq (window-frame origin) frame))
                (and (buffer-live-p minibuffer)
                     (when-let ((window (get-buffer-window minibuffer t)))
                       (eq (window-frame window) frame))))
        (if (eq (agent-recall--navigation-session-backend session) 'suspended)
            (run-at-time 0 nil #'agent-recall--navigation-request-abort session)
          (agent-recall--navigation-cleanup session))))))

;;;; Persistent Index

(defun agent-recall--index-load ()
  "Read the index file from disk into `agent-recall--index'.
Sets `agent-recall--index-loaded-p' on success.  If the file is
missing or corrupt, sets an empty hash-table."
  (let ((file agent-recall-index-file))
    (if (file-exists-p file)
        (condition-case err
            (with-temp-buffer
              (insert-file-contents file)
              (let ((data (read (current-buffer))))
                (if (hash-table-p data)
                    (setq agent-recall--index data)
                  (setq agent-recall--index (make-hash-table :test 'equal))
                  (message "agent-recall: index file corrupt, starting fresh"))))
          (error
           (setq agent-recall--index (make-hash-table :test 'equal))
           (message "agent-recall: failed to load index: %s" (error-message-string err))))
      (setq agent-recall--index (make-hash-table :test 'equal)))
    (setq agent-recall--index-loaded-p t)))

(defun agent-recall--index-save ()
  "Write `agent-recall--index' to disk atomically.
Writes to a temporary file then renames to `agent-recall-index-file'."
  (when agent-recall--index
    (let* ((file agent-recall-index-file)
           (dir (file-name-directory file)))
      (unless (file-directory-p dir)
        (make-directory dir t))
      (let ((temp (make-temp-file (expand-file-name ".index-" dir))))
      (with-temp-file temp
        (insert ";; agent-recall transcript index -*- no-byte-compile: t -*-\n")
        (insert (format ";; Generated: %s\n\n" (format-time-string "%F %T")))
        (let ((print-level nil)
              (print-length nil))
          (prin1 agent-recall--index (current-buffer)))
        (insert "\n"))
      (rename-file temp file t)))))

(defun agent-recall--index-add (file &optional session-id)
  "Add transcript FILE to the index with optional SESSION-ID.
Derives project name, directory, and timestamp from the file path.
Extracts a preview from the file content.  Saves the index to disk."
  (agent-recall--index-ensure)
  (let* ((dir (file-name-directory file))
         (project (agent-recall--project-name dir))
         (basename (file-name-sans-extension (file-name-nondirectory file)))
         (preview (when (file-exists-p file)
                    (agent-recall--transcript-preview file)))
         (title (agent-recall--read-session-title file)))
    (puthash file
             (list :project project
                   :dir (directory-file-name dir)
                   :timestamp basename
                   :session-id session-id
                   :agent (agent-recall--read-agent-name file)
                   :title title
                   :preview (or preview "(empty)"))
             agent-recall--index)
    (agent-recall--index-save)))

(defun agent-recall--index-ensure ()
  "Ensure the index is loaded into memory.
Loads from disk if not yet loaded this session.  If no index file
exists, sets an empty hash-table and notifies the user."
  (unless agent-recall--index-loaded-p
    (agent-recall--index-load)
    (when (zerop (hash-table-count agent-recall--index))
      (unless (file-exists-p agent-recall-index-file)
        (message "No transcript index found.  Run M-x agent-recall-reindex to build one.")))))

(defun agent-recall--index-dirs ()
  "Return a deduplicated list of transcript directories from the index."
  (agent-recall--index-ensure)
  (let ((dirs (make-hash-table :test 'equal)))
    (maphash (lambda (_file entry)
               (puthash (plist-get entry :dir) t dirs))
             agent-recall--index)
    (hash-table-keys dirs)))

(defun agent-recall--index-files ()
  "Return all indexed transcript file paths, skipping non-existent files."
  (agent-recall--index-ensure)
  (let ((files '()))
    (maphash (lambda (file _entry)
               (when (file-exists-p file)
                 (push file files)))
             agent-recall--index)
    (nreverse files)))

;;;; Session Metadata (sidecar store)

(defvar agent-recall-capture-functions
  (list #'agent-recall--capture-preferences)
  "Abnormal hook of functions returning session metadata to persist.
Each function is called with no arguments in a live agent-shell
buffer (on every `turn-complete' event and when the buffer is
killed) and should return an alist of (KEY . VALUE) pairs to merge
into the session's metadata.  A nil VALUE removes KEY.  Values must
be printable/readable Lisp data.

Add your own function to persist custom per-session data:

  (add-hook \\='agent-recall-capture-functions
            (lambda () \\=`((label . ,(my-get-label)))))")

(defvar agent-recall-restore-functions nil
  "Abnormal hook run after a session is resumed with saved metadata.
Each function is called with two arguments: METADATA (the session's
saved alist) and SHELL-BUFFER (the newly created agent-shell
buffer).  Use this to restore custom data captured via
`agent-recall-capture-functions'.  Model, effort, and permission
mode are restored by agent-recall itself.")

(defvar agent-recall--metadata nil
  "In-memory hash-table of session metadata.
Keys are session ID strings, values are alists of (KEY . VALUE).")

(defvar agent-recall--metadata-loaded-p nil
  "Non-nil if the metadata store has been loaded from disk this session.")

(defun agent-recall--metadata-load ()
  "Read the metadata file from disk into `agent-recall--metadata'.
If the file is missing or corrupt, sets an empty hash-table."
  (let ((file agent-recall-metadata-file))
    (if (file-exists-p file)
        (condition-case err
            (with-temp-buffer
              (insert-file-contents file)
              (let ((data (read (current-buffer))))
                (if (hash-table-p data)
                    (setq agent-recall--metadata data)
                  (setq agent-recall--metadata (make-hash-table :test 'equal))
                  (message "agent-recall: metadata file corrupt, starting fresh"))))
          (error
           (setq agent-recall--metadata (make-hash-table :test 'equal))
           (message "agent-recall: failed to load metadata: %s"
                    (error-message-string err))))
      (setq agent-recall--metadata (make-hash-table :test 'equal)))
    (setq agent-recall--metadata-loaded-p t)))

(defun agent-recall--metadata-save ()
  "Write `agent-recall--metadata' to disk atomically.
Writes to a temporary file then renames to `agent-recall-metadata-file'."
  (when agent-recall--metadata
    (let* ((file agent-recall-metadata-file)
           (dir (file-name-directory file)))
      (unless (file-directory-p dir)
        (make-directory dir t))
      (let ((temp (make-temp-file (expand-file-name ".metadata-" dir))))
        (with-temp-file temp
          (insert ";; agent-recall session metadata -*- no-byte-compile: t -*-\n")
          (insert (format ";; Generated: %s\n\n" (format-time-string "%F %T")))
          (let ((print-level nil)
                (print-length nil))
            (prin1 agent-recall--metadata (current-buffer)))
          (insert "\n"))
        (rename-file temp file t)))))

(defun agent-recall--metadata-ensure ()
  "Ensure the metadata store is loaded into memory."
  (unless agent-recall--metadata-loaded-p
    (agent-recall--metadata-load)))

(defun agent-recall-metadata (session-id)
  "Return the full metadata alist for SESSION-ID, or nil."
  (when session-id
    (agent-recall--metadata-ensure)
    (gethash session-id agent-recall--metadata)))

(defun agent-recall-metadata-get (session-id key)
  "Return the metadata value for SESSION-ID under KEY, or nil."
  (alist-get key (agent-recall-metadata session-id)))

(defun agent-recall-metadata-merge (session-id alist)
  "Merge ALIST into SESSION-ID's stored metadata.
Each (KEY . VALUE) pair upserts KEY; a nil VALUE removes KEY.
Skips the disk write entirely when nothing changed."
  (when (and session-id alist)
    (agent-recall--metadata-ensure)
    (let* ((old (gethash session-id agent-recall--metadata))
           (new (copy-alist old)))
      (dolist (pair alist)
        (if (cdr pair)
            (setf (alist-get (car pair) new) (cdr pair))
          (setf (alist-get (car pair) new nil t) nil)))
      (unless (equal old new)
        (if new
            (puthash session-id new agent-recall--metadata)
          (remhash session-id agent-recall--metadata))
        (agent-recall--metadata-save))
      new)))

(defun agent-recall-metadata-put (session-id key value)
  "Store VALUE under KEY in SESSION-ID's metadata.
A nil VALUE removes KEY.  Saves to disk when changed."
  (agent-recall-metadata-merge session-id (list (cons key value))))

(defun agent-recall-session-label (session-id)
  "Return the user-assigned display label for SESSION-ID, or nil.
Labels live under the `label' metadata key, typically written by an
`agent-recall-capture-functions' hook.  Returns nil when SESSION-ID
is nil, has no stored metadata, or the label is an empty string."
  (when session-id
    (let ((label (agent-recall-metadata-get session-id 'label)))
      (and (stringp label) (not (string-empty-p label)) label))))

(defun agent-recall--label-suffix (session-id)
  "Return a propertized \"  LABEL\" display suffix for SESSION-ID.
Returns an empty string when the session has no label, so callers
can concat it unconditionally onto candidate strings."
  (if-let ((label (agent-recall-session-label session-id)))
      (concat "  " (propertize label 'face 'agent-recall-label))
    ""))

(defun agent-recall--provider-for-name (name)
  "Return a provider symbol (`anthropic', `openai', `gemini') for NAME.
NAME is an agent name or model id string.  Returns nil when it matches
no known provider."
  (when (and name (stringp name))
    (let ((n (downcase name)))
      (cond
       ((string-match-p "claude\\|anthropic" n) 'anthropic)
       ((string-match-p "codex\\|openai\\|chatgpt\\|gpt\\|\\bo[0-9]" n) 'openai)
       ((string-match-p "gemini\\|bard\\|palm\\|google" n) 'gemini)))))

(defun agent-recall--entry-provider (file entry)
  "Return the provider symbol for transcript FILE with index ENTRY, or nil.
Prefers the cached `:agent' from ENTRY, then the transcript's Agent
header, then the saved model metadata."
  (or (agent-recall--provider-for-name (plist-get entry :agent))
      (agent-recall--provider-for-name (agent-recall--read-agent-name file))
      (when-let ((sid (plist-get entry :session-id)))
        (agent-recall--provider-for-name
         (agent-recall-metadata-get sid 'model)))))

(defvar agent-recall--provider-image-cache (make-hash-table :test 'equal)
  "Memoized provider SVG image objects, keyed by (PROVIDER HEIGHT FG).")

(defun agent-recall--provider-image (provider)
  "Return a cached SVG image object for PROVIDER, or nil.
Nil on non-graphic displays or when SVG or the asset file is
unavailable.  Monochrome logos (those using `currentColor') are tinted
to the current default foreground so they track the theme."
  (when (and provider (display-graphic-p) (image-type-available-p 'svg))
    (let* ((height (or agent-recall-provider-icon-height (default-font-height)))
           (fg (or (face-foreground 'default nil t) "#000000"))
           (key (list provider height fg)))
      (or (gethash key agent-recall--provider-image-cache)
          (let ((file (expand-file-name (format "%s.svg" provider)
                                        agent-recall-icon-directory)))
            (when (file-readable-p file)
              (let* ((raw (with-temp-buffer
                            (insert-file-contents file)
                            (buffer-string)))
                     (data (replace-regexp-in-string "currentColor" fg raw t t))
                     (img (create-image data 'svg t
                                        :height height :ascent 'center)))
                (puthash key img agent-recall--provider-image-cache))))))))

(defun agent-recall--provider-icon (file entry)
  "Return a propertized provider-logo prefix for FILE/ENTRY, or \"\".
Empty unless `agent-recall-show-provider-icons' is non-nil.  Shows a
real SVG logo on graphic displays, and a colored initial on text
terminals."
  (if (not agent-recall-show-provider-icons)
      ""
    (if-let ((provider (agent-recall--entry-provider file entry)))
        (let ((help (capitalize (symbol-name provider)))
              (img (agent-recall--provider-image provider)))
          (if img
              (concat (propertize " " 'display img 'help-echo help) " ")
            (let ((face (intern (format "agent-recall-provider-%s"
                                        (symbol-name provider)))))
              (concat (propertize (upcase (substring (symbol-name provider) 0 1))
                                  'face face 'help-echo help)
                      " "))))
      "")))

(defun agent-recall--display-timestamp (ts)
  "Format index timestamp TS as a compact date and time.
For example, \"2026-08-21-19-33-56\" becomes \"Aug 21 19:33:56\".
Timestamps from another year include that year.  Return TS unchanged
when it cannot be parsed."
  (let ((parts (split-string (or ts "") "[-T]")))
    (if (< (length parts) 3)
        ts
      (let ((year (string-to-number (nth 0 parts)))
            (month (string-to-number (nth 1 parts)))
            (day (string-to-number (nth 2 parts)))
            (hour (string-to-number (or (nth 3 parts) "0")))
            (minute (string-to-number (or (nth 4 parts) "0")))
            (second (string-to-number (or (nth 5 parts) "0"))))
        (if (and (<= 1 month 12) (<= 1 day 31) (> year 0))
            (concat
             (format-time-string
              (if (>= (length parts) 6)
                  "%b %e %H:%M:%S"
                (if (>= (length parts) 5) "%b %e %H:%M" "%b %e"))
              (encode-time second minute hour day month year))
             (unless (= year (string-to-number (format-time-string "%Y")))
               (format " %d" year)))
          ts)))))

(defun agent-recall--capture-preferences ()
  "Return the current buffer's agent-shell preferences as metadata.
Captures the model, thought level (effort), and permission mode from
the buffer-local `agent-shell--state'."
  (when (bound-and-true-p agent-shell--state)
    (let ((state agent-shell--state))
      `((model . ,(and (fboundp 'agent-shell--current-model-id)
                       (agent-shell--current-model-id state)))
        (effort . ,(and (fboundp 'agent-shell--current-thought-level-id)
                        (agent-shell--current-thought-level-id state)))
        (permission-mode . ,(and (fboundp 'agent-shell--current-mode-id)
                                 (agent-shell--current-mode-id state)))))))

(defun agent-recall--session-metadata-capture ()
  "Run `agent-recall-capture-functions' and persist the merged result.
Intended to run in an agent-shell buffer.  Does nothing until the
session ID is known."
  (when-let ((session-id
              (or (and (bound-and-true-p agent-shell--state)
                       (map-nested-elt agent-shell--state '(:session :id)))
                  agent-recall--pending-session-id)))
    (let ((merged '()))
      (dolist (fn agent-recall-capture-functions)
        (condition-case err
            (dolist (pair (funcall fn))
              (setf (alist-get (car pair) merged) (cdr pair)))
          (error
           (message "agent-recall: capture function %s failed: %s"
                    fn (error-message-string err)))))
      (when merged
        (agent-recall-metadata-merge session-id merged)))))

(defun agent-recall--project-name (transcript-dir)
  "Extract the project name from TRANSCRIPT-DIR.
Given a path like `/home/user/projects/foo/.agent-shell/transcripts',
returns \"foo\"."
  (let* ((sans-slash (directory-file-name transcript-dir))
         (agent-shell-dir (file-name-directory sans-slash))
         (project-dir (file-name-directory (directory-file-name agent-shell-dir))))
    (file-name-nondirectory (directory-file-name project-dir))))

(defun agent-recall--project-root (transcript-dir)
  "Extract the full project root path from TRANSCRIPT-DIR.
Given `/path/to/project/.agent-shell/transcripts',
returns `/path/to/project'."
  (let* ((sans-slash (directory-file-name transcript-dir))
         (agent-shell-dir (directory-file-name (file-name-directory sans-slash))))
    (directory-file-name (file-name-directory agent-shell-dir))))

(defun agent-recall--org-file-p (file)
  "Return non-nil if FILE is an `org-mode' transcript."
  (and file (string-suffix-p ".org" file)))

(defun agent-recall--org-read-property (file property)
  "Read a #+PROPERTY: PROPERTY value from org transcript FILE header."
  (when (and file (file-exists-p file))
    (with-temp-buffer
      (insert-file-contents file nil 0 1000)
      (goto-char (point-min))
      (when (re-search-forward
             (format "^#\\+PROPERTY:\\s-+%s\\s-+\\(.+\\)$" (regexp-quote property))
             nil t)
        (string-trim (match-string 1))))))

(defun agent-recall--read-session-title (file)
  "Return the session title stored in transcript FILE, or nil."
  (when (and file (file-exists-p file))
    (if (agent-recall--org-file-p file)
        (agent-recall--org-read-property file "Session_Title")
      (with-temp-buffer
        (insert-file-contents file nil 0 3000)
        (when (re-search-forward "^\\*\\*Title:\\*\\*\\s-*\\(.+\\)$" nil t)
          (string-trim (match-string 1)))))))

(defun agent-recall--project-name-from-file (file)
  "Derive a project name from transcript FILE metadata.
For org files, reads the Working_Directory property.
For markdown files, falls back to `agent-recall--project-name'."
  (if (agent-recall--org-file-p file)
      (let ((working-dir (agent-recall--org-read-property file "Working_Directory")))
        (if (and working-dir (not (string-empty-p working-dir)))
            (file-name-nondirectory (directory-file-name working-dir))
          (file-name-nondirectory
           (directory-file-name (file-name-directory file)))))
    (agent-recall--project-name (file-name-directory file))))

(defun agent-recall--transcript-dir-from-file (file)
  "Return the transcript directory containing FILE."
  (file-name-directory file))

(defun agent-recall--project-root-for-session (file)
  "Return project root for session ID resolution from FILE.
Uses conventional `.agent-shell/transcripts/' layout when applicable;
otherwise falls back to the Working Directory header property."
  (let* ((transcript-dir (agent-recall--transcript-dir-from-file file))
         (layout-root (agent-recall--project-root transcript-dir)))
    (if (and layout-root
             (string-match-p
              (concat "/" (regexp-quote agent-recall-transcript-dir-name) "/")
              transcript-dir))
        layout-root
      (or (agent-recall--read-working-directory file) layout-root))))

;;;###autoload
(defun agent-recall-invalidate-cache ()
  "Clear in-memory caches, forcing a reload from the index file.
Does not delete the persistent index; the next command will
re-read it from disk."
  (interactive)
  (setq agent-recall--index-loaded-p nil
        agent-recall--index nil)
  (clrhash agent-recall--session-id-cache)
  (message "agent-recall: caches cleared (index will reload from disk)"))

;;;###autoload
(defun agent-recall-reindex ()
  "Rebuild the transcript index by scanning `agent-recall-search-paths'.
Also indexes `agent-recall-extra-transcript-dirs' directly.
Run once after installing agent-recall, or to pick up transcripts
created outside of agent-shell sessions tracked by the hook."
  (interactive)
  (unless (or agent-recall-search-paths agent-recall-extra-transcript-dirs)
    (user-error "`agent-recall-search-paths' is not set.  Configure it first, e.g.:
  (setq agent-recall-search-paths '(\"~/projects\" \"~/work\"))"))
  (let ((dirs '())
        (new-index (make-hash-table :test 'equal))
        (file-count 0)
        (project-count 0))
    ;; Discover transcript directories via recursive scan
    (dolist (root agent-recall-search-paths)
      (when (file-directory-p root)
        (let* ((cmd (format "find %s -maxdepth %d -path %s -type d 2>/dev/null"
                            (shell-quote-argument (expand-file-name root))
                            agent-recall-max-depth
                            (shell-quote-argument
                             (concat "*/" agent-recall-transcript-dir-name))))
               (output (shell-command-to-string cmd))
               (found (split-string output "\n" t)))
          (setq dirs (append dirs found)))))
    (setq dirs (delete-dups dirs))
    (setq project-count (length dirs))
    ;; Index transcript files from discovered directories
    (dolist (dir dirs)
      (let ((project (agent-recall--project-name dir))
            (files (agent-recall--list-transcript-files dir)))
        (dolist (file files)
          (let* ((basename (file-name-sans-extension (file-name-nondirectory file)))
                 (preview (agent-recall--transcript-preview file))
                 (session-id (agent-recall--resolve-session-id file))
                 (title (agent-recall--read-session-title file)))
            (puthash file
                     (list :project project
                           :dir (directory-file-name dir)
                           :timestamp basename
                           :session-id session-id
                           :agent (agent-recall--read-agent-name file)
                           :title title
                           :preview (or preview "(empty)"))
                     new-index)
            (cl-incf file-count)))))
    ;; Index extra transcript directories (e.g. org-mode transcripts)
    (dolist (entry agent-recall-extra-transcript-dirs)
      (let* ((dir (expand-file-name (plist-get entry :dir)))
             (fixed-project (plist-get entry :project)))
        (when (file-directory-p dir)
          (cl-incf project-count)
          (let ((files (agent-recall--list-transcript-files dir)))
            (dolist (file files)
              (let* ((project (or fixed-project
                                  (agent-recall--project-name-from-file file)))
                     (basename (file-name-sans-extension
                                (file-name-nondirectory file)))
                     (preview (agent-recall--transcript-preview file))
                     (session-id (agent-recall--resolve-session-id file))
                     (title (agent-recall--read-session-title file)))
                (puthash file
                         (list :project project
                               :dir (directory-file-name dir)
                               :timestamp basename
                               :session-id session-id
                               :agent (agent-recall--read-agent-name file)
                               :title title
                               :preview (or preview "(empty)"))
                         new-index)
                (cl-incf file-count)))))))
    (setq agent-recall--index new-index
          agent-recall--index-loaded-p t)
    (agent-recall--index-save)
    (let ((without-session 0))
      (maphash (lambda (_file props)
                 (unless (plist-get props :session-id)
                   (cl-incf without-session)))
               new-index)
      (message "agent-recall: indexed %d transcripts across %d projects%s"
               file-count project-count
               (if (> without-session 0)
                   (format " (%d without session IDs -- run M-x agent-recall-backfill to enable resume)"
                           without-session)
                 "")))))

(defun agent-recall--list-transcript-files (dir)
  "List all transcript files in DIR matching `agent-recall-file-patterns'.
Summary sidecar files (TIMESTAMP.summary.md) are excluded; they are
searched separately via `agent-recall-search-summaries'."
  (let ((files '()))
    (dolist (pattern (agent-recall--file-patterns))
      (let ((regex (wildcard-to-regexp pattern)))
        (dolist (file (directory-files dir t regex t))
          (unless (string-match-p "\\.summary\\.[^.]+\\'" file)
            (push file files)))))
    (delete-dups files)))

(defun agent-recall--file-patterns-as-includes ()
  "Return `agent-recall-file-patterns' as grep --include arguments."
  (concat (mapconcat (lambda (pat)
                       (format "--include=%s" (shell-quote-argument pat)))
                     (agent-recall--file-patterns) " ")
          " --exclude=*.summary.*"))

(defun agent-recall--file-patterns-as-globs ()
  "Return `agent-recall-file-patterns' as ripgrep --glob arguments.
The result is appended to `consult-ripgrep-args' (a whitespace-separated
argument string parsed by `consult--build-args'), so patterns must not
be shell-quoted — `shell-quote-argument' would leave a literal
backslash in the argv (e.g. \"\\\\*.md\"), and ripgrep would match nothing."
  (concat (mapconcat (lambda (pat)
                       (format "--glob %s" pat))
                     (agent-recall--file-patterns) " ")
          " --glob !*.summary.*"))

;;;; Search

(defun agent-recall--ensure-symlink-dir ()
  "Create a directory with symlinks to all transcript dirs.
Returns the path.  Each symlink is named PROJECT-COUNT to avoid
collisions when multiple projects share a name.
The directory lives alongside `agent-recall-index-file'."
  (let* ((base (expand-file-name "search" (file-name-directory agent-recall-index-file)))
         (dirs (agent-recall--index-dirs)))
    (when (file-exists-p base)
      (delete-directory base t))
    (make-directory base t)
    (let ((seen (make-hash-table :test 'equal)))
      (dolist (dir dirs)
        (let* ((project (condition-case nil
                            (agent-recall--project-name dir)
                          (error (file-name-nondirectory
                                  (directory-file-name dir)))))
               (count (gethash project seen 0))
               (link-name (if (= count 0) project
                            (format "%s-%d" project count))))
          (puthash project (1+ count) seen)
          (condition-case nil
              (make-symbolic-link dir (expand-file-name link-name base) t)
            (error nil)))))
    (setq agent-recall--symlink-dir base)
    base))

(defun agent-recall--install-transcript-hook ()
  "Add transcript-mode hook to `find-file-hook' if not already present."
  (unless (memq #'agent-recall--maybe-enable-from-search find-file-hook)
    (add-hook 'find-file-hook #'agent-recall--maybe-enable-from-search)))

(defun agent-recall--clear-pending-search-origin (&optional token)
  "Clear the pending result origin when it still matches TOKEN."
  (when (or (not token)
            (eq token (plist-get agent-recall--pending-search-origin :token)))
    (remove-hook 'post-command-hook
                 #'agent-recall--finish-pending-search-origin)
    (when (timerp agent-recall--pending-search-origin-timer)
      (cancel-timer agent-recall--pending-search-origin-timer))
    (setq agent-recall--pending-search-origin nil
          agent-recall--pending-search-origin-timer nil)))

(defun agent-recall--prepare-search-origin ()
  "Record the current persistent result buffer before running a command."
  (let ((token (cons nil nil)))
    (agent-recall--clear-pending-search-origin)
    (setq agent-recall--pending-search-origin
          (list :token token
                :kind agent-recall--search-origin-kind
                :buffer (current-buffer)
                :marker (copy-marker (point))
                :window (selected-window)))
    (add-hook 'post-command-hook
              #'agent-recall--finish-pending-search-origin)
    (setq agent-recall--pending-search-origin-timer
          (run-at-time 0.1 nil
                       #'agent-recall--clear-pending-search-origin token))))

(defun agent-recall--mark-search-buffer (kind)
  "Mark the current result buffer as persistent search KIND."
  (setq-local agent-recall--search-buffer-p t)
  (setq-local agent-recall--search-origin-kind kind)
  (add-hook 'pre-command-hook #'agent-recall--prepare-search-origin nil t))

(defun agent-recall--maybe-enable-from-search ()
  "Enable transcript-mode if file is a transcript opened from agent-recall.
Only persistent grep and deadgrep result buffers establish this origin."
  (when (and agent-recall--pending-search-origin
             agent-recall-auto-transcript-mode
             (agent-recall--transcript-file-p (buffer-file-name)))
    (unless (bound-and-true-p agent-recall-transcript-mode)
      (agent-recall-transcript-mode 1))))

(defun agent-recall--attach-pending-search-origin (origin)
  "Attach persistent search ORIGIN to the current transcript buffer."
  (let ((buffer (plist-get origin :buffer))
        (marker (plist-get origin :marker)))
    (when (and agent-recall-auto-transcript-mode
               (agent-recall--transcript-file-p (buffer-file-name))
               (buffer-live-p buffer)
               (markerp marker)
               (eq (marker-buffer marker) buffer)
               (buffer-local-value 'agent-recall--search-buffer-p buffer))
      (unless (bound-and-true-p agent-recall-transcript-mode)
        (agent-recall-transcript-mode 1))
      (let ((session
             (agent-recall--navigation-new-session
              (plist-get origin :kind) 'persistent
              :origin-window (plist-get origin :window)
              :origin-buffer buffer
              :origin-marker marker)))
        (setf (agent-recall--navigation-session-selected-file session)
              (agent-recall--canonical-file (buffer-file-name))
              (agent-recall--navigation-session-selected-line session)
              (line-number-at-pos)
              (agent-recall--navigation-session-state session) 'transcript)
        (agent-recall--navigation-attach session (current-buffer))
        t))))

(defun agent-recall--finish-pending-search-origin ()
  "Finish a persistent result command and consume its recorded origin."
  (when-let ((origin agent-recall--pending-search-origin))
    (unwind-protect
        (agent-recall--attach-pending-search-origin origin)
      (agent-recall--clear-pending-search-origin
       (plist-get origin :token)))))

(defun agent-recall--search-via-grep (query dirs)
  "Search DIRS for QUERY using grep with results in `grep-mode'.
Falls back to standard grep, available on all systems."
  (let* ((dir-args (mapconcat #'shell-quote-argument dirs " "))
         (cmd (format "grep -rnH -C %d %s -- %s %s"
                      agent-recall-search-context-lines
                      (agent-recall--file-patterns-as-includes)
                      (shell-quote-argument query)
                      dir-args)))
    (grep cmd)
    (when agent-recall-auto-transcript-mode
      (agent-recall--install-transcript-hook)
      (when-let ((buf (get-buffer "*grep*")))
        (with-current-buffer buf
          (agent-recall--mark-search-buffer 'grep))))))

(defun agent-recall--search-via-deadgrep (query _dirs)
  "Search transcripts for QUERY using `deadgrep'.
DIRS are unused; deadgrep searches the symlink directory instead."
  (unless (fboundp 'deadgrep)
    (user-error "Deadgrep is not installed.  Install it or set `agent-recall-search-function' to `grep'"))
  (let ((dir (agent-recall--ensure-symlink-dir))
        (deadgrep-extra-arguments (append (bound-and-true-p deadgrep-extra-arguments) '("--follow"))))
    (deadgrep query dir)
    (when agent-recall-auto-transcript-mode
      (agent-recall--install-transcript-hook)
      (agent-recall--mark-search-buffer 'deadgrep))))

(defun agent-recall--search-via-counsel-rg (query _dirs)
  "Search transcripts for QUERY using `counsel-rg'.
DIRS are unused; counsel-rg searches the symlink directory instead."
  (unless (fboundp 'counsel-rg)
    (user-error "Counsel is not installed.  Install it or set `agent-recall-search-function' to `grep'"))
  (let* ((dir (agent-recall--ensure-symlink-dir))
         (counsel-rg-base-command
          (append (list "rg" "--max-columns" "240" "--with-filename"
                        "--no-heading" "--line-number" "--color" "never"
                        "--follow")
                  (cl-mapcan (lambda (pat) (list "--glob" pat))
                             (agent-recall--file-patterns))
                  (list "%s"))))
    (counsel-rg query dir "" "Recall: ")
    (when (and agent-recall-auto-transcript-mode
               (agent-recall--transcript-file-p (buffer-file-name)))
      (agent-recall-transcript-mode 1))))

(defun agent-recall--search-via-consult-ripgrep (query _dirs)
  "Search transcripts for QUERY using `consult-ripgrep'.
DIRS are unused; consult-ripgrep searches the symlink directory instead."
  (unless (require 'consult nil t)
    (user-error "Consult is not installed.  Install it or set `agent-recall-search-function' to `grep'"))
  (let* ((dir (agent-recall--ensure-symlink-dir))
         (consult-ripgrep-args
          (concat consult-ripgrep-args
                  " --follow "
                  (agent-recall--file-patterns-as-globs))))
    (consult-ripgrep dir query)
    (when (and agent-recall-auto-transcript-mode
               (agent-recall--transcript-file-p (buffer-file-name)))
      (agent-recall-transcript-mode 1))))

;;;###autoload
(defun agent-recall-search-string (query &optional max-results)
  "Search transcripts for QUERY, return results as a string.
Intended for programmatic use, e.g. from `emacsclient --eval'.
Uses ripgrep to search all indexed transcript directories.
Returns up to MAX-RESULTS (default 20) matching lines with context.
Returns an empty string when no matches are found."
  (agent-recall--index-ensure)
  (let* ((dirs (agent-recall--index-dirs))
         (dir-args (mapconcat #'shell-quote-argument dirs " "))
         ;; Shell-quoted variant of `agent-recall--file-patterns-as-globs'
         ;; (that one feeds consult argv and must NOT be quoted).
         (glob-args (concat (mapconcat (lambda (pat)
                                         (format "--glob %s" (shell-quote-argument pat)))
                                       (agent-recall--file-patterns) " ")
                            " --glob " (shell-quote-argument "!*.summary.*")))
         (cmd (format "%s --follow %s --sort=modified -C %d -m %d -i -- %s %s"
                      agent-recall-rg-executable
                      glob-args
                      agent-recall-search-context-lines
                      (or max-results 20)
                      (shell-quote-argument query)
                      dir-args)))
    (if dirs
        (string-trim (shell-command-to-string cmd))
      "")))

;;;###autoload
(defun agent-recall-search-summaries-string (query &optional max-results)
  "Search transcript summaries for QUERY, return results as a string.
Like `agent-recall-search-string' but searches only summary files
\(*.summary.md), which are produced by `agent-recall-summarize'.
Shows at most MAX-RESULTS matches (default 20).  Returns an empty
string when no matches or summaries are found."
  (agent-recall--index-ensure)
  (let* ((dirs (agent-recall--index-dirs))
         (dir-args (mapconcat #'shell-quote-argument dirs " "))
         (cmd (format "%s --follow --glob '%s' --sort=modified -C %d -m %d -i -- %s %s"
                      agent-recall-rg-executable
                      "*.summary.md"
                      agent-recall-search-context-lines
                      (or max-results 20)
                      (shell-quote-argument query)
                      dir-args)))
    (if dirs
        (string-trim (shell-command-to-string cmd))
      "")))

;;;###autoload
(defun agent-recall-list-projects-string ()
  "Return a summary of indexed projects and transcript counts.
Intended for programmatic use, e.g. from `emacsclient --eval'.
Returns a human-readable string with project names and file counts."
  (agent-recall--index-ensure)
  (let ((project-data (make-hash-table :test 'equal))
        (lines '()))
    (maphash (lambda (_file entry)
               (let* ((project (plist-get entry :project))
                      (cur (gethash project project-data 0)))
                 (puthash project (1+ cur) project-data)))
             agent-recall--index)
    (maphash (lambda (project count)
               (push (format "  %-30s %4d transcripts" project count) lines))
             project-data)
    (setq lines (sort lines #'string<))
    (if lines
        (mapconcat #'identity
                   (cons (format "Indexed projects: %d\n" (hash-table-count project-data))
                         lines)
                   "\n")
      "No transcripts indexed.  Run M-x agent-recall-reindex.")))

;;;###autoload
(defun agent-recall-search (query)
  "Search all agent-shell transcripts for QUERY.
The search backend is controlled by `agent-recall-search-function'."
  (interactive "sSearch transcripts: ")
  (let ((dirs (agent-recall--index-dirs)))
    (unless dirs
      (user-error "No transcripts indexed.  Run M-x agent-recall-reindex"))
    (pcase agent-recall-search-function
      ('deadgrep         (agent-recall--search-via-deadgrep query dirs))
      ('counsel-rg       (agent-recall--search-via-counsel-rg query dirs))
      ('consult-ripgrep  (agent-recall--search-via-consult-ripgrep query dirs))
      (_                 (agent-recall--search-via-grep query dirs)))))

;;;###autoload
(defun agent-recall-search-live ()
  "Search transcripts with live-updating results.
Uses `agent-recall-search-function' if it supports live search,
otherwise falls back to the best available live backend."
  (interactive)
  (let ((dirs (agent-recall--index-dirs)))
    (unless dirs
      (user-error "No transcripts indexed.  Run M-x agent-recall-reindex"))
    (pcase agent-recall-search-function
      ('counsel-rg       (agent-recall--search-via-counsel-rg "" dirs))
      ('consult-ripgrep  (agent-recall--search-via-consult-ripgrep "" dirs))
      ;; deadgrep and grep don't do live filtering -- pick best available
      (_
       (cond
        ((fboundp 'counsel-rg)      (agent-recall--search-via-counsel-rg "" dirs))
        ((fboundp 'consult-ripgrep) (agent-recall--search-via-consult-ripgrep "" dirs))
        (t                          (call-interactively #'agent-recall-search)))))))

;;;; Browse

(defun agent-recall--transcript-path-less-p (a b)
  "Return non-nil when transcript record A has a path before B."
  (string< (agent-recall--canonical-file (nth 1 a))
           (agent-recall--canonical-file (nth 1 b))))

(defun agent-recall--transcript-primary-less-p (a b primary direction)
  "Compare transcript records A and B by PRIMARY in DIRECTION.
Use the canonical absolute path as a deterministic tie-breaker."
  (let ((av (funcall primary a))
        (bv (funcall primary b)))
    (if (equal av bv)
        (agent-recall--transcript-path-less-p a b)
      (funcall direction av bv))))

(defun agent-recall--transcript-mtime (record)
  "Return the modification time for transcript RECORD."
  (or (when-let ((attributes (file-attributes (nth 1 record))))
        (file-attribute-modification-time attributes))
      (seconds-to-time 0)))

(defun agent-recall--sort-transcript-records (records)
  "Sort transcript RECORDS according to `agent-recall-browse-sort'."
  (sort
   records
   (lambda (a b)
     (pcase agent-recall-browse-sort
       ('date-desc
        (agent-recall--transcript-primary-less-p a b #'caddr #'string>))
       ('date-asc
        (agent-recall--transcript-primary-less-p a b #'caddr #'string<))
       ('modified-desc
        (agent-recall--transcript-primary-less-p
         a b #'agent-recall--transcript-mtime
         (lambda (left right) (time-less-p right left))))
       ('modified-asc
        (agent-recall--transcript-primary-less-p
         a b #'agent-recall--transcript-mtime #'time-less-p))
       ('project
        (agent-recall--transcript-primary-less-p
         a b (lambda (record) (or (nth 3 record) "")) #'string<))
       (_ (agent-recall--transcript-path-less-p a b))))))

(defun agent-recall--list-transcripts ()
  "Return an alist of (DISPLAY-NAME . FILE-PATH) for all transcripts.
Each entry also carries its timestamp for sorting."
  (agent-recall--index-ensure)
  (let ((transcripts '()))
    (maphash (lambda (file entry)
               (when (file-exists-p file)
                 (let* ((file (agent-recall--canonical-file file))
                        (project (plist-get entry :project))
                        (ts (plist-get entry :timestamp))
                        (title (plist-get entry :title))
                        (display (concat (agent-recall--provider-icon file entry)
                                         (format "[%s] " project)
                                         (if (and title (not (string-empty-p title)))
                                             (concat title "  ")
                                           "")
                                         (propertize (agent-recall--display-timestamp ts)
                                                     'face 'shadow)
                                         (agent-recall--label-suffix
                                          (plist-get entry :session-id)))))
                   (push (list display file ts project) transcripts))))
             agent-recall--index)
    (setq transcripts (agent-recall--sort-transcript-records transcripts))
    (mapcar (lambda (entry) (cons (nth 0 entry) (nth 1 entry))) transcripts)))

(defun agent-recall--transcript-preview (file)
  "Extract a one-line preview from transcript FILE.
Returns the first user message, truncated.  Supports both markdown
and `org-mode' transcript formats."
  (with-temp-buffer
    (insert-file-contents file nil 0 3000)
    (goto-char (point-min))
    (let ((regex (if (agent-recall--org-file-p file)
                     "^\\*\\* User.*\n+"
                   "^## User.*\n+\\(?:> \\)?\\(.+\\)")))
      (if (re-search-forward regex nil t)
          (let ((text (if (agent-recall--org-file-p file)
                          ;; For org: grab everything up to the next heading
                          (let* ((start (point))
                                 (end (if (re-search-forward "^\\*\\* " nil t)
                                          (match-beginning 0)
                                        (point-max)))
                                 (raw (string-trim
                                       (buffer-substring-no-properties start end))))
                            (when (string-prefix-p "#+begin_quote" raw)
                              (setq raw (replace-regexp-in-string
                                         "\\`#\\+begin_quote\n?" "" raw))
                              (setq raw (replace-regexp-in-string
                                         "\n?#\\+end_quote\\'" "" raw)))
                            (string-trim raw))
                        ;; For markdown: already captured by regex group
                        (string-trim (match-string 1)))))
            (if (> (length text) 0)
                (truncate-string-to-width
                 (car (split-string text "\n" t)) 80)
              "(empty)"))
        "(empty)"))))

(defun agent-recall--open-transcript (file &optional other-window line force-mode)
  "Open transcript FILE and optionally move to LINE.
When OTHER-WINDOW is non-nil, open in another window.  Enable
`agent-recall-transcript-mode' when configured, or unconditionally when
FORCE-MODE is non-nil."
  (if other-window
      (find-file-other-window file)
    (find-file file))
  (goto-char (point-min))
  (when line
    (forward-line (1- (max 1 line))))
  (when (or force-mode agent-recall-auto-transcript-mode)
    (agent-recall-transcript-mode 1)))

(defun agent-recall--browse-preview-state (file-lookup)
  "Return a consult state function for live preview of transcripts.
FILE-LOOKUP maps completion identities to files."
  (require 'agent-recall-consult)
  (agent-recall-consult--browse-preview-state file-lookup))

(defun agent-recall--browse-consult (candidates annotate-fn)
  "Browse CANDIDATES using Consult with ANNOTATE-FN.
CANDIDATES is a list of propertized display strings.
Returns the selected candidate string, or nil."
  (require 'agent-recall-consult)
  (agent-recall-consult--browse-read candidates annotate-fn))

(defvar agent-recall--ivy-temporary-buffers nil
  "Buffers opened during `agent-recall-browse' ivy preview.")

(defvar agent-recall--active-browse-candidates nil
  "Candidate snapshot used by the active Browse picker.")

(defun agent-recall--ivy-browse-update-fn ()
  "Preview the current ivy candidate transcript in the window."
  (let* ((current (or (agent-recall--candidate-lookup
                       (ivy-state-current ivy-last)
                       agent-recall--active-browse-candidates)
                      (ivy-state-current ivy-last)))
         (file (agent-recall--candidate-file current)))
    (when file
      (let ((buf (get-file-buffer file)))
        (unless buf
          (setq buf (find-file-noselect file))
          (push buf agent-recall--ivy-temporary-buffers))
        (with-selected-window (ivy--get-window ivy-last)
          (switch-to-buffer buf 'norecord))))))

(defun agent-recall--ivy-browse-unwind ()
  "Clean up temporary buffers opened during ivy browse preview."
  (mapc #'kill-buffer agent-recall--ivy-temporary-buffers)
  (setq agent-recall--ivy-temporary-buffers nil))

(defun agent-recall--browse-ivy (candidates _annotate-fn)
  "Browse transcripts using ivy with live preview.
CANDIDATES is a list of propertized display strings.
Returns the selected candidate string, or nil."
  (let ((ivy-update-fns-alist
         (cons '(agent-recall-browse . agent-recall--ivy-browse-update-fn)
               ivy-update-fns-alist))
        (ivy-unwind-fns-alist
         (cons '(agent-recall-browse . agent-recall--ivy-browse-unwind)
               ivy-unwind-fns-alist)))
    (unwind-protect
        (let ((selected
               (ivy-read "Transcript: " candidates
                         :caller 'agent-recall-browse
                         :require-match t
                         :preselect (car agent-recall--browse-history)
                         :history 'agent-recall--browse-history
                         :action (lambda (x) x))))
          (or (agent-recall--candidate-lookup selected candidates) selected))
      (agent-recall--ivy-browse-unwind))))

(defun agent-recall--browse-default (candidates annotate-fn)
  "Browse transcripts with plain `completing-read'.
CANDIDATES is a list of propertized display strings.
ANNOTATE-FN is the annotation function.
Returns the selected candidate string, or nil."
  (let ((selected
         (completing-read
          "Transcript: "
          (lambda (string pred action)
            (if (eq action 'metadata)
                `(metadata
                  (category . agent-recall-transcript)
                  (display-sort-function . identity)
                  (cycle-sort-function . identity)
                  (annotation-function . ,annotate-fn))
              (complete-with-action action candidates string pred)))
          nil t nil 'agent-recall--browse-history
          (car agent-recall--browse-history))))
    (or (agent-recall--candidate-lookup selected candidates) selected)))

(defun agent-recall--browse-candidates (transcripts)
  "Build unique payload-bearing candidates from TRANSCRIPTS."
  (agent-recall--disambiguate-candidates
   (mapcar (lambda (entry)
             (agent-recall--make-candidate
              (car entry) (cdr entry) nil 'browse))
           transcripts)))

(defun agent-recall--index-entry-for-file (file)
  "Return the index entry matching canonical FILE."
  (or (gethash file agent-recall--index)
      (let (found)
        (maphash
         (lambda (indexed entry)
           (when (and (not found)
                      (equal file (agent-recall--canonical-file indexed)))
             (setq found entry)))
         agent-recall--index)
        found)))

(defun agent-recall--browse-annotation-function (candidates)
  "Return a preview annotation function for CANDIDATES."
  (lambda (candidate)
    (when-let* ((original (or (agent-recall--candidate-lookup
                               candidate candidates)
                              candidate))
                (file (agent-recall--candidate-file original))
                (entry (agent-recall--index-entry-for-file file))
                (preview (plist-get entry :preview))
                ((not (string-empty-p preview))))
      (concat "  " preview))))

(defun agent-recall--consult-picker-available-p ()
  "Return non-nil when Browse should use the Consult adapter."
  (and (require 'consult nil t)
       (require 'agent-recall-consult nil t)
       (or agent-recall-browse-preview
           (agent-recall-consult--suspend-available-p))))

(defun agent-recall--read-browse-candidate (candidates)
  "Read one item from the Browse CANDIDATES snapshot."
  (let ((agent-recall--active-browse-candidates candidates)
        (annotate-fn (agent-recall--browse-annotation-function candidates)))
    (cond
     ((and agent-recall-browse-preview
           (bound-and-true-p ivy-mode)
           (require 'ivy nil t))
      (agent-recall--browse-ivy candidates annotate-fn))
     ((agent-recall--consult-picker-available-p)
      (agent-recall--browse-consult candidates annotate-fn))
     (t
      (agent-recall--browse-default candidates annotate-fn)))))

;;;###autoload
(defun agent-recall-browse ()
  "Browse and open agent-shell transcripts.
Presents a searchable list of all transcripts grouped by project.
When `agent-recall-browse-preview' is non-nil, provides live preview
using consult or ivy if available.  Falls back to plain `completing-read'."
  (interactive)
  (agent-recall--setup-embark)
  (let* ((transcripts (agent-recall--list-transcripts)))
    (unless transcripts
      (user-error "No transcripts indexed.  Run M-x agent-recall-reindex"))
    (let* ((candidates (agent-recall--browse-candidates transcripts))
           (selection (agent-recall--read-browse-candidate candidates))
           (file (agent-recall--candidate-file selection)))
      (when file
        (agent-recall--open-transcript file)))))

(defun agent-recall--current-project-name ()
  "Derive the current project name from `default-directory'.
Uses `locate-dominating-file' to find a `.agent-shell/' directory,
matching the path-based convention used by agent-shell.  If no
`.agent-shell/' ancestor is found, falls back to the basename of
`default-directory'."
  (let* ((root (locate-dominating-file default-directory ".agent-shell"))
         (dir (or root default-directory)))
    (file-name-nondirectory (directory-file-name dir))))

(defun agent-recall--list-transcripts-for-project (project)
  "Return an alist of (DISPLAY-NAME . FILE-PATH) for transcripts matching PROJECT.
Like `agent-recall--list-transcripts' but filtered to entries whose
`:project' equals PROJECT (case-insensitive)."
  (agent-recall--index-ensure)
  (let ((transcripts '())
        (project-down (downcase project)))
    (maphash (lambda (file entry)
               (when (and (file-exists-p file)
                          (string= (downcase (or (plist-get entry :project) ""))
                                   project-down))
                 (let* ((file (agent-recall--canonical-file file))
                        (ts (plist-get entry :timestamp))
                        (display (concat (agent-recall--provider-icon file entry)
                                         (format "[%s] " (plist-get entry :project))
                                         (propertize (agent-recall--display-timestamp ts)
                                                     'face 'shadow)
                                         (agent-recall--label-suffix
                                          (plist-get entry :session-id)))))
                   (push (list display file ts (plist-get entry :project))
                         transcripts))))
             agent-recall--index)
    (setq transcripts (agent-recall--sort-transcript-records transcripts))
    (mapcar (lambda (entry) (cons (nth 0 entry) (nth 1 entry))) transcripts)))

;;;###autoload
(defun agent-recall-browse-project ()
  "Browse transcripts for the current project only.
Determines the project name from `default-directory' and shows only
matching transcripts.  Uses the same preview and completion backend
as `agent-recall-browse'."
  (interactive)
  (agent-recall--setup-embark)
  (let* ((project (agent-recall--current-project-name))
         (transcripts (agent-recall--list-transcripts-for-project project)))
    (unless transcripts
      (user-error "No transcripts found for project \"%s\"" project))
    (let* ((candidates (agent-recall--browse-candidates transcripts))
           (selection (agent-recall--read-browse-candidate candidates))
           (file (agent-recall--candidate-file selection)))
      (when file
        (agent-recall--open-transcript file)))))

(defun agent-recall-clean-view ()
  "Open a clean view of the current transcript.
Creates a new buffer with only User and Agent messages, stripping
tool calls, agent thoughts, and other noise.  The result is a
plain markdown buffer you can render with your preferred method."
  (interactive)
  (let ((source-file (buffer-file-name))
        (source-buffer (current-buffer)))
    (unless source-file
      (user-error "Buffer is not visiting a file"))
    (let* ((base (file-name-sans-extension
                  (file-name-nondirectory source-file)))
           (temp (expand-file-name (concat base "-clean.md")
                                   temporary-file-directory))
           (buf (find-file-noselect temp)))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (with-current-buffer source-buffer
            (save-excursion
              (goto-char (point-min))
              ;; Copy the header (everything before first ## heading)
              (let ((header-end (or (re-search-forward "^## " nil t)
                                    (point-max))))
                (with-current-buffer buf
                  (insert-buffer-substring source-buffer 1 header-end)))
              (goto-char (point-min))
              ;; Extract User and Agent sections, stopping at tool calls
              (while (re-search-forward "^## \\(User\\|Agent\\) " nil t)
                (let* ((section-start (match-beginning 0))
                       (section-end
                        (save-excursion
                          (goto-char (match-end 0))
                          ;; Stop at next ## heading or ### Tool Call, whichever comes first
                          (if (re-search-forward "^\\(## \\|### Tool Call\\)" nil t)
                              (match-beginning 0)
                            (point-max))))
                       (text (buffer-substring-no-properties
                              section-start section-end)))
                  (with-current-buffer buf
                    (insert text))
                  (goto-char section-end)))))
          (goto-char (point-min))
          (save-buffer)
          (when (fboundp 'markdown-mode)
            (markdown-mode))
          (set-buffer-modified-p nil)))
      ;; Same-window by default; users can reroute via `display-buffer-alist'.
      (pop-to-buffer buf '(display-buffer-same-window)))))

(defun agent-recall-next-user-message ()
  "Jump to the next user message in the transcript."
  (interactive)
  (let ((pos (save-excursion
               (end-of-line)
               (re-search-forward "^## User" nil t))))
    (if pos
        (goto-char (match-beginning 0))
      (message "No more user messages"))))

(defun agent-recall-prev-user-message ()
  "Jump to the previous user message in the transcript."
  (interactive)
  (let ((pos (save-excursion
               (beginning-of-line)
               (re-search-backward "^## User" nil t))))
    (if pos
        (goto-char pos)
      (message "No earlier user messages"))))

(defun agent-recall-browse-from-transcript ()
  "Return to this transcript's exact origin, or reopen Browse."
  (interactive)
  (let ((session (car agent-recall--navigation-origins)))
    (cond
     ((and session (agent-recall--navigation-session-valid-p session))
      (pcase (agent-recall--navigation-session-backend session)
        ('suspended
         (agent-recall--navigation-detach session)
         (funcall (agent-recall--navigation-session-resume-function session)
                  session))
        ('persistent
         (agent-recall--navigation-restore-persistent session))))
     (t
      (when session
        (agent-recall--navigation-detach session)
        (if (eq (agent-recall--navigation-session-backend session) 'suspended)
            (agent-recall--navigation-schedule-orphan-abort)
          (agent-recall--navigation-cleanup session)))
      (quit-window)
      (agent-recall-browse)))))

(defun agent-recall-quit-transcript ()
  "Close the transcript and cleanly abort any suspended picker."
  (interactive)
  (let ((session (car agent-recall--navigation-origins)))
    (cond
     ((and session
           (eq (agent-recall--navigation-session-backend session) 'suspended)
           (agent-recall--navigation-session-valid-p session))
     (agent-recall--navigation-request-abort session))
     (t
      (when session
        (if (eq (agent-recall--navigation-session-backend session) 'suspended)
            (progn
              (agent-recall--navigation-detach session)
              (agent-recall--navigation-schedule-orphan-abort))
          (agent-recall--navigation-cleanup session)))
      (quit-window)))))

(defvar agent-recall-transcript-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") #'agent-recall-resume-current)
    (define-key map (kbd "R") #'agent-recall-force-resume-current)
    (define-key map (kbd "c") #'agent-recall-clean-view)
    (define-key map (kbd "b") #'agent-recall-browse-from-transcript)
    (define-key map (kbd "q") #'agent-recall-quit-transcript)
    (define-key map (kbd "C-c C-n") #'agent-recall-next-user-message)
    (define-key map (kbd "C-c C-p") #'agent-recall-prev-user-message)
    map)
  "Keymap for `agent-recall-transcript-mode'.")

;; Evil users get normal-state keys on the minor-mode map itself.  This
;; must not use `evil-local-set-key' inside the mode body: when the mode
;; is enabled by `global-agent-recall-transcript-mode' it runs from
;; `after-change-major-mode-hook' ahead of evil's own enable-in-buffer
;; hook, so evil's buffer-local maps do not exist yet and the error
;; aborts the hook chain (leaving the buffer without evil, in
;; `fundamental-mode').  Binding on the mode map is order-independent.
(with-eval-after-load 'evil
  (evil-define-key* 'normal agent-recall-transcript-mode-map
    (kbd "r") #'agent-recall-resume-current
    (kbd "R") #'agent-recall-force-resume-current
    (kbd "c") #'agent-recall-clean-view
    (kbd "C-j") #'agent-recall-next-user-message
    (kbd "C-k") #'agent-recall-prev-user-message
    (kbd "]]") #'agent-recall-next-user-message
    (kbd "[[") #'agent-recall-prev-user-message
    (kbd "gj") #'agent-recall-next-user-message
    (kbd "gk") #'agent-recall-prev-user-message
    (kbd "b") #'agent-recall-browse-from-transcript
    (kbd "q") #'agent-recall-quit-transcript))

(defun agent-recall--header-entry (key label)
  "Format a header line entry with KEY highlighted and LABEL dimmed."
  (concat (propertize key 'face 'agent-recall-header-key)
          " "
          (propertize label 'face 'agent-recall-header-label)))

(defun agent-recall--header-line (&optional session-id)
  "Build the header line string for transcript mode.
When SESSION-ID is non-nil, include a resume entry."
  (let ((entries (list))
        (existing (and session-id
                       (agent-recall--find-session-buffer session-id))))
    (when session-id
      (push (agent-recall--header-entry
             "r" (if existing
                     (format "Resume (%s)" (buffer-name existing))
                   (format "Resume (%s)" (substring session-id 0 (min 8 (length session-id))))))
            entries)
      (when existing
        (push (agent-recall--header-entry "R" "Force Resume") entries)))
    (push (agent-recall--header-entry "c" "Clean") entries)
    (push (agent-recall--header-entry "b" "Back") entries)
    (push (agent-recall--header-entry "C-j/C-k" "Navigate") entries)
    (push (agent-recall--header-entry "q" "Quit") entries)
    (concat "  " (mapconcat #'identity (nreverse entries) "  "))))

(defun agent-recall--metadata-summary (metadata)
  "Format METADATA alist as a single-line summary for the echo area."
  (mapconcat (lambda (kv)
               (concat (propertize (format "%s: "
                                           (string-replace
                                            "-" " " (symbol-name (car kv))))
                                   'face 'shadow)
                       (format "%s" (cdr kv))))
             metadata "  |  "))

(define-minor-mode agent-recall-transcript-mode
  "Minor mode for viewing agent-recall transcripts.
When the transcript has a resumable session ID, press `r' to resume.
When the session has saved metadata (model, effort, labels, etc.),
a summary is shown in the echo area."
  :lighter " Recall"
  :keymap agent-recall-transcript-mode-map
  (if agent-recall-transcript-mode
      (let ((session-id (agent-recall--resolve-session-id (buffer-file-name))))
        (setq-local agent-recall--transcript-session-id session-id)
        (read-only-mode 1)
        (setq-local header-line-format
                    '(:eval (agent-recall--header-line agent-recall--transcript-session-id)))
        (when-let ((metadata (and session-id
                                  (agent-recall-metadata session-id))))
          (message "metadata detected — %s"
                   (agent-recall--metadata-summary metadata))))
    (when agent-recall--navigation-origins
      (agent-recall--navigation-transcript-killed))
    (read-only-mode -1)
    (kill-local-variable 'agent-recall--transcript-session-id)
    (kill-local-variable 'header-line-format)))

(defun agent-recall--transcript-file-p (file)
  "Return non-nil if FILE is inside an agent-shell transcript directory.
Also matches files opened via the agent-recall search symlink directory,
and files in `agent-recall-extra-transcript-dirs'."
  (and file
       (or (string-match-p (concat "/" (regexp-quote agent-recall-transcript-dir-name) "/") file)
           (and agent-recall--symlink-dir
                (string-prefix-p (expand-file-name agent-recall--symlink-dir)
                                 (expand-file-name file)))
           ;; Compare truenames: with `find-file-visit-truename' the buffer
           ;; file is fully resolved (e.g. /private/var on macOS) while the
           ;; configured dir usually is not, and a plain prefix test misses.
           (cl-some (lambda (entry)
                      (let ((dir (file-name-as-directory
                                  (file-truename
                                   (expand-file-name (plist-get entry :dir))))))
                        (string-prefix-p dir (file-truename
                                              (expand-file-name file)))))
                    agent-recall-extra-transcript-dirs))))

(defun agent-recall--maybe-enable-transcript-mode ()
  "Enable `agent-recall-transcript-mode' if visiting a transcript file."
  (when (agent-recall--transcript-file-p (buffer-file-name))
    (agent-recall-transcript-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-agent-recall-transcript-mode
  agent-recall-transcript-mode agent-recall--maybe-enable-transcript-mode
  :group 'agent-recall)

(defun agent-recall--find-session-buffer (session-id)
  "Return a live agent-shell buffer already running SESSION-ID, or nil."
  (seq-find
   (lambda (buf)
     (and (buffer-live-p buf)
          (with-current-buffer buf
            (and (derived-mode-p 'agent-shell-mode)
                 (boundp 'agent-shell--state)
                 agent-shell--state
                 (or (equal session-id
                            (map-nested-elt agent-shell--state
                                            '(:session :id)))
                     (equal session-id
                            (map-elt agent-shell--state
                                     :resume-session-id)))))))
   (buffer-list)))

(defun agent-recall--display-buffer (buffer)
  "Display agent-shell BUFFER respecting viewport preferences.
Window placement for the non-viewport path is controlled by
`display-buffer-alist'."
  (if (bound-and-true-p agent-shell-prefer-viewport-interaction)
      (agent-shell-viewport--show-buffer :shell-buffer buffer)
    (pop-to-buffer buffer)))

(defun agent-recall-resume-current ()
  "Resume this transcript's session, or switch to its existing buffer."
  (interactive)
  (let ((session-id (buffer-local-value 'agent-recall--transcript-session-id
                                        (current-buffer)))
        (file (buffer-file-name)))
    (unless session-id
      (user-error "This transcript has no resumable session ID"))
    (if-let ((existing (agent-recall--find-session-buffer session-id)))
        (progn
          (message "Switching to existing buffer: %s" (buffer-name existing))
          (agent-recall--display-buffer existing))
      (agent-recall--start-resume session-id file))))

(defun agent-recall-force-resume-current ()
  "Force-resume this transcript's session, ignoring existing buffers."
  (interactive)
  (let ((session-id (buffer-local-value 'agent-recall--transcript-session-id
                                        (current-buffer)))
        (file (buffer-file-name)))
    (unless session-id
      (user-error "This transcript has no resumable session ID"))
    (agent-recall--start-resume session-id file)))

(defun agent-recall--read-working-directory (file)
  "Extract the Working Directory from transcript FILE header.
Supports both markdown and `org-mode' transcript formats."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file nil 0 500)
      (goto-char (point-min))
      (let ((regex (if (agent-recall--org-file-p file)
                       "^#\\+PROPERTY:\\s-+Working_Directory\\s-+\\(.+\\)"
                     "^\\*\\*Working Directory:\\*\\* \\(.+\\)")))
        (when (re-search-forward regex nil t)
          (let ((dir (string-trim (match-string 1))))
            (when (file-directory-p dir)
              dir)))))))

(defun agent-recall--read-agent-name (file)
  "Extract the Agent from transcript FILE header."
  (when (file-exists-p file)
    (if (agent-recall--org-file-p file)
        (agent-recall--org-read-property file "Agent")
      (with-temp-buffer
        (insert-file-contents file nil 0 500)
        (goto-char (point-min))
        (when (re-search-forward "^\\*\\*Agent:\\*\\* \\(.+\\)" nil t)
          (string-trim (match-string 1)))))))

(defun agent-recall--normalize-agent-name (name)
  "Normalize agent NAME for matching transcript headers to configs."
  (when name
    (replace-regexp-in-string
     "[^[:alnum:]]+" ""
     (downcase (string-trim (format "%s" name))))))

(defun agent-recall--agent-config-matches-name-p (config name)
  "Return non-nil if agent CONFIG matches transcript agent NAME."
  (let ((normalized-name (agent-recall--normalize-agent-name name)))
    (and normalized-name
         (seq-some
          (lambda (config-name)
            (equal normalized-name
                   (agent-recall--normalize-agent-name config-name)))
          (list (map-elt config :identifier)
                (map-elt config :mode-line-name)
                (map-elt config :buffer-name))))))

(defun agent-recall--agent-config-for-transcript (file)
  "Return the `agent-shell' config matching transcript FILE's Agent header.
Checks the user's preferred config first, then falls back to the
default `agent-shell-agent-configs' list."
  (when-let ((agent-name (agent-recall--read-agent-name file)))
    (let ((preferred (and (fboundp 'agent-shell--resolve-preferred-config)
                          (agent-shell--resolve-preferred-config))))
      (or (and preferred
               (agent-recall--agent-config-matches-name-p preferred agent-name)
               preferred)
          (seq-find (lambda (config)
                      (agent-recall--agent-config-matches-name-p config agent-name))
                    (if (fboundp 'agent-shell--resolved-agent-configs)
                        (agent-shell--resolved-agent-configs)
                      agent-shell-agent-configs))))))

(defun agent-recall--restore-allowed-p (metadata)
  "Return non-nil if saved METADATA should be restored on resume.
Honors `agent-recall-resume-restore-preferences', prompting when
set to `ask'."
  (and metadata
       (pcase agent-recall-resume-restore-preferences
         ('nil nil)
         ('ask (y-or-n-p
                (format "Restore session preferences (%s)? "
                        (mapconcat (lambda (kv)
                                     (format "%s=%s" (car kv) (cdr kv)))
                                   metadata ", "))))
         (_ t))))

(defun agent-recall--config-with-preferences (config metadata)
  "Return CONFIG with model/mode overrides from METADATA prepended.
The overrides are closures that agent-shell calls during session
init; each validates the saved id against what the live session
actually offers and returns nil (skipping the step) when the id is
no longer available, so a stale id can never stall initialization."
  (when-let ((model (alist-get 'model metadata)))
    (setq config
          (append
           `((:default-model-id
              . ,(lambda ()
                   (if (and (fboundp 'agent-shell--get-available-models)
                            (seq-find (lambda (m)
                                        (equal (map-elt m :model-id) model))
                                      (agent-shell--get-available-models
                                       agent-shell--state)))
                       model
                     (message "agent-recall: saved model %s unavailable; skipping"
                              model)
                     nil))))
           config)))
  (when-let ((mode (alist-get 'permission-mode metadata)))
    (setq config
          (append
           `((:default-session-mode-id
              . ,(lambda ()
                   (if (and (fboundp 'agent-shell--get-available-modes)
                            (seq-find (lambda (m)
                                        (equal (map-elt m :id) mode))
                                      (agent-shell--get-available-modes
                                       agent-shell--state)))
                       mode
                     (message "agent-recall: saved mode %s unavailable; skipping"
                              mode)
                     nil))))
           config)))
  config)

(defun agent-recall--restore-thought-level (shell-buffer effort)
  "Restore EFFORT (thought level) in SHELL-BUFFER once init completes.
Subscribes one-shot to `init-finished' (which re-fires on every
command, hence the immediate unsubscribe) and silently skips when
the agent does not advertise a thought level option or EFFORT is
not among its values."
  (let ((token nil))
    (setq token
          (agent-shell-subscribe-to
           :shell-buffer shell-buffer
           :event 'init-finished
           :on-event
           (lambda (_event)
             (when token
               (agent-shell-unsubscribe :subscription token)
               (setq token nil)
               (when (buffer-live-p shell-buffer)
                 (with-current-buffer shell-buffer
                   (when (and (fboundp 'agent-shell--get-available-thought-levels)
                              (seq-find (lambda (v)
                                          (equal (map-elt v :value) effort))
                                        (agent-shell--get-available-thought-levels
                                         agent-shell--state)))
                     (agent-shell--config-option-set-thought-level-id
                      :thought-level-id effort
                      :on-failure
                      (lambda (&rest _)
                        (message "agent-recall: could not restore effort %s"
                                 effort))))))))))))

(defun agent-recall--start-resume (session-id &optional transcript-file)
  "Resume SESSION-ID using agent-shell, skipping shell picker.
Uses the transcript Agent header to select the original agent when
available, then starts a new shell buffer with the session loaded.
When TRANSCRIPT-FILE is provided, sets working directory from the
transcript.  Saved session preferences (model, effort, permission
mode, and custom `agent-recall-restore-functions' data) are restored
per `agent-recall-resume-restore-preferences'."
  (let* ((transcript-agent (and transcript-file
                                (agent-recall--read-agent-name transcript-file)))
         (default-directory (or (and transcript-file
                                     (agent-recall--read-working-directory transcript-file))
                                default-directory))
         (config (or (and transcript-file
                          (agent-recall--agent-config-for-transcript transcript-file))
                     (and (not transcript-agent)
                          (agent-shell--resolve-preferred-config))
                     (agent-shell-select-config
                      :prompt (if transcript-agent
                                  (format "Resume %s session with agent: "
                                          transcript-agent)
                                "Resume with agent: "))
                     (error "No agent config found")))
         (metadata (agent-recall-metadata session-id))
         (restore (agent-recall--restore-allowed-p metadata))
         (config (if restore
                     (agent-recall--config-with-preferences config metadata)
                   config))
         (shell-buffer (agent-shell--start :config config
                                           :session-id session-id
                                           :session-strategy 'new
                                           :no-focus t
                                           :new-session t)))
    (when (and transcript-file agent-recall-resume-continue-transcript)
      (with-current-buffer shell-buffer
        (setq-local agent-shell--transcript-file transcript-file)))
    (when restore
      (when-let ((effort (alist-get 'effort metadata)))
        (agent-recall--restore-thought-level shell-buffer effort))
      (run-hook-with-args 'agent-recall-restore-functions metadata shell-buffer))
    (agent-recall--display-buffer shell-buffer)))

;;;###autoload
(defun agent-recall-resume ()
  "Resume a past agent-shell session from a transcript.
Only shows transcripts that have resolvable session IDs."
  (interactive)
  (agent-recall--index-ensure)
  (let ((resumable '()))
    (maphash (lambda (file entry)
               (when (file-exists-p file)
                 (let ((session-id (or (plist-get entry :session-id)
                                       (agent-recall--resolve-session-id file))))
                   (when session-id
                     (let* ((project (plist-get entry :project))
                            (ts (plist-get entry :timestamp))
                            (preview (or (plist-get entry :preview) ""))
                            (display (concat (agent-recall--provider-icon file entry)
                                             (format "[%s] " project)
                                             (propertize (agent-recall--display-timestamp ts)
                                                         'face 'shadow)
                                             (agent-recall--label-suffix session-id))))
                       ;; Short dates make display strings collide; the
                       ;; property, not the string, identifies the file.
                       (push (list (propertize display 'agent-recall-file file)
                                   file session-id preview)
                             resumable))))))
             agent-recall--index)
    (unless resumable
      (user-error "No resumable transcripts found.  Try `agent-recall-backfill' first"))
    (let* ((selection (completing-read
                       "Resume session: "
                       (lambda (string pred action)
                         (if (eq action 'metadata)
                             `(metadata
                               (annotation-function
                                . ,(lambda (candidate)
                                     (when-let ((entry (assoc candidate resumable)))
                                       (let ((preview (nth 3 entry)))
                                         (when (and preview (not (string-empty-p preview)))
                                           (concat "  " preview)))))))
                           (complete-with-action
                            action (mapcar #'car resumable) string pred)))
                       nil t))
           (entry (or (when-let ((file (agent-recall--candidate-file selection)))
                        (seq-find (lambda (e) (equal (nth 1 e) file)) resumable))
                      ;; Fallback for completion UIs that strip text
                      ;; properties from the returned string.
                      (assoc selection resumable)))
           (file (nth 1 entry))
           (session-id (nth 2 entry)))
      (when session-id
        (agent-recall--start-resume session-id file)))))

;;;; Stats

;;;###autoload
(defun agent-recall-stats ()
  "Display statistics about your agent-shell transcript collection."
  (interactive)
  (agent-recall--index-ensure)
  (let ((total-files 0)
        (total-size 0)
        (project-data (make-hash-table :test 'equal))
        (project-stats '()))
    ;; Group files by project, compute sizes
    (maphash (lambda (file entry)
               (when (file-exists-p file)
                 (let* ((project (plist-get entry :project))
                        (size (or (file-attribute-size (file-attributes file)) 0))
                        (cur (gethash project project-data (list 0 0))))
                   (puthash project
                            (list (1+ (nth 0 cur)) (+ (nth 1 cur) size))
                            project-data)
                   (cl-incf total-files)
                   (cl-incf total-size size))))
             agent-recall--index)
    (maphash (lambda (project counts)
               (push (list project (nth 0 counts) (nth 1 counts)) project-stats))
             project-data)
    (setq project-stats
          (sort project-stats (lambda (a b) (> (nth 1 a) (nth 1 b)))))
    (with-current-buffer (get-buffer-create "*agent-recall-stats*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "Agent Recall -- Transcript Statistics\n"
                            'face 'info-title-1))
        (insert (make-string 40 ?═) "\n\n")
        (insert (format "  Transcripts: %d\n" total-files))
        (insert (format "  Projects:    %d\n" (hash-table-count project-data)))
        (insert (format "  Total size:  %.1f MB\n\n" (/ total-size 1048576.0)))
        (insert (propertize "By Project:\n" 'face 'bold))
        (insert (make-string 40 ?─) "\n")
        (dolist (stat project-stats)
          (insert (format "  %-30s %4d files  (%.1f MB)\n"
                          (nth 0 stat) (nth 1 stat)
                          (/ (nth 2 stat) 1048576.0)))))
      (goto-char (point-min))
      (special-mode)
      (pop-to-buffer (current-buffer)))))

;;; ====================================================================
;;; Part A: Forward Session ID Embedding
;;; ====================================================================

(defun agent-recall--write-session-title-to-file (filepath title)
  "Store TITLE in the header of transcript at FILEPATH."
  (when (and filepath (file-exists-p filepath)
             (stringp title) (not (string-empty-p title)))
    (let* ((title (string-trim (car (split-string title "\n"))))
           (orgp (agent-recall--org-file-p filepath))
           (line (if orgp
                     (format "#+PROPERTY: Session_Title %s" title)
                   (format "**Title:** %s" title)))
           (regexp (if orgp
                       "^#\\+PROPERTY:\\s-+Session_Title\\s-+.*$"
                     "^\\*\\*Title:\\*\\*.*$")))
      (with-temp-buffer
        (insert-file-contents filepath)
        (goto-char (point-min))
        (if (re-search-forward regexp nil t)
            (unless (equal (match-string 0) line)
              (replace-match line t t)
              (write-region (point-min) (point-max) filepath nil 'no-message))
          (goto-char (point-min))
          (if orgp
              (when (re-search-forward "^#\\+TITLE:.*$" nil t)
                (end-of-line))
            (when (re-search-forward "^---$" nil t)
              (goto-char (match-beginning 0))))
          (insert (if orgp (concat "\n" line) (concat line "\n\n")))
          (write-region (point-min) (point-max) filepath nil 'no-message))))))

(defun agent-recall--sync-session-metadata (shell-buffer)
  "Write SHELL-BUFFER's session metadata and refresh its index entry."
  (when (buffer-live-p shell-buffer)
    (with-current-buffer shell-buffer
      (when (and agent-shell--transcript-file
                 (file-exists-p agent-shell--transcript-file))
        (let ((session-id (map-nested-elt agent-shell--state '(:session :id)))
              (title (map-nested-elt agent-shell--state '(:session :title))))
          (when session-id
            (agent-recall--write-session-id-to-file
             agent-shell--transcript-file session-id))
          (agent-recall--write-session-title-to-file
           agent-shell--transcript-file title)
          (agent-recall--index-add agent-shell--transcript-file session-id))))))

(defun agent-recall--write-session-id-to-file (filepath session-id)
  "Insert SESSION-ID into the header of transcript at FILEPATH.
For markdown files, inserts `**Session:** UUID' before the `---' separator.
For org files, inserts `#+PROPERTY: Session UUID' after existing properties.

Skips writing when a session ID is already present, including agent-shell's
native `**Session ID:**' / `#+PROPERTY: Session_ID' headers."
  (when (and filepath (file-exists-p filepath) session-id)
    (with-temp-buffer
      (insert-file-contents filepath)
      (goto-char (point-min))
      (if (agent-recall--org-file-p filepath)
          (unless (re-search-forward
                   "^#\\+PROPERTY:\\s-+Session\\(?:_ID\\)?\\s-" nil t)
            (goto-char (point-min))
            (let ((insert-pos nil))
              (if (re-search-forward "^#\\+PROPERTY:" nil t)
                  (progn
                    (setq insert-pos (line-end-position))
                    (while (re-search-forward "^#\\+PROPERTY:" nil t)
                      (setq insert-pos (line-end-position))))
                (goto-char (point-min))
                (if (re-search-forward "^#\\+TITLE:" nil t)
                    (setq insert-pos (line-end-position))
                  (setq insert-pos (point-min))))
              (goto-char insert-pos)
              (unless (bolp)
                (end-of-line))
              (insert (format "\n#+PROPERTY: Session %s" session-id))
              (write-region (point-min) (point-max) filepath nil 'no-message)))
        ;; Match both agent-recall's `**Session:**' and agent-shell's
        ;; `**Session ID:**' so we do not duplicate headers.
        (unless (re-search-forward "^\\*\\*Session\\(?: ID\\)?:\\*\\*" nil t)
          (goto-char (point-min))
          (when (re-search-forward "^---$" nil t)
            (goto-char (match-beginning 0))
            (insert (format "**Session:** %s\n\n" session-id))
            (write-region (point-min) (point-max) filepath nil 'no-message)))))))

;;;###autoload
(defun agent-recall-track-sessions ()
  "Hook function for `agent-shell-mode-hook' to embed session IDs.
Subscribes to agent-shell events to capture the session ID and write
it into the transcript file header.  This enables instant session
resume from `agent-recall-browse' and `agent-recall-resume'.

Add to your config:
  (add-hook \\='agent-shell-mode-hook #\\='agent-recall-track-sessions)"

  (let ((shell-buffer (current-buffer)))
    ;; Persistent metadata capture: snapshot preferences (and any custom
    ;; `agent-recall-capture-functions' data) after every turn and on
    ;; buffer kill, so the stored values always reflect the last state.
    (agent-shell-subscribe-to
     :shell-buffer shell-buffer
     :event 'turn-complete
     :on-event
     (lambda (_event)
       (when (buffer-live-p shell-buffer)
         (with-current-buffer shell-buffer
           (agent-recall--session-metadata-capture)))))
    (add-hook 'kill-buffer-hook #'agent-recall--session-metadata-capture nil t)
    (agent-shell-subscribe-to
     :shell-buffer shell-buffer
     :event 'session-title-changed
     :on-event (lambda (_event)
                 (agent-recall--sync-session-metadata shell-buffer)))
    ;; Subscribe to init-session to capture the session ID
    (agent-shell-subscribe-to
     :shell-buffer shell-buffer
     :event 'init-session
     :on-event
     (lambda (_event)
       (when-let ((session-id
                   (and (buffer-live-p shell-buffer)
                        (buffer-local-value 'agent-shell--state shell-buffer)
                        (map-nested-elt
                         (buffer-local-value 'agent-shell--state shell-buffer)
                         '(:session :id)))))
         (with-current-buffer shell-buffer
           (setq-local agent-recall--pending-session-id session-id)
           ;; Subscribe to turn-complete to write after first prompt
           ;; (transcript file is guaranteed to exist by then)
           (let ((write-token nil))
             (setq write-token
                   (agent-shell-subscribe-to
                    :shell-buffer shell-buffer
                    :event 'turn-complete
                    :on-event
                    (lambda (_event)
                      (with-current-buffer shell-buffer
                        (when (and agent-recall--pending-session-id
                                   (not agent-recall--session-id-written-p)
                                   agent-shell--transcript-file
                                   (file-exists-p agent-shell--transcript-file))
                          (agent-recall--write-session-id-to-file
                           agent-shell--transcript-file
                           agent-recall--pending-session-id)
                          (agent-recall--write-session-title-to-file
                           agent-shell--transcript-file
                           (map-nested-elt agent-shell--state
                                           '(:session :title)))
                          (agent-recall--index-add
                           agent-shell--transcript-file
                           agent-recall--pending-session-id)
                          (setq-local agent-recall--session-id-written-p t)
                          (setq-local agent-recall--pending-session-id nil))
                        ;; Unsubscribe after first successful write
                        (when (and write-token agent-recall--session-id-written-p)
                          (agent-shell-unsubscribe
                           :subscription write-token)))))))))))))

;;; ====================================================================
;;; Part B: Retroactive Session Matching
;;; ====================================================================

(defun agent-recall--read-embedded-session-id (file)
  "Read the session ID from transcript FILE header, if present.
Supports markdown `**Session:** UUID' (agent-recall) and
`**Session ID:** UUID' (agent-shell native header), plus org
`#+PROPERTY: Session UUID' / `#+PROPERTY: Session_ID UUID'."
  (when (file-exists-p file)
    (let ((uuid-re "[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}"))
      (with-temp-buffer
        (insert-file-contents file nil 0 1500)
        (goto-char (point-min))
        (when (re-search-forward
               (if (agent-recall--org-file-p file)
                   (format "^#\\+PROPERTY:\\s-+Session\\(?:_ID\\)?\\s-+\\(%s\\)"
                           uuid-re)
                 (format "^\\*\\*Session\\(?: ID\\)?:\\*\\*\\s-+\\(%s\\)"
                         uuid-re))
               nil t)
          (match-string 1))))))

(defun agent-recall--parse-transcript-timestamp (file)
  "Extract the `Started' timestamp from transcript FILE header.
Returns an Emacs time value (as from `encode-time'), or nil.
Supports markdown (`**Started:**') and org (`#+DATE:') formats."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file nil 0 500)
      (goto-char (point-min))
      (let ((regex (if (agent-recall--org-file-p file)
                       "^#\\+DATE:\\s-+\\(.+\\)$"
                     "^\\*\\*Started:\\*\\*\\s-+\\(.+\\)$")))
        (when (re-search-forward regex nil t)
          (let ((decoded (parse-time-string (match-string 1))))
            (when (nth 5 decoded)
              (encode-time (decoded-time-set-defaults decoded)))))))))

(defun agent-recall--parse-iso8601-timestamp (iso-string)
  "Parse ISO-STRING into an Emacs time value, or nil if unparseable."
  (when iso-string
    (condition-case nil
        (encode-time (iso8601-parse iso-string))
      (error nil))))

(defun agent-recall--claude-project-dir (project-path)
  "Return the Claude sessions directory for PROJECT-PATH.
Claude CLI stores sessions in ~/.claude/projects/ with directory names
derived from the project path (slashes become dashes, leading slash dropped).
Returns nil if the directory doesn't exist."
  (when project-path
    (let* ((expanded (directory-file-name (expand-file-name project-path)))
           ;; Claude's naming: replace / . _ and space with -, keep leading dash
           (mangled (replace-regexp-in-string "[/. _]" "-" expanded))
           (dir (expand-file-name
                 (concat "projects/" mangled)
                 agent-recall-claude-config-dir)))
      (when (file-directory-p dir)
        dir))))

(defun agent-recall--load-sessions-index (claude-dir)
  "Load session entries from `sessions-index.json' in CLAUDE-DIR.
Returns an alist of (SESSION-ID . CREATED-TIME) where CREATED-TIME
is an Emacs time value."
  (let ((index-file (expand-file-name "sessions-index.json" claude-dir)))
    (when (file-exists-p index-file)
      (condition-case nil
          (let* ((json-object-type 'alist)
                 (json-array-type 'list)
                 (json-key-type 'symbol)
                 (data (json-read-file index-file))
                 (entries (alist-get 'entries data))
                 (result '()))
            (dolist (entry entries)
              (let* ((session-id (alist-get 'sessionId entry))
                     (created (alist-get 'created entry))
                     (time (agent-recall--parse-iso8601-timestamp created)))
                (when (and session-id time)
                  (push (cons session-id time) result))))
            result)
        (error nil)))))

(defun agent-recall--scan-jsonl-timestamps (claude-dir)
  "Scan JSONL files in CLAUDE-DIR for session timestamps.
Emits one entry per minute of activity per session.  Resumed sessions
append to the same JSONL across multiple days; emitting per-minute
keeps each resumption epoch as a candidate the matcher can pick from.
Returns an alist of (SESSION-ID . CREATED-TIME)."
  (let ((result '())
        (seen (make-hash-table :test 'equal))
        (parse-fn (if (fboundp 'json-parse-string)
                      (lambda (s)
                        (json-parse-string s :object-type 'alist))
                    (lambda (s)
                      (let ((json-object-type 'alist)
                            (json-key-type 'symbol))
                        (json-read-from-string s))))))
    (dolist (file (directory-files claude-dir t "\\.jsonl\\'"))
      (let ((session-id (file-name-sans-extension (file-name-nondirectory file))))
        (condition-case nil
            (with-temp-buffer
              (insert-file-contents file)
              (goto-char (point-min))
              (while (not (eobp))
                (let ((line (buffer-substring-no-properties
                             (line-beginning-position) (line-end-position))))
                  (when (> (length line) 0)
                    (condition-case nil
                        (let* ((data (funcall parse-fn line))
                               (type (alist-get 'type data)))
                          (when (equal type "user")
                            (let* ((ts (alist-get 'timestamp data))
                                   (minute-key (when (and ts (>= (length ts) 16))
                                                 (substring ts 0 16)))
                                   (dedup-key (and minute-key
                                                   (concat session-id "|" minute-key))))
                              (when (and dedup-key (not (gethash dedup-key seen)))
                                (puthash dedup-key t seen)
                                (let ((time (agent-recall--parse-iso8601-timestamp ts)))
                                  (when time
                                    (push (cons session-id time) result)))))))
                      (error nil))))
                (forward-line 1)))
          (error nil))))
    result))

(defun agent-recall--transcript-first-message (file)
  "Extract the full first user message from transcript FILE.
Returns the message text, or nil.  Supports markdown and org formats."
  (when (file-exists-p file)
    (let ((org-p (agent-recall--org-file-p file)))
      (with-temp-buffer
        (insert-file-contents file nil 0 5000)
        (goto-char (point-min))
        (let ((heading-re (if org-p "^\\*\\* User.*\n+" "^## User.*\n+"))
              (next-re (if org-p "^\\*\\* " "^## ")))
          (when (re-search-forward heading-re nil t)
            (let* ((start (point))
                   (end (if (re-search-forward next-re nil t)
                            (match-beginning 0)
                          (point-max)))
                   (text (string-trim (buffer-substring-no-properties start end))))
              (cond
               ((string-prefix-p "> " text)
                (setq text (substring text 2)))
               ((and org-p (string-prefix-p "#+begin_quote" text))
                (setq text (replace-regexp-in-string
                            "\\`#\\+begin_quote\n?" "" text))
                (setq text (replace-regexp-in-string
                            "\n?#\\+end_quote\\'" "" text))))
              (when (> (length text) 0)
                text))))))))

(defun agent-recall--jsonl-first-message (file)
  "Extract the first real user message from JSONL session FILE.
Skips system/command messages that start with `<'."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let ((result nil))
        (while (and (not result) (not (eobp)))
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
            (when (> (length line) 0)
              (condition-case nil
                  (let* ((json-object-type 'alist)
                         (json-array-type 'list)
                         (json-key-type 'symbol)
                         (data (json-read-from-string line))
                         (type (alist-get 'type data)))
                    (when (equal type "user")
                      (let* ((msg (alist-get 'message data))
                             (content (alist-get 'content msg))
                             (text (cond
                                    ((stringp content) content)
                                    ((listp content)
                                     (cl-loop for c in content
                                              when (equal (alist-get 'type c) "text")
                                              return (alist-get 'text c))))))
                        (when (and text
                                   (not (string-prefix-p "<" (string-trim text))))
                          (setq result (string-trim text))))))
                (error nil)))
            (forward-line 1)))
        result))))

(defun agent-recall--normalize-message (text)
  "Normalize TEXT for comparison: trim, downcase, collapse whitespace."
  (when text
    (downcase
     (replace-regexp-in-string "\\s-+" " " (string-trim text)))))

(defun agent-recall--match-session (transcript-time transcript-file sessions claude-dir)
  "Find the session matching TRANSCRIPT-FILE at TRANSCRIPT-TIME from SESSIONS.
SESSIONS is an alist of (SESSION-ID . CREATED-TIME).
CLAUDE-DIR is the Claude project directory containing JSONL files.
Uses hybrid matching: timestamp narrows candidates, message content confirms.
Returns session ID string, or nil."
  (let* ((candidates
          (when transcript-time
            (let ((result '()))
              (dolist (entry sessions)
                (let* ((session-id (car entry))
                       (session-time (cdr entry)))
                  (when session-time
                    (let ((delta (abs (float-time (time-subtract session-time transcript-time)))))
                      (when (<= delta agent-recall-session-match-window)
                        (push (cons session-id delta) result))))))
              ;; Sort by closest delta
              (sort result (lambda (a b) (< (cdr a) (cdr b)))))))
         (transcript-msg (when candidates
                           (agent-recall--normalize-message
                            (agent-recall--transcript-first-message transcript-file)))))
    (cond
     ;; Has candidates and a message -- try to confirm with content
     ((and candidates transcript-msg)
      (let ((confirmed nil))
        (dolist (cand candidates)
          (unless confirmed
            (let* ((id (car cand))
                   (jsonl-file (expand-file-name (concat id ".jsonl") claude-dir))
                   (jsonl-msg (agent-recall--normalize-message
                               (agent-recall--jsonl-first-message jsonl-file))))
              (when (and jsonl-msg (equal transcript-msg jsonl-msg))
                (setq confirmed id)))))
        ;; If no message match, fall back to closest timestamp
        (or confirmed (car (car candidates)))))
     ;; Has candidates but no message -- closest timestamp
     (candidates
      (car (car candidates)))
     ;; No candidates
     (t nil))))

(defun agent-recall--resolve-session-id (file)
  "Resolve the session ID for transcript FILE.
Checks in order:
  1. In-memory cache
  2. Embedded header (`**Session:**' or agent-shell `**Session ID:**')
  3. Retroactive timestamp matching against Claude session data
Returns session ID string, or nil if unresolvable."
  ;; Check cache
  (let ((cached (gethash file agent-recall--session-id-cache)))
    (cond
     ;; Cache hit with a real session ID
     ((and cached (not (eq cached 'none)))
      cached)
     ;; Cache hit with 'none -- we already tried and failed
     ((eq cached 'none)
      nil)
     ;; Cache miss -- resolve
     (t
      (let ((session-id
             (or
              ;; 1. Check embedded header
              (agent-recall--read-embedded-session-id file)
              ;; 2. Try hybrid matching (timestamp + message content)
              (let* ((project-root (agent-recall--project-root-for-session file))
                     (claude-dir (agent-recall--claude-project-dir project-root))
                     (transcript-time (agent-recall--parse-transcript-timestamp file)))
                (when claude-dir
                  (let* ((index-sessions (agent-recall--load-sessions-index claude-dir))
                         (jsonl-sessions (agent-recall--scan-jsonl-timestamps claude-dir))
                         (all-sessions (cl-remove-if-not
                                        (lambda (s) (and (car s) (cdr s)))
                                        (delete-dups
                                         (append index-sessions jsonl-sessions)))))
                    (agent-recall--match-session
                     transcript-time file all-sessions claude-dir)))))))
        ;; Cache the result
        (puthash file (or session-id 'none) agent-recall--session-id-cache)
        session-id)))))

;;; ====================================================================
;;; Part D: Backfill
;;; ====================================================================

;;;###autoload
(defun agent-recall-backfill (&optional write-mode)
  "Match old transcripts to session IDs and optionally write them.

By default runs in dry-run mode, showing matches in a preview buffer.
With \\[universal-argument] prefix (WRITE-MODE non-nil), actually writes
session IDs into transcript file headers.

Results are displayed in the `*agent-recall-backfill*' buffer."
  (interactive "P")
  (agent-recall--index-ensure)
  (let* ((actually-write write-mode)
         (matched 0)
         (skipped 0)
         (no-match 0)
         (total 0)
         (modified-files '())
         (sessions-cache (make-hash-table :test 'equal)))
    (with-current-buffer (get-buffer-create "*agent-recall-backfill*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize
                 (if actually-write
                     "Agent Recall -- Backfill (WRITING)\n"
                   "Agent Recall -- Backfill (DRY RUN)\n")
                 'face 'info-title-1))
        (insert (make-string 50 ?═) "\n\n")
        (maphash
         (lambda (file entry)
           (when (file-exists-p file)
             (let ((project (plist-get entry :project)))
               (cl-incf total)
               (let ((existing (agent-recall--read-embedded-session-id file)))
                 (cond
                  ;; Already has session ID
                  (existing
                   (cl-incf skipped)
                   (insert (format "  SKIP:     [%s] %s (has %s)\n"
                                   project
                                   (file-name-nondirectory file)
                                   (substring existing 0 8))))
                  ;; Try to match using hybrid approach
                  (t
                   (let* ((project-root (agent-recall--project-root-for-session file))
                          (claude-dir (agent-recall--claude-project-dir project-root))
                          (transcript-time (agent-recall--parse-transcript-timestamp file))
                          (session-id nil))
                     (when claude-dir
                       (let ((all-sessions
                              (or (gethash claude-dir sessions-cache)
                                  (puthash
                                   claude-dir
                                   (cl-remove-if-not
                                    (lambda (s) (and (car s) (cdr s)))
                                    (delete-dups
                                     (append
                                      (agent-recall--load-sessions-index claude-dir)
                                      (agent-recall--scan-jsonl-timestamps claude-dir))))
                                   sessions-cache))))
                         (setq session-id
                               (agent-recall--match-session
                                transcript-time file all-sessions claude-dir))))
                     (if session-id
                         (progn
                           (cl-incf matched)
                           (insert (format "  MATCH:    [%s] %s → %s\n"
                                           project
                                           (file-name-nondirectory file)
                                           (substring session-id 0 8)))
                           (when actually-write
                             (agent-recall--write-session-id-to-file file session-id)
                             (push file modified-files)))
                       (cl-incf no-match)
                       (insert (format "  NO MATCH: [%s] %s\n"
                                       project
                                       (file-name-nondirectory file)))))))))))
         agent-recall--index)
        ;; Summary
        (insert "\n" (make-string 50 ?─) "\n")
        (insert (propertize "Summary:\n" 'face 'bold))
        (insert (format "  Total:      %d\n" total))
        (insert (format "  Matched:    %d\n" matched))
        (insert (format "  Skipped:    %d (already have session ID)\n" skipped))
        (insert (format "  No match:   %d\n" no-match))
        (when (and actually-write modified-files)
          (insert (format "\n  Wrote session IDs to %d files.\n" (length modified-files)))
          ;; Write undo log
          (let ((log-file (expand-file-name "backfill-log.el"
                                            (file-name-directory agent-recall-index-file))))
            (with-temp-file log-file
              (insert ";; agent-recall backfill undo log\n")
              (insert (format ";; Written: %s\n" (format-time-string "%F %T")))
              (insert (format ";; Files modified: %d\n\n" (length modified-files)))
              (insert ";; To undo, evaluate this buffer (removes **Session:** lines):\n")
              (insert "(dolist (file '(\n")
              (dolist (f modified-files)
                (insert (format "  %S\n" f)))
              (insert "))\n")
              (insert "  (when (file-exists-p file)\n")
              (insert "    (with-temp-buffer\n")
              (insert "      (insert-file-contents file)\n")
              (insert "      (goto-char (point-min))\n")
              (insert "      (when (re-search-forward \"^\\\\*\\\\*Session:\\\\*\\\\*.*\\n\\n?\" nil t)\n")
              (insert "        (replace-match \"\")\n")
              (insert "        (write-region (point-min) (point-max) file nil 'no-message)))))\n"))
            (insert (format "  Undo log: %s\n" log-file))))
        (unless actually-write
          (insert "\n  To write, run: C-u C-u M-x agent-recall-backfill\n")))
      (goto-char (point-min))
      (special-mode)
      (pop-to-buffer (current-buffer)))))

;;;; Embark Integration

(defun agent-recall-embark-open-other-window (candidate)
  "Open transcript CANDIDATE in another window."
  (when-let* ((file (agent-recall--candidate-file candidate)))
    (agent-recall--open-transcript file t)))

(defun agent-recall-embark-resume (candidate)
  "Resume transcript CANDIDATE's session, or switch to existing buffer."
  (when-let* ((file (agent-recall--candidate-file candidate)))
    (let ((session-id (agent-recall--resolve-session-id file)))
      (unless session-id
        (user-error "This transcript has no resumable session ID"))
      (if-let ((existing (agent-recall--find-session-buffer session-id)))
          (progn
            (message "Switching to existing buffer: %s" (buffer-name existing))
            (agent-recall--display-buffer existing))
        (agent-recall--start-resume session-id file)))))

(defun agent-recall-embark-force-resume (candidate)
  "Force-resume transcript CANDIDATE's session, ignoring existing buffers."
  (when-let* ((file (agent-recall--candidate-file candidate)))
    (let ((session-id (agent-recall--resolve-session-id file)))
      (if session-id
          (agent-recall--start-resume session-id file)
        (user-error "This transcript has no resumable session ID")))))

(defvar agent-recall-transcript-embark-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "o") #'agent-recall-embark-open-other-window)
    (define-key map (kbd "r") #'agent-recall-embark-resume)
    (define-key map (kbd "R") #'agent-recall-embark-force-resume)
    map)
  "Embark actions for agent-recall transcript candidates.")

(defun agent-recall--setup-embark ()
  "Register embark actions for agent-recall transcript candidates."
  (when (bound-and-true-p embark-keymap-alist)
    (add-to-list 'embark-keymap-alist
                 '(agent-recall-transcript . agent-recall-transcript-embark-map))))

(agent-recall--setup-embark)

;;; ====================================================================
;;; Transcript Summarization
;;; ====================================================================

(defun agent-recall--summary-file (transcript-file)
  "Return the summary file path for TRANSCRIPT-FILE.
Given `TIMESTAMP.md', returns `TIMESTAMP.summary.md' in the same directory."
  (let ((base (file-name-sans-extension transcript-file)))
    (concat base ".summary.md")))

(defun agent-recall--needs-summary-p (file)
  "Return non-nil if transcript FILE has no summary yet."
  (not (file-exists-p (agent-recall--summary-file file))))

(defun agent-recall--clean-transcript-string (file)
  "Return transcript FILE content without tool-call sections.
Extracts only the header plus User and Agent sections, removing
tool calls and agent thought blocks."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((source (current-buffer))
          (result (generate-new-buffer " *recall-clean*")))
      (unwind-protect
          (progn
            (with-current-buffer source
              (save-excursion
                (goto-char (point-min))
                ;; Copy the header (everything before first ## heading)
                (let ((header-end (or (re-search-forward "^## " nil t)
                                      (point-max))))
                  (with-current-buffer result
                    (insert-buffer-substring source 1 header-end)))
                (goto-char (point-min))
                ;; Extract User and Agent sections, stopping at tool calls
                (while (re-search-forward "^## \\(User\\|Agent\\) " nil t)
                  (let* ((section-start (match-beginning 0))
                         (section-end
                          (save-excursion
                            (goto-char (match-end 0))
                            (if (re-search-forward
                                 "^\\(## \\|### Tool Call\\)" nil t)
                                (match-beginning 0)
                              (point-max))))
                         (text (buffer-substring-no-properties
                                section-start section-end)))
                    (with-current-buffer result
                      (insert text))
                    (goto-char section-end)))))
            (with-current-buffer result
              (buffer-string)))
        (kill-buffer result)))))

(defvar agent-recall--summarize-prompt
  "Summarize the following agent-shell conversation transcript.
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

"
  "Prompt template for transcript summarization.")

(defvar-local agent-recall--summarize-client nil
  "ACP client for the current summarization session.")

(defvar-local agent-recall--summarize-session-id nil
  "ACP session ID for the current summarization session.")

(defvar-local agent-recall--summarize-response-text nil
  "Accumulated response text from agent_message_chunk notifications.")

(defvar-local agent-recall--summarize-progress-marker nil
  "Marker in the progress buffer for updating the current item's status.")

(defvar-local agent-recall--summarize-progress-buffer nil
  "Reference to the progress buffer, for timer and notification callbacks.")

(defvar-local agent-recall--summarize-spinner-index 0
  "Current spinner frame index.")

(defvar-local agent-recall--summarize-spinner-timer nil
  "Timer for the spinner animation.")

(defvar-local agent-recall--summarize-timeout-timer nil
  "One-shot timer that fires when the current item exceeds the timeout.")

(defvar-local agent-recall--summarize-start-time nil
  "Time when the current item started processing, for countdown display.")

(defvar-local agent-recall--summarize-generation 0
  "Counter incremented each time a new item starts processing.
Used to detect and discard stale callbacks from timed-out items.")

(defconst agent-recall--spinner-frames
  ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"]
  "Braille spinner frames for progress indication.")

(defun agent-recall--summarize-handle-agent-error (work-buffer progress-buffer err)
  "Record agent process error ERR in PROGRESS-BUFFER without tearing down.

acp.el reports every non-empty stderr line from the agent process as an
error (for example the startup banner printed by wrappers such as
acp-multiplex), so these are notices, not failures.  A dead process
surfaces as the pending request's failure, which handles cleanup.
WORK-BUFFER is left untouched."
  (ignore work-buffer)
  (when (buffer-live-p progress-buffer)
    (with-current-buffer progress-buffer
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-max))
          (insert (format "\nAgent notice: %s\n"
                          (string-trim
                           (format "%s" (or (alist-get 'message err)
                                            err))))))))))

(defun agent-recall--summarize-cleanup (work-buffer)
  "Clean up summarization ACP session in WORK-BUFFER."
  (when (buffer-live-p work-buffer)
    (with-current-buffer work-buffer
      (when agent-recall--summarize-session-id
        (ignore-errors
          (acp-send-notification
           :client agent-recall--summarize-client
           :notification (acp-make-session-cancel-notification
                          :session-id agent-recall--summarize-session-id
                          :reason "Summarization complete"))))
      (when agent-recall--summarize-client
        (ignore-errors
          (acp-shutdown :client agent-recall--summarize-client)))
      (when agent-recall--summarize-spinner-timer
        (cancel-timer agent-recall--summarize-spinner-timer))
      (when agent-recall--summarize-timeout-timer
        (cancel-timer agent-recall--summarize-timeout-timer))
      (setq agent-recall--summarize-client nil
            agent-recall--summarize-session-id nil
            agent-recall--summarize-response-text nil
            agent-recall--summarize-spinner-timer nil
            agent-recall--summarize-timeout-timer nil
            agent-recall--summarize-start-time nil
            agent-recall--summarize-progress-marker nil
            agent-recall--summarize-progress-buffer nil))))

(defun agent-recall--summarize-refresh-status (work-buffer)
  "Update spinner, char count, and timeout countdown in progress buffer.
Uses buffer-local state from WORK-BUFFER to render inline status."
  (when (buffer-live-p work-buffer)
    (let* ((progress-buf (buffer-local-value
                          'agent-recall--summarize-progress-buffer work-buffer))
           (marker (buffer-local-value
                    'agent-recall--summarize-progress-marker work-buffer))
           (idx (buffer-local-value
                 'agent-recall--summarize-spinner-index work-buffer))
           (text (buffer-local-value
                  'agent-recall--summarize-response-text work-buffer))
           (nchars (if text (length text) 0))
           (frame (aref agent-recall--spinner-frames
                        (mod idx (length agent-recall--spinner-frames))))
           (start-time (buffer-local-value
                        'agent-recall--summarize-start-time work-buffer))
           (remaining (if start-time
                         (max 0 (- agent-recall-summarize-timeout
                                   (floor (float-time
                                           (time-subtract nil start-time)))))
                       agent-recall-summarize-timeout)))
      (when (and (buffer-live-p progress-buf)
                 marker (marker-position marker))
        (with-current-buffer progress-buf
          (let ((inhibit-read-only t))
            (save-excursion
              (goto-char marker)
              (delete-region marker (line-end-position))
              (insert (format " %s %d received [%ds left]"
                              frame nchars remaining)))))))))

(defun agent-recall--summarize-start-spinner (work-buffer)
  "Start the spinner timer for WORK-BUFFER."
  (when (buffer-live-p work-buffer)
    (with-current-buffer work-buffer
      (when agent-recall--summarize-spinner-timer
        (cancel-timer agent-recall--summarize-spinner-timer))
      (setq agent-recall--summarize-spinner-index 0)
      (setq agent-recall--summarize-spinner-timer
            (run-with-timer
             0.1 0.1
             (lambda ()
               (when (buffer-live-p work-buffer)
                 (with-current-buffer work-buffer
                   (cl-incf agent-recall--summarize-spinner-index))
                 (agent-recall--summarize-refresh-status work-buffer))))))))

(defun agent-recall--summarize-stop-spinner (work-buffer)
  "Stop the spinner timer for WORK-BUFFER."
  (when (buffer-live-p work-buffer)
    (with-current-buffer work-buffer
      (when agent-recall--summarize-spinner-timer
        (cancel-timer agent-recall--summarize-spinner-timer)
        (setq agent-recall--summarize-spinner-timer nil)))))

(defun agent-recall--summarize-finalize-line (work-buffer progress-buffer text)
  "Stop spinner, clear inline status, and insert TEXT as the final status.
WORK-BUFFER owns the spinner and PROGRESS-BUFFER owns the status line.
TEXT should include a trailing newline to complete the current line."
  (agent-recall--summarize-stop-spinner work-buffer)
  (when (buffer-live-p progress-buffer)
    (with-current-buffer progress-buffer
      (let ((inhibit-read-only t)
            (marker (and (buffer-live-p work-buffer)
                         (buffer-local-value
                          'agent-recall--summarize-progress-marker
                          work-buffer))))
        (save-excursion
          (if (and marker (marker-position marker))
              (progn
                (goto-char marker)
                (delete-region marker (line-end-position)))
            (goto-char (point-max)))
          (insert text))))))

(defun agent-recall--summarize-next (files work-buffer progress-buffer
                                           total done)
  "Summarize the next transcript in FILES using WORK-BUFFER's ACP session.
PROGRESS-BUFFER shows status.  TOTAL and DONE track progress."
  (if (null files)
      (progn
        (agent-recall--summarize-cleanup work-buffer)
        (ignore-errors (kill-buffer work-buffer))
        (when (buffer-live-p progress-buffer)
          (with-current-buffer progress-buffer
            (let ((inhibit-read-only t))
              (goto-char (point-max))
              (insert (format "\nDone.  Summarized %d transcripts.\n" done))))))
    (let* ((file (car files))
           (rest (cdr files))
           (project (or (plist-get (gethash file agent-recall--index) :project)
                        "unknown"))
           (clean-content (agent-recall--clean-transcript-string file))
           (prompt (concat agent-recall--summarize-prompt clean-content))
           (gen nil))
      (when (buffer-live-p progress-buffer)
        (with-current-buffer progress-buffer
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (insert (format "  [%d/%d] [%s] %s..."
                            (1+ done) total project
                            (file-name-nondirectory file))))))
      ;; Reset accumulated text and set up progress tracking for this turn
      (with-current-buffer work-buffer
        (setq agent-recall--summarize-response-text "")
        (setq agent-recall--summarize-start-time (current-time))
        (setq agent-recall--summarize-progress-marker
              (with-current-buffer progress-buffer
                (copy-marker (point-max))))
        ;; Bump generation so stale callbacks from timed-out items are ignored
        (cl-incf agent-recall--summarize-generation)
        (setq gen agent-recall--summarize-generation)
        ;; Cancel any previous timeout timer
        (when agent-recall--summarize-timeout-timer
          (cancel-timer agent-recall--summarize-timeout-timer))
        (setq agent-recall--summarize-timeout-timer
              (run-with-timer
               agent-recall-summarize-timeout nil
               (lambda ()
                 (when (and (buffer-live-p work-buffer)
                            (= gen (buffer-local-value
                                    'agent-recall--summarize-generation
                                    work-buffer)))
                   ;; Cancel the in-flight turn so the abandoned stream's
                   ;; chunks don't bleed into the next item's accumulator.
                   (with-current-buffer work-buffer
                     (when agent-recall--summarize-session-id
                       (ignore-errors
                         (acp-send-notification
                          :client agent-recall--summarize-client
                          :notification (acp-make-session-cancel-notification
                                         :session-id agent-recall--summarize-session-id
                                         :reason "Summarization timeout")))))
                   (agent-recall--summarize-finalize-line
                    work-buffer progress-buffer
                    (format " ⏱ timeout (%ds)\n"
                            agent-recall-summarize-timeout))
                   (agent-recall--summarize-next
                    rest work-buffer progress-buffer total (1+ done)))))))
      (agent-recall--summarize-start-spinner work-buffer)
      (acp-send-request
       :client (buffer-local-value 'agent-recall--summarize-client work-buffer)
       :sync nil
       :request (acp-make-session-prompt-request
                 :session-id (buffer-local-value
                              'agent-recall--summarize-session-id work-buffer)
                 :prompt (vector (list (cons 'type "text")
                                       (cons 'text prompt))))
       :on-success
       (lambda (_result)
         ;; Ignore if this item was already timed out
         (when (and (buffer-live-p work-buffer)
                    (= gen (buffer-local-value
                            'agent-recall--summarize-generation work-buffer)))
           (when (buffer-live-p work-buffer)
             (with-current-buffer work-buffer
               (when agent-recall--summarize-timeout-timer
                 (cancel-timer agent-recall--summarize-timeout-timer)
                 (setq agent-recall--summarize-timeout-timer nil))))
           (condition-case err
               (let* ((text (and (buffer-live-p work-buffer)
                                 (with-current-buffer work-buffer
                                   (string-trim
                                    agent-recall--summarize-response-text))))
                      (nchars (if text (length text) 0))
                      (summary-file (agent-recall--summary-file file)))
                 (if (and text (not (string-empty-p text)))
                     (progn
                       (with-temp-file summary-file
                         (insert text "\n"))
                       (agent-recall--summarize-finalize-line
                        work-buffer progress-buffer
                        (format " ✓ %d chars → %s\n"
                                nchars (file-name-nondirectory summary-file))))
                   (agent-recall--summarize-finalize-line
                    work-buffer progress-buffer " ✗ empty output\n")))
             (error
              (agent-recall--summarize-finalize-line
               work-buffer progress-buffer
               (format " ✗ %s\n" (error-message-string err)))))
           ;; Continue to next file
           (agent-recall--summarize-next
            rest work-buffer progress-buffer total (1+ done))))
       :on-failure
       (lambda (err)
         ;; Ignore if this item was already timed out
         (when (and (buffer-live-p work-buffer)
                    (= gen (buffer-local-value
                            'agent-recall--summarize-generation work-buffer)))
           (when (buffer-live-p work-buffer)
             (with-current-buffer work-buffer
               (when agent-recall--summarize-timeout-timer
                 (cancel-timer agent-recall--summarize-timeout-timer)
                 (setq agent-recall--summarize-timeout-timer nil))))
           (agent-recall--summarize-finalize-line
            work-buffer progress-buffer
            (format " ✗ %S\n" err))
           (agent-recall--summarize-next
            rest work-buffer progress-buffer total (1+ done))))))))

(defun agent-recall--indexed-projects ()
  "Return the sorted list of distinct project names in the index."
  (let ((projects '()))
    (maphash (lambda (_file entry)
               (when-let ((project (plist-get entry :project)))
                 (unless (member project projects)
                   (push project projects))))
             agent-recall--index)
    (sort projects #'string<)))

;;;###autoload
(defun agent-recall-summarize (&optional projects)
  "Summarize un-summarized transcripts via ACP.
Creates a dedicated ACP session (independent of any agent-shell
buffer) to send each transcript through the LLM.  Summaries are
saved as TIMESTAMP.summary.md files next to the original transcripts.

Interactively, prompts for one or more PROJECTS to limit the batch
\(comma-separated; empty input means all projects).  From Lisp,
PROJECTS is a list of project-name strings, or nil for all.

This is a user-initiated batch operation.  Transcripts that already
have a summary file are skipped.  Progress is shown in the
*agent-recall-summarize* buffer."
  (interactive
   (progn
     (agent-recall--index-ensure)
     (list (completing-read-multiple
            "Summarize projects (empty = all): "
            (agent-recall--indexed-projects)))))
  (agent-recall--index-ensure)
  (let ((config (agent-shell-select-config
                 :prompt "Select agent for summarization: ")))
    (unless config
      (user-error "No agent config selected"))
    (let ((files '()))
      (maphash (lambda (file entry)
                 (when (and (file-exists-p file)
                            (agent-recall--needs-summary-p file)
                            (or (null projects)
                                (member (plist-get entry :project) projects)))
                   (push file files)))
               agent-recall--index)
      (unless files
        (user-error "All matching transcripts already have summaries"))
      (setq files (sort files #'string<))
      (let* ((progress-buffer (get-buffer-create "*agent-recall-summarize*"))
             (work-buffer (generate-new-buffer " *agent-recall-summarize-work*"))
             (client nil))
        (with-current-buffer progress-buffer
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (format "Agent Recall — Summarizing %d transcripts\n"
                            (length files)))
            (insert (make-string 40 ?═) "\n\n"))
          (special-mode))
        (pop-to-buffer progress-buffer)
        ;; Set up ACP client in the hidden work buffer
        (with-current-buffer work-buffer
          (setq agent-recall--summarize-response-text "")
          (setq agent-recall--summarize-progress-buffer progress-buffer)
          (setq client (funcall (alist-get :client-maker config) work-buffer))
          (setq agent-recall--summarize-client client)
          ;; Subscribe to notifications — accumulate agent_message_chunk text
          (acp-subscribe-to-notifications
           :client client
           :buffer work-buffer
           :on-notification
           (lambda (notification)
             (when (buffer-live-p work-buffer)
               (with-current-buffer work-buffer
                 (let-alist notification
                   (when (equal .method "session/update")
                     (let ((update (alist-get 'update .params)))
                       (when (equal (alist-get 'sessionUpdate update)
                                    "agent_message_chunk")
                         (let-alist update
                           (setq agent-recall--summarize-response-text
                                 (concat agent-recall--summarize-response-text
                                         .content.text)))
                         (agent-recall--summarize-refresh-status work-buffer)))))))))
          ;; Subscribe to errors (agent stderr): log-only, never tear down.
          (acp-subscribe-to-errors
           :client client
           :buffer work-buffer
           :on-error
           (lambda (err)
             (agent-recall--summarize-handle-agent-error
              work-buffer progress-buffer err)))
          ;; Initialize → New session → Start summarizing
          (acp-send-request
           :client client
           :sync nil
           :request (acp-make-initialize-request
                     :protocol-version 1
                     :read-text-file-capability nil
                     :write-text-file-capability nil)
           :on-success
           (lambda (_result)
             (when (buffer-live-p work-buffer)
               (acp-send-request
                :client client
                :sync nil
                :request (acp-make-session-new-request
                          :cwd default-directory
                          :mcp-servers [])
                :on-success
                (lambda (session-response)
                  (when (buffer-live-p work-buffer)
                    (with-current-buffer work-buffer
                      (setq agent-recall--summarize-session-id
                            (alist-get 'sessionId session-response)))
                    (agent-recall--summarize-next
                     files work-buffer progress-buffer
                     (length files) 0)))
                :on-failure
                (lambda (err)
                  (when (buffer-live-p progress-buffer)
                    (with-current-buffer progress-buffer
                      (let ((inhibit-read-only t))
                        (goto-char (point-max))
                        (insert (format "\nSession creation failed: %S\n" err)))))
                  (agent-recall--summarize-cleanup work-buffer)
                  (ignore-errors (kill-buffer work-buffer))))))
           :on-failure
           (lambda (err)
             (when (buffer-live-p progress-buffer)
               (with-current-buffer progress-buffer
                 (let ((inhibit-read-only t))
                   (goto-char (point-max))
                   (insert (format "\nInitialize failed: %S\n" err)))))
             (agent-recall--summarize-cleanup work-buffer)
             (ignore-errors (kill-buffer work-buffer)))))))))

(provide 'agent-recall)
;;; agent-recall.el ends here
