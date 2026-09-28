;;; agent-recall-consult.el --- Rich Consult backend for agent-recall -*- lexical-binding: t; -*-

;; Author: Umar Ahmad
;; Keywords: tools, convenience, ai

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Rich Consult backend for agent-recall.  Aggregates ripgrep matches
;; per session and renders one candidate per transcript as
;;
;;   [project] [N] DATE-TIME first-matched-line
;;
;; where N is the total match count in that session.  File path and
;; line number are stored as text properties so RET still jumps to the
;; exact match; preview and embark integration work like in
;; `consult-ripgrep'.
;;
;; By default only resumable sessions (those with a known session ID
;; in the agent-recall index) are shown.  Set
;; `agent-recall-consult-resumable-only' to nil to show everything,
;; with a leading status indicator.
;;
;; Usage:
;;
;;   (require 'agent-recall-consult)
;;   (keymap-global-set "C-c q s" #'agent-recall-consult-search)

;;; Code:

(require 'agent-recall)
(eval-when-compile (require 'agent-recall))
(require 'cl-lib)
(require 'consult nil t)

(defvar agent-recall--index)
(defvar agent-recall-auto-transcript-mode)
(defvar agent-recall-browse-preview)
(defvar agent-recall--browse-history)
(defvar agent-recall-file-patterns)
(defvar agent-recall-rg-executable)
(defvar agent-recall-search-extra-args)
(declare-function agent-recall--index-dirs "agent-recall")
(declare-function agent-recall--candidate-file "agent-recall")
(declare-function agent-recall--candidate-identity "agent-recall")
(declare-function agent-recall--candidate-key "agent-recall")
(declare-function agent-recall--candidate-kind "agent-recall")
(declare-function agent-recall--candidate-line "agent-recall")
(declare-function agent-recall--canonical-file "agent-recall")
(declare-function agent-recall--disambiguate-candidates "agent-recall")
(declare-function agent-recall--make-candidate "agent-recall")
(declare-function agent-recall--navigation-attach "agent-recall")
(declare-function agent-recall--navigation-cleanup "agent-recall")
(declare-function agent-recall--navigation-new-session "agent-recall")
(declare-function agent-recall--navigation-request-abort "agent-recall")
(declare-function agent-recall--open-transcript "agent-recall")
(declare-function agent-recall--project-name "agent-recall")
(declare-function agent-recall--provider-icon "agent-recall")
(declare-function agent-recall--transcript-file-p "agent-recall")
(declare-function agent-recall-transcript-mode "agent-recall")

(defvar consult--grep-history)
(defvar consult--completion-candidate-hook)
(defvar consult--grep-match-regexp)
(defvar consult-grep-max-columns)
(defvar consult-ripgrep-args)
(declare-function consult--build-args "consult")
(declare-function consult--dynamic-collection "consult")
(declare-function consult--file-action "consult")
(declare-function consult--jump-state "consult")
(declare-function consult--lookup-member "consult")
(declare-function consult--marker-from-line-column "consult")
(declare-function consult--read "consult")
(declare-function consult--ripgrep-make-builder "consult")
(declare-function consult--temporary-files "consult")
(declare-function consult--buffer-preview "consult")
(declare-function consult--tofu-encode "consult")
(declare-function vertico-exit "vertico")
(declare-function vertico-suspend "vertico-suspend")

(defvar vertico-mode)
(defvar vertico--input)
(defvar vertico-suspend--ov)

(defvar agent-recall-consult--read-lookup nil
  "Candidate lookup dynamically bound by a suspendable Consult read.")

(defvar agent-recall-consult--identity-ids nil
  "Stable identity-to-tofu mapping for a suspendable Consult read.")

(defvar agent-recall-consult--next-identity-id 0
  "Next tofu identifier in the current suspendable Consult read.")

(defvar-local agent-recall-consult--picker-lookup nil
  "Candidate lookup owned by the current picker minibuffer.")

(defgroup agent-recall-consult nil
  "Rich Consult backend for agent-recall."
  :group 'agent-recall
  :prefix "agent-recall-consult-")

(defcustom agent-recall-consult-resumable-only t
  "When non-nil, hide non-resumable transcripts from search results.
Transcripts whose index entry lacks a `:session-id' (or have no entry
at all) are filtered out entirely.  When nil, all matching transcripts
are shown with a leading status indicator (`●' resumable, `○' not)."
  :type 'boolean
  :group 'agent-recall-consult)

(defcustom agent-recall-consult-sort-order nil
  "How to sort search results.
When nil, results appear in ripgrep's default order.  When
`date-descending', results are sorted newest-first by the
timestamp in the filename.  When `date-ascending', oldest-first."
  :type '(choice (const :tag "Default (ripgrep order)" nil)
                 (const :tag "Newest first" date-descending)
                 (const :tag "Oldest first" date-ascending))
  :group 'agent-recall-consult)

(defun agent-recall-consult--ensure-consult ()
  "Ensure Consult is available."
  (unless (require 'consult nil t)
    (user-error
     "Consult is not installed.  Install it to use `agent-recall-consult-search'")))

(defun agent-recall-consult--suspend-available-p ()
  "Return non-nil when this Consult session can use Vertico suspension."
  (and agent-recall-auto-transcript-mode
       (bound-and-true-p vertico-mode)
       (require 'vertico-suspend nil t)
       (fboundp 'vertico-suspend)
       (fboundp 'vertico-exit)
       (fboundp 'consult--tofu-encode)))

(defun agent-recall-consult--encode-candidates (candidates)
  "Give CANDIDATES unique Consult tofu identities and update lookup state."
  (mapcar
   (lambda (candidate)
     (let* ((identity
             (or (get-text-property 0 'agent-recall-identity candidate)
                 (agent-recall--candidate-identity
                  (agent-recall--candidate-file candidate)
                  (agent-recall--candidate-line candidate)
                  (agent-recall--candidate-kind candidate))))
            (missing (make-symbol "missing"))
            (id (gethash identity agent-recall-consult--identity-ids missing)))
       (when (eq id missing)
         (setq id agent-recall-consult--next-identity-id)
         (cl-incf agent-recall-consult--next-identity-id)
         (puthash identity id agent-recall-consult--identity-ids))
       (let ((encoded (concat candidate (consult--tofu-encode id))))
         (puthash (agent-recall--candidate-key encoded)
                  encoded agent-recall-consult--read-lookup)
         encoded)))
   candidates))

(defun agent-recall-consult--prepare-candidates (candidates)
  "Disambiguate CANDIDATES and encode them for an active suspended read."
  (setq candidates (agent-recall--disambiguate-candidates candidates))
  (if agent-recall-consult--read-lookup
      (agent-recall-consult--encode-candidates candidates)
    candidates))

(defun agent-recall-consult--lookup-candidate (candidate lookup)
  "Resolve CANDIDATE through LOOKUP, retaining its payload properties."
  (and candidate
       (or (and lookup
                (gethash (agent-recall--candidate-key candidate) lookup))
           candidate)))

(defun agent-recall-consult--browse-preview-state (&optional file-lookup)
  "Return the Consult state function for transcript previews.
FILE-LOOKUP is an optional compatibility map from raw identities to files."
  (let ((open (consult--temporary-files))
        (preview (consult--buffer-preview)))
    (lambda (action candidate)
      (unless candidate
        (funcall open))
      (let* ((resolved (agent-recall-consult--lookup-candidate
                        candidate agent-recall-consult--read-lookup))
             (file (or (agent-recall--candidate-file resolved)
                       (and file-lookup
                            (gethash (agent-recall--candidate-key candidate)
                                     file-lookup))))
             (buffer (and file
                          (eq action 'preview)
                          (funcall open file))))
        (funcall preview action (and buffer (buffer-name buffer)))))))

(defun agent-recall-consult--picker-structurally-valid-p (session suspended)
  "Return non-nil when SESSION owns the visible picker.
When SUSPENDED is non-nil, also require Vertico's suspension overlay."
  (when-let* ((window (active-minibuffer-window))
              ((window-live-p window))
              (buffer (window-buffer window))
              ((eq buffer
                   (agent-recall--navigation-session-minibuffer session)))
              ((buffer-live-p buffer))
              ((minibufferp buffer)))
    (with-current-buffer buffer
      (and (eq agent-recall--picker-navigation-session session)
           (bound-and-true-p vertico--input)
           (or (not suspended)
               (and (overlayp vertico-suspend--ov)
                    (overlay-buffer vertico-suspend--ov)))))))

(defun agent-recall-consult--session-valid-p (session)
  "Return non-nil when SESSION is the exact suspended Vertico picker."
  (and (eq (agent-recall--navigation-session-state session) 'suspended)
       (featurep 'vertico-suspend)
       (fboundp 'vertico-suspend)
       (agent-recall-consult--picker-structurally-valid-p session t)))

(defun agent-recall-consult--resume-session (session)
  "Resume the exact Vertico picker recorded in SESSION."
  (condition-case err
      (progn
        (unless (agent-recall-consult--session-valid-p session)
          (user-error "The originating picker is no longer available"))
        (vertico-suspend)
        (setf (agent-recall--navigation-session-state session) 'picker))
    (error
     (agent-recall--navigation-request-abort session)
     (signal (car err) (cdr err)))))

(defun agent-recall-consult--abort-session (session)
  "Abort the active minibuffer owned by SESSION."
  (if (agent-recall-consult--picker-structurally-valid-p session nil)
      (let ((window (active-minibuffer-window)))
        (setf (agent-recall--navigation-session-state session) 'aborting)
        (select-window window)
        (abort-recursive-edit)
        t)
    (let* ((minibuffer
            (agent-recall--navigation-session-minibuffer session))
           (active (active-minibuffer-window)))
      (cond
       ((or (not (buffer-live-p minibuffer))
            (not (window-live-p active))
            (eq (window-buffer active) minibuffer))
        (agent-recall--navigation-cleanup session)
        t)
       (t
        ;; An unrelated nested minibuffer is active.  Wait for it to unwind;
        ;; aborting it would violate the session identity guarantee.
        (setf (agent-recall--navigation-session-state session) 'orphaned)
        nil)))))

(defun agent-recall-consult--activate-selection (session file line)
  "Visit FILE at LINE and attach it to suspended SESSION."
  (cond
   ((eq (agent-recall--navigation-session-state session) 'closed))
   ((not (and (agent-recall-consult--session-valid-p session)
              (window-live-p
               (agent-recall--navigation-session-origin-window session))
              (file-exists-p file)))
    (message "agent-recall: suspended selection became unavailable")
    (agent-recall--navigation-request-abort session))
   (t
    (condition-case err
        (with-selected-window
            (agent-recall--navigation-session-origin-window session)
          (agent-recall--open-transcript file nil line t)
          (agent-recall--navigation-attach session (current-buffer)))
      (error
       (message "agent-recall: could not activate transcript: %s"
                (error-message-string err))
       (agent-recall--navigation-request-abort session))))))

(defun agent-recall-consult--current-candidate ()
  "Return the candidate highlighted by the active Consult frontend."
  (run-hook-with-args-until-success 'consult--completion-candidate-hook))

(defun agent-recall-consult--visit ()
  "Suspend the current picker and visit its highlighted result."
  (interactive)
  (let* ((session agent-recall--picker-navigation-session)
         (candidate
          (agent-recall-consult--lookup-candidate
           (agent-recall-consult--current-candidate)
           agent-recall-consult--picker-lookup))
         (file (agent-recall--candidate-file candidate))
         (line (agent-recall--candidate-line candidate)))
    (unless (and session
                 (agent-recall-consult--picker-structurally-valid-p session nil))
      (user-error "No active Agent Recall Vertico picker"))
    (unless (and file (file-exists-p file))
      (user-error "The selected candidate has no live transcript file"))
    (setf (agent-recall--navigation-session-selected-file session) file
          (agent-recall--navigation-session-selected-line session) line
          (agent-recall--navigation-session-state session) 'suspending)
    (let (did-suspend)
      (condition-case err
          (progn
            (vertico-suspend)
            (setq did-suspend t)
            (setf (agent-recall--navigation-session-state session) 'suspended)
            ;; Give Consult's window-selection hook one turn to promote a
            ;; temporary preview before opening the fully initialized file.
            (run-at-time 0 nil #'agent-recall-consult--activate-selection
                         session file line))
        (error
         (if did-suspend
             (agent-recall--navigation-request-abort session)
           (setf (agent-recall--navigation-session-state session) 'picker))
         (signal (car err) (cdr err)))))))

(defun agent-recall-consult--final-accept ()
  "Accept the current Vertico candidate and close its picker."
  (interactive)
  (unless (and agent-recall--picker-navigation-session
               (agent-recall-consult--picker-structurally-valid-p
                agent-recall--picker-navigation-session nil))
    (user-error "No active Agent Recall Vertico picker"))
  (vertico-exit))

(defvar agent-recall-consult--picker-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'agent-recall-consult--visit)
    (define-key map (kbd "M-RET") #'agent-recall-consult--final-accept)
    map)
  "Command-specific keymap for suspendable Agent Recall pickers.")

(defun agent-recall-consult--setup-picker (session lookup)
  "Attach SESSION and candidate LOOKUP to the current minibuffer."
  (setf (agent-recall--navigation-session-minibuffer session) (current-buffer))
  (setq-local agent-recall--picker-navigation-session session)
  (setq-local agent-recall-consult--picker-lookup lookup))

(defun agent-recall-consult--suspendable-read (kind table options lookup)
  "Run a suspendable Consult read of KIND over TABLE with OPTIONS and LOOKUP."
  (let ((session
         (agent-recall--navigation-new-session
          kind 'suspended
          :origin-window (selected-window)
          :valid-function #'agent-recall-consult--session-valid-p
          :resume-function #'agent-recall-consult--resume-session
          :abort-function #'agent-recall-consult--abort-session)))
    (unwind-protect
        (let ((enable-recursive-minibuffers t)
              (read-options (copy-sequence options)))
          (setq read-options
                (plist-put read-options :keymap agent-recall-consult--picker-map))
          (minibuffer-with-setup-hook
              (lambda () (agent-recall-consult--setup-picker session lookup))
            (apply #'consult--read table read-options)))
      (agent-recall--navigation-cleanup session))))

(defun agent-recall-consult--browse-read (candidates annotate-function)
  "Read Browse CANDIDATES with Consult and ANNOTATE-FUNCTION."
  (agent-recall-consult--ensure-consult)
  (if (agent-recall-consult--suspend-available-p)
      (let ((agent-recall-consult--read-lookup
             (make-hash-table :test 'equal))
            (agent-recall-consult--identity-ids
             (make-hash-table :test 'equal))
            (agent-recall-consult--next-identity-id 0))
        (setq candidates (agent-recall-consult--encode-candidates candidates))
        (agent-recall-consult--suspendable-read
         'browse candidates
         (list :prompt "Transcript (RET visits, M-RET closes): "
               :annotate
               (lambda (candidate)
                 (funcall annotate-function
                          (agent-recall-consult--lookup-candidate
                           candidate agent-recall-consult--read-lookup)))
               :state (and agent-recall-browse-preview
                           (agent-recall-consult--browse-preview-state))
               :lookup #'consult--lookup-member
               :category 'agent-recall-transcript
               :sort nil
               :require-match t
               :history 'agent-recall--browse-history)
         agent-recall-consult--read-lookup))
    (consult--read
     candidates
     :prompt "Transcript: "
     :annotate annotate-function
     :state (and agent-recall-browse-preview
                 (agent-recall-consult--browse-preview-state))
     :lookup #'consult--lookup-member
     :category 'agent-recall-transcript
     :sort nil
     :require-match t
     :default (car agent-recall--browse-history)
     :history 'agent-recall--browse-history)))

(defun agent-recall-consult--ripgrep-args ()
  "Return `consult-ripgrep-args' customized for agent-recall transcripts."
  (let ((args (consult--build-args consult-ripgrep-args)))
    (append (cons agent-recall-rg-executable (cdr args))
            agent-recall-search-extra-args
            (cl-mapcan (lambda (pat) (list "--glob" pat))
                       (agent-recall--file-patterns)))))

(defun agent-recall-consult--humanize-timestamp (basename)
  "Format BASENAME like `2026-04-30-15-32-21' as `30 Apr 26 03:32 PM'.
Also accepts the ISO-style `T' separator.  Falls back to BASENAME on no
match."
  (let ((parts (split-string basename "[-T]")))
    (if (= (length parts) 6)
        (format-time-string
         "%d %b %y %I:%M %p"
         (apply #'encode-time
                (nreverse (mapcar #'string-to-number parts))))
      basename)))

(defun agent-recall-consult--build-candidate (file count line content
                                                   proj-width count-width)
  "Build one aggregated candidate string for FILE.
COUNT is the total match count, LINE is the first matched line, CONTENT
is its trimmed text.  PROJ-WIDTH and COUNT-WIDTH are the longest
project name and count digit-string in the current result set, used to
pad those columns so the date column aligns.  File path and line are
stored as text properties.  When `agent-recall-consult-resumable-only'
is nil, a leading indicator shows whether the session is resumable.
Sessions with a user-assigned label (see `agent-recall-session-label')
show it after the date column."
  (let* ((entry (gethash file agent-recall--index))
         (canonical (agent-recall--canonical-file file))
         (project (or (plist-get entry :project)
                      (agent-recall--project-name (file-name-directory file))))
         (resumable (and entry (plist-get entry :session-id)))
         (indicator (cond
                     (agent-recall-consult-resumable-only "")
                     (resumable (concat (propertize "●" 'face 'success
                                                   'help-echo "Resumable")
                                        " "))
                     (t (concat (propertize "○" 'face 'shadow
                                            'help-echo "No session ID; not resumable")
                                " "))))
         (count-str (number-to-string count))
         (proj-pad (make-string (max 0 (- proj-width (length project))) ?\s))
         (count-pad (make-string (max 0 (- count-width (length count-str))) ?\s))
         (timestamp (agent-recall-consult--humanize-timestamp
                     (file-name-sans-extension (file-name-nondirectory file))))
         (cand (concat
                (agent-recall--provider-icon file entry)
                indicator
                (propertize (format "[%s]" project) 'face 'consult-file)
                proj-pad
                " "
                (propertize (format "[%s]" count-str) 'face 'consult-line-number)
                count-pad
                " "
                (propertize timestamp 'face 'shadow)
                (agent-recall--label-suffix (plist-get entry :session-id))
                " "
                content)))
    (setq cand (agent-recall--make-candidate cand canonical line 'search))
    (add-text-properties 0 (length cand)
                         (list 'agent-recall-consult-file canonical
                               'agent-recall-consult-line line)
                         cand)
    cand))

(defun agent-recall-consult--search-fn (input)
  "Run ripgrep for INPUT, aggregate matches by file, return candidates.
Each candidate is `[project] [N] DATE-TIME first-matched-line'."
  (agent-recall-consult--ensure-consult)
  (let* ((dirs (agent-recall--index-dirs))
         (consult-ripgrep-args
          (agent-recall-consult--ripgrep-args))
         (builder (consult--ripgrep-make-builder dirs))
         (built (funcall builder input)))
    (when built
      (let* ((cmd (car built))
             (highlight (cdr built))
             (by-file (make-hash-table :test 'equal))
             ;; Transcripts whose preview text already comes from a
             ;; summary line; later raw-transcript hits must not replace it.
             (summary-previewed (make-hash-table :test 'equal))
             (order '())
             (output (with-temp-buffer
                       (apply #'call-process (car cmd) nil t nil (cdr cmd))
                       (buffer-string))))
        (save-match-data
          (dolist (str (split-string output "\n" t))
            (when (string-match consult--grep-match-regexp str)
              (let* ((hit-file (match-string 1 str))
                     (hit-line (string-to-number (match-string 2 str)))
                     (raw (substring str (match-end 0)))
                     ;; A summary hit counts for its transcript: the
                     ;; transcript is what is indexed, resumable, and
                     ;; visited.  Summaries have their own line numbers,
                     ;; so point the candidate at the transcript top.
                     (file (agent-recall--summary-parent-file hit-file))
                     (summary-p (not (equal file hit-file)))
                     (lnum (if summary-p 1 hit-line))
                     (text (if (and consult-grep-max-columns
                                    (length> raw consult-grep-max-columns))
                               (substring raw 0 consult-grep-max-columns)
                             raw))
                     (entry (gethash file by-file)))
                (cond
                 (entry
                  (cl-incf (car entry))
                  ;; A summary line (Topic, Problem, Outcome, ...) is a
                  ;; far better preview than the first raw transcript
                  ;; line, so the first summary hit takes over the text.
                  (when (and summary-p
                             (not (gethash file summary-previewed)))
                    (when highlight (funcall highlight text))
                    (setcar (cddr entry) text)
                    (puthash file t summary-previewed)))
                 ((and agent-recall-consult-resumable-only
                       (not (plist-get (gethash file agent-recall--index)
                                       :session-id))))
                 (t
                  (when highlight (funcall highlight text))
                  (puthash file (list 1 lnum text) by-file)
                  (when summary-p (puthash file t summary-previewed))
                  (push file order)))))))
        (let* ((files (let ((lst (nreverse order)))
                       (if agent-recall-consult-sort-order
                           (let ((cmp (if (eq agent-recall-consult-sort-order
                                              'date-descending)
                                          #'string> #'string<)))
                             (sort lst (lambda (a b)
                                         (let ((an (file-name-nondirectory a))
                                               (bn (file-name-nondirectory b)))
                                           (if (equal an bn)
                                               (string<
                                                (agent-recall--canonical-file a)
                                                (agent-recall--canonical-file b))
                                             (funcall cmp an bn))))))
                         lst)))
               (proj-width 0)
               (count-width 0))
          (dolist (file files)
            (let ((entry (gethash file agent-recall--index))
                  (count (car (gethash file by-file))))
              (setq proj-width
                    (max proj-width
                         (length (or (plist-get entry :project)
                                     (agent-recall--project-name
                                      (file-name-directory file))))))
              (setq count-width
                    (max count-width (length (number-to-string count))))))
          (agent-recall-consult--prepare-candidates
           (mapcar (lambda (file)
                     (pcase-let ((`(,count ,lnum ,text) (gethash file by-file)))
                       (agent-recall-consult--build-candidate
                        file count lnum text proj-width count-width)))
                   files)))))))

(defun agent-recall-consult--position (cand &optional find-file)
  "Return (MARKER) for CAND opening the file via FIND-FILE."
  (when cand
    (when-let* ((file (or (agent-recall--candidate-file cand)
                          (get-text-property
                           0 'agent-recall-consult-file cand)))
                (line (or (agent-recall--candidate-line cand)
                          (get-text-property
                           0 'agent-recall-consult-line cand)))
                (buf  (funcall (or find-file #'consult--file-action) file))
                (pos  (consult--marker-from-line-column buf line 0)))
      (cons pos nil))))

(defun agent-recall-consult--state ()
  "State function: live preview the transcript at the matched line."
  (let ((open (consult--temporary-files))
        (jump (consult--jump-state)))
    (lambda (action cand)
      (unless cand (funcall open))
      (funcall jump action
               (agent-recall-consult--position
                cand
                (and (not (eq action 'return)) open))))))

;;;###autoload
(defun agent-recall-consult-search ()
  "Live ripgrep over indexed agent-recall transcripts.
Aggregates matches per session and renders candidates as
`[project] [match-count] DATE-TIME first-matched-line'.  Selection
jumps to the first match in the chosen transcript."
  (interactive)
  (agent-recall-consult--ensure-consult)
  (let ((dirs (agent-recall--index-dirs)))
    (unless dirs
      (user-error "No transcripts indexed.  Run M-x agent-recall-reindex"))
    (let* ((suspendable (agent-recall-consult--suspend-available-p))
           (lookup (and suspendable (make-hash-table :test 'equal)))
           (agent-recall-consult--read-lookup lookup)
           (agent-recall-consult--identity-ids
            (and suspendable (make-hash-table :test 'equal)))
           (agent-recall-consult--next-identity-id 0)
           (table
            (consult--dynamic-collection #'agent-recall-consult--search-fn))
           (options
            (list :prompt (if suspendable
                              "Recall (RET visits, M-RET closes): "
                            "Recall: ")
                  :lookup #'consult--lookup-member
                  :state (agent-recall-consult--state)
                  :require-match t
                  :category 'consult-grep
                  :history '(:input consult--grep-history)
                  :sort nil))
           (selected
            (if suspendable
                (agent-recall-consult--suspendable-read
                 'search table options lookup)
              (apply #'consult--read table options))))
      (when (and selected
                 agent-recall-auto-transcript-mode
                 (agent-recall--transcript-file-p (buffer-file-name)))
        (agent-recall-transcript-mode 1)))))

(provide 'agent-recall-consult)
;;; agent-recall-consult.el ends here
