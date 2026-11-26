;;; test-deadgrep.el --- Real deadgrep search regressions -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise the deadgrep backend with real ripgrep processes and symlinks.
;; Requires deadgrep and ripgrep; tests skip when either is unavailable.

;;; Code:

(require 'ert)
(require 'agent-recall)

(defmacro agent-recall-test--with-deadgrep (&rest body)
  "Run BODY with isolated transcripts and a temporary search index."
  (declare (indent 0))
  `(progn
     (skip-unless (require 'deadgrep nil t))
     (skip-unless (executable-find "rg"))
     (let* ((root (make-temp-file "agent-recall-deadgrep-" t))
            (transcript-dir (expand-file-name "project/.agent-shell/transcripts" root))
            (agent-recall-index-file (expand-file-name "state/index.el" root))
            (agent-recall--index (make-hash-table :test 'equal))
            (agent-recall--index-loaded-p t)
            (agent-recall--symlink-dir nil)
            (agent-recall-file-patterns '("*.md" "*.org"))
            (agent-recall--search-scope 'transcripts)
            (agent-recall-auto-transcript-mode nil)
            (agent-recall-search-function 'deadgrep)
            (deadgrep-extra-arguments '("--no-config"))
            (deadgrep-max-buffers nil)
            (deadgrep-display-buffer-function #'switch-to-buffer)
            (current-prefix-arg nil)
            results-buffer)
       (make-directory transcript-dir t)
       (dolist (fixture '(("chat.md" . "needle transcript markdown\n")
                          ("chat.org" . "needle transcript org\n")
                          ("chat.summary.md" . "needle summary markdown\n")
                          ("chat.summary.org" . "needle summary org\n")
                          ("unrelated.txt" . "needle unrelated text\n")))
         (let ((file (expand-file-name (car fixture) transcript-dir)))
           (with-temp-file file (insert (cdr fixture)))
           (when (member (car fixture) '("chat.md" "chat.org"))
             (puthash file (list :dir transcript-dir :project "project")
                      agent-recall--index))))
       (save-window-excursion
         (unwind-protect
             (progn ,@body)
           (dolist (buffer (buffer-list))
             (when (with-current-buffer buffer
                     (and (derived-mode-p 'deadgrep-mode)
                          (string-prefix-p root default-directory)))
               (when-let* ((process (get-buffer-process buffer)))
                 (delete-process process))
               (kill-buffer buffer)))
           (delete-directory root t))))))

(defun agent-recall-test--wait-for-deadgrep ()
  "Wait for the real search in the current buffer and require success."
  (let ((process (get-buffer-process (current-buffer)))
        (deadline (+ (float-time) 5)))
    (should (processp process))
    (while (and deadgrep--running (< (float-time) deadline))
      (accept-process-output process 0.05))
    (ert-info ((buffer-substring-no-properties (point-min) (point-max)))
      (should-not deadgrep--running)
      (should (= (process-exit-status process) 0)))))

(ert-deftest agent-recall-deadgrep-searches-transcripts-through-symlinks ()
  "The real search finds Markdown and Org transcripts, excluding sidecars."
  (agent-recall-test--with-deadgrep
    (agent-recall--search-via-deadgrep "needle" nil)
    (setq results-buffer (current-buffer))
    (agent-recall-test--wait-for-deadgrep)
    (should (= deadgrep--result-count 2))
    (should (string-match-p "needle transcript markdown" (buffer-string)))
    (should (string-match-p "needle transcript org" (buffer-string)))
    (should-not (string-match-p "needle summary" (buffer-string)))
    (should-not (string-match-p "needle unrelated" (buffer-string)))))

(ert-deftest agent-recall-deadgrep-searches-only-summaries ()
  "The real summaries search includes sidecars and excludes transcripts."
  (agent-recall-test--with-deadgrep
    (let ((agent-recall--search-scope 'summaries))
      (agent-recall--search-via-deadgrep "needle" nil))
    (setq results-buffer (current-buffer))
    (agent-recall-test--wait-for-deadgrep)
    (should (= deadgrep--result-count 2))
    (should (string-match-p "needle summary markdown" (buffer-string)))
    (should (string-match-p "needle summary org" (buffer-string)))
    (should-not (string-match-p "needle transcript" (buffer-string)))
    (should-not (string-match-p "needle unrelated" (buffer-string)))))

(ert-deftest agent-recall-deadgrep-restart-preserves-symlinks-and-scope ()
  "Restart follows symlinks and retains the initial summaries-only scope."
  (agent-recall-test--with-deadgrep
    (let ((agent-recall--search-scope 'summaries))
      (agent-recall--search-via-deadgrep "needle" nil))
    (setq results-buffer (current-buffer))
    (agent-recall-test--wait-for-deadgrep)
    (should (= deadgrep--result-count 2))
    (with-temp-file (expand-file-name "new.summary.md" transcript-dir)
      (insert "needle new summary\n"))
    (with-temp-file (expand-file-name "new.md" transcript-dir)
      (insert "needle new transcript\n"))
    (deadgrep-restart)
    (agent-recall-test--wait-for-deadgrep)
    (should (= deadgrep--result-count 3))
    (should (string-match-p "needle new summary" (buffer-string)))
    (should-not (string-match-p "needle transcript" (buffer-string)))
    (should-not (string-match-p "needle new transcript" (buffer-string)))))

(ert-deftest agent-recall-deadgrep-switches-scope-from-results ()
  "New and reused result buffers use only the requested scope, also on restart."
  (dolist (reuse '(nil t))
    (agent-recall-test--with-deadgrep
      (dolist (scope '(summaries transcripts summaries))
        (let ((query (if (or reuse (eq scope 'summaries)) "needle" "need")))
          (if (eq scope 'summaries)
              (agent-recall-search-summaries query)
            (agent-recall-search query)))
        (setq results-buffer (current-buffer))
        (agent-recall-test--wait-for-deadgrep)
        (should (= deadgrep--result-count 2))
        (should-not (string-match-p (if (eq scope 'summaries)
                                       "needle transcript" "needle summary")
                                   (buffer-string)))
        (deadgrep-restart)
        (agent-recall-test--wait-for-deadgrep)
        (should (= deadgrep--result-count 2))))))

(ert-deftest agent-recall-deadgrep-keeps-user-options-with-custom-display ()
  "Honor local user options and a display function that does not select results."
  (agent-recall-test--with-deadgrep
    (with-temp-buffer
      (setq-local deadgrep-extra-arguments '("--no-config" "--max-count=1"))
      (dolist (file '("chat.md" "chat.org" "chat.summary.md" "chat.summary.org"))
        (write-region "needle extra matching line\n" nil
                      (expand-file-name file transcript-dir) t 'silent))
      (let* ((origin (current-buffer))
             (original-options (copy-sequence deadgrep-extra-arguments))
             (agent-recall-auto-transcript-mode t)
             (deadgrep-display-buffer-function
              (lambda (buffer) (setq results-buffer buffer))))
        (agent-recall-search "needle")
        (should (eq (current-buffer) origin))
        (should (equal deadgrep-extra-arguments original-options))
        (with-current-buffer results-buffer
          (agent-recall-test--wait-for-deadgrep)
          (should (= deadgrep--result-count 2))
          (should (string-match-p "needle transcript markdown" (buffer-string)))
          (should agent-recall--search-buffer-p)
          (agent-recall-search-summaries "need"))
        (with-current-buffer results-buffer
          (agent-recall-test--wait-for-deadgrep)
          (should (= deadgrep--result-count 2))
          (should (string-match-p "needle summary markdown" (buffer-string)))
          (deadgrep-restart)
          (agent-recall-test--wait-for-deadgrep)
          (should (= deadgrep--result-count 2)))
        (should (equal deadgrep-extra-arguments original-options))))))

(ert-deftest agent-recall-deadgrep-user-globs-cannot-broaden-scope ()
  "Recall's scope wins over custom inclusion globs, including on restart."
  (agent-recall-test--with-deadgrep
    (let ((deadgrep-extra-arguments '("--no-config" "--glob=*" "--glob=*.md")))
      (dolist (scope '(transcripts summaries))
        (if (eq scope 'summaries)
            (agent-recall-search-summaries "needle")
          (agent-recall-search "needle"))
        (setq results-buffer (current-buffer))
        (agent-recall-test--wait-for-deadgrep)
        (should (= deadgrep--result-count 2))
        (should-not (string-match-p "needle unrelated" (buffer-string)))
        (should-not (string-match-p (if (eq scope 'summaries)
                                       "needle transcript" "needle summary")
                                   (buffer-string)))
        (deadgrep-restart)
        (agent-recall-test--wait-for-deadgrep)
        (should (= deadgrep--result-count 2))))))

(ert-deftest agent-recall-deadgrep-deferred-search-retains-scope ()
  "A prefix-postponed search retains arguments until its first real start."
  (agent-recall-test--with-deadgrep
    (let ((current-prefix-arg '(4)))
      (agent-recall-search-summaries "needle"))
    (setq results-buffer (current-buffer))
    (should-not (get-buffer-process results-buffer))
    (let ((noninteractive nil))
      (call-interactively #'deadgrep-restart))
    (agent-recall-test--wait-for-deadgrep)
    (should (= deadgrep--result-count 2))
    (should-not (string-match-p "needle transcript" (buffer-string)))))

(ert-deftest agent-recall-deadgrep-skips-missing-indexed-directories ()
  "Stale index entries must not retain or recreate dangling search symlinks."
  (agent-recall-test--with-deadgrep
    (let* ((missing-dir (expand-file-name "workspace/.agent-shell/transcripts" root))
           (search-dir (expand-file-name "search" (file-name-directory agent-recall-index-file)))
           (stale-link (expand-file-name "workspace" search-dir)))
      (puthash (expand-file-name "missing.md" missing-dir)
               (list :dir missing-dir :project "workspace") agent-recall--index)
      (make-directory search-dir t)
      (make-symbolic-link missing-dir stale-link)
      (agent-recall-search "needle")
      (setq results-buffer (current-buffer))
      (agent-recall-test--wait-for-deadgrep)
      (should (= deadgrep--result-count 2))
      (should-not (file-symlink-p stale-link))
      ;; Refreshing again must not recreate the dangling link.
      (agent-recall--ensure-symlink-dir)
      (should-not (file-symlink-p stale-link))
      (should (gethash (expand-file-name "missing.md" missing-dir)
                       agent-recall--index)))))

(provide 'test-deadgrep)
;;; test-deadgrep.el ends here
