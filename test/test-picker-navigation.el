;;; test-picker-navigation.el --- Tests for persistent picker navigation -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests stable candidate payloads, deterministic ordering, origin dispatch,
;; and the isolated Consult/Vertico session validator.

;;; Code:

(require 'ert)
(require 'agent-recall)

(defvar agent-recall-consult--picker-map)
(defvar agent-recall-consult--read-lookup)
(defvar agent-recall-consult--identity-ids)
(defvar agent-recall-consult--next-identity-id)
(defvar agent-recall-consult-resumable-only)
(declare-function agent-recall-consult--encode-candidates
                  "agent-recall-consult" (candidates))
(declare-function agent-recall-consult--abort-session
                  "agent-recall-consult" (session))
(declare-function agent-recall-consult--final-accept
                  "agent-recall-consult" ())
(declare-function agent-recall-consult--position
                  "agent-recall-consult" (candidate &optional find-file))
(declare-function agent-recall-consult--search-fn
                  "agent-recall-consult" (input))
(declare-function agent-recall-consult--session-valid-p
                  "agent-recall-consult" (session))
(declare-function agent-recall-consult--resume-session
                  "agent-recall-consult" (session))
(declare-function agent-recall-consult--suspend-available-p
                  "agent-recall-consult" ())
(declare-function agent-recall-consult--suspendable-read
                  "agent-recall-consult" (kind table options lookup))
(declare-function agent-recall-consult--visit "agent-recall-consult" ())
(declare-function agent-recall-consult-search "agent-recall-consult" ())

(defmacro agent-recall-test--with-files (bindings &rest body)
  "Create temporary transcript files in BINDINGS, then evaluate BODY."
  (declare (indent 1))
  `(let ((directory (make-temp-file "agent-recall-navigation-" t)))
     (unwind-protect
         (let ,(mapcar
                (lambda (binding)
                  `(,(car binding)
                    (expand-file-name ,(cadr binding) directory)))
                bindings)
           ,@(mapcar
              (lambda (binding)
                `(with-temp-file ,(car binding)
                   (insert "# Transcript\n\n## User\nhello\n")))
              bindings)
           ,@body)
       (delete-directory directory t))))

(ert-deftest test-picker-candidate-duplicate-labels-have-distinct-identities ()
  "Duplicate labels should retain distinct canonical file payloads."
  (agent-recall-test--with-files ((first "one.md") (second "two.md"))
    (let* ((candidates
            (agent-recall--disambiguate-candidates
             (list (agent-recall--make-candidate "same" first nil 'browse)
                   (agent-recall--make-candidate "same" second nil 'browse))))
           (left (car candidates))
           (right (cadr candidates)))
      (should-not (equal (agent-recall--candidate-key left)
                         (agent-recall--candidate-key right)))
      (should (equal (agent-recall--canonical-file first)
                     (agent-recall--candidate-file left)))
      (should (equal (agent-recall--canonical-file second)
                     (agent-recall--candidate-file right)))
      (should (string-match-p (regexp-quote (file-name-nondirectory first))
                              (agent-recall--candidate-key left)))
      (should (string-match-p (regexp-quote (file-name-nondirectory second))
                              (agent-recall--candidate-key right))))))

(ert-deftest test-picker-search-payload-includes-file-and-line ()
  "Search payload identity should include its canonical file and line."
  (agent-recall-test--with-files ((file "result.md"))
    (let ((candidate (agent-recall--make-candidate "result" file 17 'search)))
      (should (equal (agent-recall--canonical-file file)
                     (agent-recall--candidate-file candidate)))
      (should (= 17 (agent-recall--candidate-line candidate)))
      (should (eq 'search (agent-recall--candidate-kind candidate)))
      (should (string-match-p "17"
                              (get-text-property
                               0 'agent-recall-identity candidate))))))

(ert-deftest test-picker-display-timestamp-includes-time-of-day ()
  "Browse timestamps should visibly distinguish sessions on the same day."
  (let ((display (agent-recall--display-timestamp "2026-08-24-13-14-15")))
    (should (string-match-p "13:14:15" display))))

(ert-deftest test-picker-sort-ties-use-canonical-path ()
  "Every Browse comparator should use the absolute path as its tie-breaker."
  (agent-recall-test--with-files ((first "a.md") (second "b.md"))
    (set-file-times first (seconds-to-time 1000))
    (set-file-times second (seconds-to-time 1000))
    (dolist (sort '(date-desc date-asc modified-desc modified-asc project))
      (let* ((agent-recall-browse-sort sort)
             (records (list (list "same" second "same" "project")
                            (list "same" first "same" "project")))
             (ordered (agent-recall--sort-transcript-records records)))
        (should (equal (agent-recall--canonical-file first)
                       (agent-recall--canonical-file (nth 1 (car ordered)))))))))

(ert-deftest test-picker-browse-and-project-lists-handle-duplicates ()
  "Browse and project Browse should map duplicate labels to the right files."
  (agent-recall-test--with-files ((first "one.md") (second "two.md"))
    (let ((agent-recall--index (make-hash-table :test 'equal))
          (agent-recall--index-loaded-p t)
          (agent-recall-browse-sort 'date-desc)
          (agent-recall-show-provider-icons nil))
      (dolist (file (list first second))
        (puthash file
                 '(:project "same" :timestamp "2026-08-24-10-00-00"
                   :preview "hello")
                 agent-recall--index))
      (dolist (transcripts
               (list (agent-recall--list-transcripts)
                     (agent-recall--list-transcripts-for-project "same")))
        (let ((candidates (agent-recall--browse-candidates transcripts)))
          (should (= 2 (length candidates)))
          (should-not
           (equal (agent-recall--candidate-key (car candidates))
                  (agent-recall--candidate-key (cadr candidates))))
          (should
           (equal (sort (mapcar #'agent-recall--candidate-file candidates)
                        #'string<)
                  (sort (mapcar #'agent-recall--canonical-file
                                (list first second))
                        #'string<))))))))

(ert-deftest test-picker-suspended-origin-dispatches-without-quitting-window ()
  "Back should resume a valid suspended origin without reopening Browse."
  (let (resumed)
    (with-temp-buffer
      (let ((session
             (agent-recall--navigation-session-create
              :id "session"
              :kind 'browse
              :backend 'suspended
              :state 'suspended
              :valid-function (lambda (_session) t)
              :resume-function (lambda (item) (setq resumed item)))))
        (agent-recall--navigation-attach session (current-buffer))
        (cl-letf (((symbol-function 'quit-window)
                   (lambda (&rest _) (ert-fail "quit-window was called")))
                  ((symbol-function 'agent-recall-browse)
                   (lambda () (ert-fail "Browse fallback was called"))))
          (agent-recall-browse-from-transcript))
        (should (eq resumed session))
        (should-not agent-recall--navigation-origins)))))

(ert-deftest test-picker-nested-origins-resume-last-in-first-out ()
  "Nested suspended origins should resume newest first."
  (let (order)
    (with-temp-buffer
      (let ((outer
             (agent-recall--navigation-session-create
              :id "outer" :kind 'browse :backend 'suspended :state 'suspended
              :valid-function (lambda (_session) t)
              :resume-function (lambda (_session) (push 'outer order))))
            (inner
             (agent-recall--navigation-session-create
              :id "inner" :kind 'search :backend 'suspended :state 'suspended
              :valid-function (lambda (_session) t)
              :resume-function (lambda (_session) (push 'inner order)))))
        (agent-recall--navigation-attach outer (current-buffer))
        (agent-recall--navigation-attach inner (current-buffer))
        (agent-recall-browse-from-transcript)
        (should (equal order '(inner)))
        (should (eq (car agent-recall--navigation-origins) outer))
        (agent-recall-browse-from-transcript)
        (should (equal order '(outer inner)))
        (should-not agent-recall--navigation-origins)))))

(ert-deftest test-picker-quit-aborts-linked-suspended-session ()
  "Transcript quit should abort and clean a linked suspended picker."
  (let (aborted)
    (with-temp-buffer
      (let (session)
        (setq session
              (agent-recall--navigation-session-create
               :id "session"
               :kind 'browse
               :backend 'suspended
               :state 'suspended
               :valid-function (lambda (_session) t)
               :abort-function
               (lambda (item)
                 (setq aborted item)
                 (agent-recall--navigation-cleanup item)
                 t)))
        (agent-recall--navigation-attach session (current-buffer))
        (cl-letf (((symbol-function 'quit-window)
                   (lambda (&rest _) (ert-fail "quit-window was called"))))
          (agent-recall-quit-transcript))
        (should (eq aborted session))
        (should (eq 'closed
                    (agent-recall--navigation-session-state session)))
        (should-not agent-recall--navigation-origins)))))

(ert-deftest test-picker-persistent-origin-restores-buffer-and-marker ()
  "Back should restore a persistent result buffer at its recorded marker."
  (let ((result (generate-new-buffer " *agent-recall-results*"))
        (transcript (generate-new-buffer " *agent-recall-transcript*")))
    (unwind-protect
        (save-window-excursion
          (with-current-buffer result (insert "one\ntwo\nthree\n"))
          (switch-to-buffer transcript)
          (let* ((marker (with-current-buffer result
                           (goto-char (point-min))
                           (forward-line 1)
                           (copy-marker (point))))
                 (target (marker-position marker))
                 (session
                  (agent-recall--navigation-session-create
                   :id "grep"
                   :kind 'grep
                   :backend 'persistent
                   :origin-window (selected-window)
                   :origin-buffer result
                   :origin-marker marker
                   :state 'transcript)))
            (agent-recall--navigation-attach session transcript)
            (with-current-buffer transcript
              (agent-recall-browse-from-transcript))
            (should (eq (window-buffer (selected-window)) result))
            (should (= target (with-current-buffer result (point))))
            (should (eq 'closed
                        (agent-recall--navigation-session-state session)))))
      (when (buffer-live-p result) (kill-buffer result))
      (when (buffer-live-p transcript) (kill-buffer transcript)))))

(ert-deftest test-picker-persistent-origin-attaches-to-existing-file-buffer ()
  "A grep result should link a transcript buffer that was already visiting its file."
  (agent-recall-test--with-files ((file "existing.md"))
    (let ((result (generate-new-buffer " *agent-recall-existing-results*"))
          (transcript (find-file-noselect file))
          (agent-recall-extra-transcript-dirs
           (list (list :dir directory :project "test")))
          session)
      (unwind-protect
          (save-window-excursion
            (switch-to-buffer result)
            (insert "first\nsecond\n")
            (goto-char (point-min))
            (forward-line 1)
            (agent-recall--mark-search-buffer 'grep)
            (agent-recall--prepare-search-origin)
            (switch-to-buffer transcript)
            (goto-char (point-max))
            (agent-recall--finish-pending-search-origin)
            (setq session (car agent-recall--navigation-origins))
            (should session)
            (should (eq 'persistent
                        (agent-recall--navigation-session-backend session)))
            (should (eq 'grep
                        (agent-recall--navigation-session-kind session)))
            (should (= 2
                       (with-current-buffer result
                         (line-number-at-pos
                          (agent-recall--navigation-session-origin-marker
                           session)))))
            (should (= (line-number-at-pos)
                       (agent-recall--navigation-session-selected-line session))))
        (when session (agent-recall--navigation-cleanup session))
        (agent-recall--clear-pending-search-origin)
        (when (buffer-live-p transcript) (kill-buffer transcript))
        (when (buffer-live-p result) (kill-buffer result))))))

(ert-deftest test-picker-minibuffer-quit-cleans-session-record ()
  "A quit unwinding Consult should remove its navigation session."
  (skip-unless (require 'agent-recall-consult nil t))
  (let ((before (copy-sequence agent-recall--navigation-sessions))
        caught)
    (cl-letf (((symbol-function 'consult--read)
               (lambda (&rest _) (signal 'quit nil))))
      (condition-case nil
          (agent-recall-consult--suspendable-read
           'browse '("candidate") '(:require-match t)
           (make-hash-table :test 'equal))
        (quit (setq caught t))))
    (should caught)
    (should (equal before agent-recall--navigation-sessions))))

(ert-deftest test-picker-transcript-kill-aborts-orphaned-picker ()
  "Killing a linked transcript should arrange picker cleanup."
  (let ((transcript (generate-new-buffer " *agent-recall-killed-transcript*"))
        aborted session)
    (unwind-protect
        (progn
          (setq session
                (agent-recall--navigation-new-session
                 'browse 'suspended
                 :abort-function
                 (lambda (item)
                   (setq aborted item)
                   (agent-recall--navigation-cleanup item)
                   t)))
          (setf (agent-recall--navigation-session-state session) 'suspended)
          (agent-recall--navigation-attach session transcript)
          (kill-buffer transcript)
          (when (timerp agent-recall--navigation-orphan-timer)
            (cancel-timer agent-recall--navigation-orphan-timer)
            (setq agent-recall--navigation-orphan-timer nil))
          (agent-recall--navigation-abort-orphan)
          (should (eq aborted session))
          (should (eq 'closed
                      (agent-recall--navigation-session-state session)))
          (should-not (memq session agent-recall--navigation-sessions)))
      (when (buffer-live-p transcript) (kill-buffer transcript))
      (when (and session
                 (not (eq 'closed
                          (agent-recall--navigation-session-state session))))
        (agent-recall--navigation-cleanup session)))))

(ert-deftest test-picker-frame-deletion-dispatches-session-abort ()
  "Deleting a picker frame should dispatch cleanup for its exact session."
  (let (aborted session)
    (setq session
          (agent-recall--navigation-new-session
           'browse 'suspended
           :origin-window (selected-window)
           :abort-function
           (lambda (item)
             (setq aborted item)
             (agent-recall--navigation-cleanup item)
             t)))
    (setf (agent-recall--navigation-session-state session) 'suspended)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_time _repeat function &rest arguments)
                 (apply function arguments))))
      (agent-recall--navigation-frame-deleted (selected-frame)))
    (should (eq aborted session))
    (should (eq 'closed (agent-recall--navigation-session-state session)))
    (should-not (memq session agent-recall--navigation-sessions))))

(ert-deftest test-picker-abort-errors-still-clean-session-records ()
  "A backend abort error should not leak its navigation session."
  (let ((session
         (agent-recall--navigation-new-session
          'browse 'suspended
          :abort-function (lambda (_session) (error "broken abort")))))
    (should-error (agent-recall--navigation-request-abort session))
    (should (eq 'closed (agent-recall--navigation-session-state session)))
    (should-not (memq session agent-recall--navigation-sessions))))

(ert-deftest test-picker-orphan-abort-waits-for-unrelated-minibuffer ()
  "Cleanup should wait rather than abort an unrelated nested minibuffer."
  (skip-unless (require 'agent-recall-consult nil t))
  (let ((picker (generate-new-buffer " *agent-recall-orphan-picker*"))
        (other (generate-new-buffer " *agent-recall-nested-picker*")))
    (unwind-protect
        (save-window-excursion
          (let* ((window (selected-window))
                 (session
                  (agent-recall--navigation-session-create
                   :id "orphan" :kind 'browse :backend 'suspended
                   :minibuffer picker :state 'suspended)))
            (set-window-buffer window other)
            (cl-letf (((symbol-function 'active-minibuffer-window)
                       (lambda () window)))
              (should-not (agent-recall-consult--abort-session session))
              (should (eq 'orphaned
                          (agent-recall--navigation-session-state session))))
            (cl-letf (((symbol-function 'active-minibuffer-window)
                       (lambda () nil)))
              (should (agent-recall-consult--abort-session session))
              (should (eq 'closed
                          (agent-recall--navigation-session-state session))))))
      (when (buffer-live-p picker) (kill-buffer picker))
      (when (buffer-live-p other) (kill-buffer other)))))

(ert-deftest test-picker-resume-race-requests-clean-abort ()
  "A picker that becomes stale during resume should enter the abort path."
  (skip-unless (require 'agent-recall-consult nil t))
  (let ((session
         (agent-recall--navigation-session-create
          :id "race" :kind 'browse :backend 'suspended :state 'suspended))
        aborted)
    (cl-letf (((symbol-function 'agent-recall-consult--session-valid-p)
               (lambda (_session) nil))
              ((symbol-function 'agent-recall--navigation-request-abort)
               (lambda (item) (setq aborted item))))
      (should-error (agent-recall-consult--resume-session session)
                    :type 'user-error))
    (should (eq aborted session))))

(ert-deftest test-picker-optional-path-does-not-load-vertico-suspend ()
  "A non-Vertico Consult path should not attempt to load suspension code."
  (skip-unless (require 'agent-recall-consult nil t))
  (let ((agent-recall-auto-transcript-mode t)
        (vertico-mode nil)
        required)
    (cl-letf (((symbol-function 'require)
               (lambda (feature &rest _)
                 (push feature required)
                 nil)))
      (should-not (agent-recall-consult--suspend-available-p)))
    (should-not (memq 'vertico-suspend required))))

(ert-deftest test-picker-command-map-uses-vertico-final-accept ()
  "Suspendable picker bindings should use visit and Vertico final accept."
  (skip-unless (require 'agent-recall-consult nil t))
  (should (eq (lookup-key agent-recall-consult--picker-map (kbd "RET"))
              #'agent-recall-consult--visit))
  (should (eq (lookup-key agent-recall-consult--picker-map (kbd "M-RET"))
              #'agent-recall-consult--final-accept)))

(ert-deftest test-picker-header-consistently-labels-back-action ()
  "The transcript header should label the return action as Back."
  (with-temp-buffer
    (setq-local agent-recall-transcript-mode t)
    (should (string-match-p "Back" (agent-recall--header-line nil)))
    ;; Unbound commands appear by name; "browse" in the command is not a label.
    (let ((case-fold-search nil))
      (should-not (string-match-p "Browse" (agent-recall--header-line nil))))
    (let ((session
           (agent-recall--navigation-session-create
            :id "session"
            :kind 'browse
            :backend 'suspended
            :state 'suspended
            :valid-function (lambda (_session) t))))
      (setq-local agent-recall--navigation-origins (list session))
      (should (string-match-p "Back" (agent-recall--header-line nil))))))

(ert-deftest test-picker-persistent-origin-validation-rejects-dead-marker ()
  "Persistent origins should require a live buffer-backed marker."
  (let ((origin (generate-new-buffer " *agent-recall-origin*")))
    (unwind-protect
        (let* ((marker (with-current-buffer origin (copy-marker (point-min))))
               (session
                (agent-recall--navigation-session-create
                 :id "persistent"
                 :kind 'grep
                 :backend 'persistent
                 :origin-buffer origin
                 :origin-marker marker
                 :state 'transcript)))
          (should (agent-recall--navigation-session-valid-p session))
          (set-marker marker nil)
          (should-not (agent-recall--navigation-session-valid-p session)))
      (when (buffer-live-p origin)
        (kill-buffer origin)))))

(ert-deftest test-picker-consult-identities-map-to-authoritative-payloads ()
  "Consult tofu identities should preserve duplicate candidate payloads."
  (skip-unless (require 'agent-recall-consult nil t))
  (agent-recall-test--with-files ((first "one.md") (second "two.md"))
    (let* ((agent-recall-consult--read-lookup
            (make-hash-table :test 'equal))
           (agent-recall-consult--identity-ids
            (make-hash-table :test 'equal))
           (agent-recall-consult--next-identity-id 0)
           (encoded
            (agent-recall-consult--encode-candidates
             (list (agent-recall--make-candidate "same" first nil 'browse)
                   (agent-recall--make-candidate "same" second nil 'browse)))))
      (should-not (equal (agent-recall--candidate-key (car encoded))
                         (agent-recall--candidate-key (cadr encoded))))
      (dolist (candidate encoded)
        (let ((resolved
               (gethash (agent-recall--candidate-key candidate)
                        agent-recall-consult--read-lookup)))
          (should (equal (agent-recall--candidate-file candidate)
                         (agent-recall--candidate-file resolved))))))))

(ert-deftest test-picker-aggregated-search-retains-matched-line ()
  "Aggregated search candidates should preview and visit their matched line."
  (skip-unless (and (require 'agent-recall-consult nil t)
                    (executable-find agent-recall-rg-executable)))
  (agent-recall-test--with-files ((file "2026-08-24-10-00-00.md"))
    (with-temp-file file
      (insert "first\nsecond\nneedle here\nfourth\n"))
    (let ((agent-recall--index (make-hash-table :test 'equal))
          (agent-recall--index-loaded-p t)
          (agent-recall-consult-resumable-only nil)
          opened)
      (puthash file
               (list :project "project"
                     :dir (file-name-directory file)
                     :timestamp "2026-08-24-10-00-00")
               agent-recall--index)
      (let* ((candidate (car (agent-recall-consult--search-fn "needle")))
             (position
              (agent-recall-consult--position
               candidate
               (lambda (path)
                 (setq opened (find-file-noselect path))))))
        (unwind-protect
            (progn
              (should candidate)
              (should (= 3 (agent-recall--candidate-line candidate)))
              (should (equal (agent-recall--canonical-file file)
                             (agent-recall--candidate-file candidate)))
              (should (= 3 (with-current-buffer opened
                             (line-number-at-pos (car position))))))
          (when (buffer-live-p opened) (kill-buffer opened)))))))

(ert-deftest test-picker-aggregated-search-uses-suspendable-adapter ()
  "Aggregated Consult search should opt into the shared search session path."
  (skip-unless (require 'agent-recall-consult nil t))
  (let (called)
    (cl-letf (((symbol-function 'agent-recall-consult--ensure-consult) #'ignore)
              ((symbol-function 'agent-recall--index-dirs)
               (lambda () '("/tmp")))
              ((symbol-function 'agent-recall-consult--suspend-available-p)
               (lambda () t))
              ((symbol-function 'consult--dynamic-collection)
               (lambda (function) function))
              ((symbol-function 'agent-recall-consult--state)
               (lambda () #'ignore))
              ((symbol-function 'agent-recall-consult--suspendable-read)
               (lambda (kind _table _options lookup)
                 (setq called (list kind (hash-table-p lookup)))
                 nil)))
      (agent-recall-consult-search))
    (should (equal called '(search t)))))

(ert-deftest test-picker-consult-session-validation-is-exact ()
  "Consult validation should reject unrelated, unsuspended, and dead pickers."
  (skip-unless (and (require 'agent-recall-consult nil t)
                    (require 'vertico-suspend nil t)))
  (let ((picker (generate-new-buffer " *agent-recall-picker*"))
        (other (generate-new-buffer " *agent-recall-other-picker*")))
    (unwind-protect
        (save-window-excursion
          (let* ((window (selected-window))
                 (session
                  (agent-recall--navigation-session-create
                   :id "consult"
                   :kind 'browse
                   :backend 'suspended
                   :minibuffer picker
                   :state 'suspended)))
            (set-window-buffer window picker)
            (with-current-buffer picker
              (setq-local agent-recall--picker-navigation-session session)
              (setq-local vertico--input t)
              (setq-local vertico-suspend--ov
                          (make-overlay (point-min) (point-max))))
            (cl-letf (((symbol-function 'active-minibuffer-window)
                       (lambda () window))
                      ((symbol-function 'minibufferp)
                       ;; Emacs 30's `minibufferp' takes (BUFFER LIVE);
                       ;; evil's window advice calls it with both.
                       (lambda (&optional buffer _live) (eq buffer picker))))
              (should (agent-recall-consult--session-valid-p session))
              (with-current-buffer picker
                (delete-overlay vertico-suspend--ov)
                (setq vertico-suspend--ov nil))
              (should-not (agent-recall-consult--session-valid-p session))
              (with-current-buffer picker
                (setq-local vertico-suspend--ov
                            (make-overlay (point-min) (point-max)))
                (setq-local agent-recall--picker-navigation-session
                            (agent-recall--navigation-session-create
                             :id "other" :backend 'suspended)))
              (should-not (agent-recall-consult--session-valid-p session))
              (with-current-buffer picker
                (setq-local agent-recall--picker-navigation-session session))
              (set-window-buffer window other)
              (should-not (agent-recall-consult--session-valid-p session))))
          (kill-buffer picker)
          (should-not (buffer-live-p picker)))
      (when (buffer-live-p picker) (kill-buffer picker))
      (when (buffer-live-p other) (kill-buffer other)))))

(provide 'test-picker-navigation)
;;; test-picker-navigation.el ends here
