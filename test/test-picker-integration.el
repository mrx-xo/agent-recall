;;; test-picker-integration.el --- Real Consult/Vertico picker test -*- lexical-binding: t; -*-

;;; Commentary:
;; This test drives a real minibuffer with timers.  Run it in an interactive,
;; pseudo-graphical, or graphical Emacs with Consult and Vertico installed.

;;; Code:

(require 'ert)
(require 'agent-recall)

(defvar vertico--candidates)
(defvar vertico--index)
(defvar vertico--scroll)
(defvar vertico--lock-candidate)
(defvar agent-recall-consult--picker-lookup)
(declare-function vertico--exhibit "vertico")
(declare-function vertico--goto "vertico" (index))
(declare-function agent-recall-consult--current-candidate "agent-recall-consult")
(declare-function agent-recall-consult--final-accept "agent-recall-consult")
(declare-function agent-recall-consult--visit "agent-recall-consult")

(defun agent-recall-test--picker-state (picker)
  "Return the user-visible Vertico state from PICKER."
  (with-current-buffer picker
    (list :input (minibuffer-contents-no-properties)
          :candidates (mapcar #'substring-no-properties vertico--candidates)
          :index vertico--index
          :scroll vertico--scroll
          :selected (when-let ((candidate
                                (agent-recall-consult--current-candidate)))
                      (substring-no-properties candidate))
          :lock vertico--lock-candidate)))

(ert-deftest test-picker-real-consult-vertico-suspend-roundtrip ()
  "A real suspended picker should resume with byte-for-byte Vertico state."
  (skip-unless (and (not noninteractive)
                    (require 'consult nil t)
                    (require 'vertico nil t)
                    (require 'vertico-suspend nil t)
                    (require 'agent-recall-consult nil t)))
  (let* ((root (make-temp-file "agent-recall-integration-" t))
         (transcript-dir
          (expand-file-name "project/.agent-shell/transcripts" root))
         (agent-recall--index (make-hash-table :test 'equal))
         (agent-recall--index-loaded-p t)
         (agent-recall-browse-preview t)
         (agent-recall-auto-transcript-mode t)
         (agent-recall-browse-sort 'date-asc)
         (agent-recall-show-provider-icons nil)
         (was-vertico-mode (bound-and-true-p vertico-mode))
         (old-f12 (lookup-key global-map [f12]))
         (old-f11 (lookup-key global-map [f11]))
         (old-f10 (lookup-key global-map [f10]))
         picker before after selected-file preview-buffer preview-buffer-name
         preview-active finished)
    (make-directory transcript-dir t)
    (dotimes (index 40)
      (let* ((file (expand-file-name
                    (format "2026-08-24-10-00-%02d.md" index)
                    transcript-dir)))
        (with-temp-file file
          (insert (format "# Item %02d\n\n## User\nitem %02d\n" index index)))
        (puthash file
                 (list :project "project"
                       :dir transcript-dir
                       :timestamp "2026-08-24-10-00-00"
                       :preview (format "item %02d" index))
                 agent-recall--index)))
    (unwind-protect
        (progn
          (vertico-mode 1)
          (switch-to-buffer (get-buffer-create " *agent-recall-before-picker*"))
          (global-set-key
           [f12]
           (lambda ()
             (interactive)
             (delete-minibuffer-contents)
             (insert "[project]")
             (vertico--exhibit)
             (vertico--goto 12)
             (vertico--exhibit)
             ;; Let Consult's post-command preview action run before visiting.
             (setq unread-command-events '(f10))))
          (global-set-key
           [f10]
           (lambda ()
             (interactive)
             (sit-for 0.2)
             (setq preview-buffer
                   (window-buffer (minibuffer-selected-window))
                   preview-buffer-name (buffer-name preview-buffer)
                   preview-active
                   (and (buffer-live-p preview-buffer)
                        (null (buffer-local-value
                               'buffer-file-name preview-buffer))))
             (setq before (agent-recall-test--picker-state picker))
             (let* ((current (agent-recall-consult--current-candidate))
                    (resolved
                     (gethash (agent-recall--candidate-key current)
                              agent-recall-consult--picker-lookup)))
               (setq selected-file (agent-recall--candidate-file resolved)))
             (agent-recall-consult--visit)
             (setq unread-command-events '(f11))))
          (global-set-key
           [f11]
           (lambda ()
             (interactive)
             ;; Allow the zero-delay preview-promotion callback to run.
             (sit-for 0.1)
             (let* ((session
                     (buffer-local-value
                      'agent-recall--picker-navigation-session picker))
                    (transcript
                     (and session
                          (agent-recall--navigation-session-transcript-buffer
                           session))))
               (when (buffer-live-p transcript)
                 (with-current-buffer transcript
                   (let ((inhibit-read-only t))
                     (goto-char (point-max))
                     (insert "\nmtime changed while picker was suspended\n")
                     (save-buffer))
                   (agent-recall-browse-from-transcript))
                 (setq after (agent-recall-test--picker-state picker)
                       finished t)
                 (with-selected-window (active-minibuffer-window)
                   (agent-recall-consult--final-accept))))))
          (setq unread-command-events '(f12))
          (minibuffer-with-setup-hook
              (lambda ()
                (setq picker (current-buffer)))
            (with-timeout (5 (ert-fail "Timed out driving the suspended picker"))
              (agent-recall-browse)))
          (should finished)
          (should (equal before after))
          (should (equal selected-file
                         (agent-recall--canonical-file (buffer-file-name))))
          (should preview-active)
          (should (string-prefix-p " Preview:" preview-buffer-name))
          (should-not (buffer-live-p preview-buffer))
          (should (eq (get-file-buffer selected-file) (current-buffer)))
          (should agent-recall-transcript-mode))
      (define-key global-map [f12] old-f12)
      (define-key global-map [f11] old-f11)
      (define-key global-map [f10] old-f10)
      (mapc (lambda (file)
              (when-let ((buffer (get-file-buffer file)))
                (kill-buffer buffer)))
            (hash-table-keys agent-recall--index))
      (when (get-buffer " *agent-recall-before-picker*")
        (kill-buffer " *agent-recall-before-picker*"))
      (unless was-vertico-mode (vertico-mode -1))
      (delete-directory root t))))

(ert-deftest test-picker-real-transcript-quit-aborts-picker ()
  "Transcript `q' lifecycle should not leave an active hidden minibuffer."
  (skip-unless (and (not noninteractive)
                    (require 'consult nil t)
                    (require 'vertico nil t)
                    (require 'vertico-suspend nil t)
                    (require 'agent-recall-consult nil t)))
  (let* ((root (make-temp-file "agent-recall-quit-" t))
         (transcript-dir
          (expand-file-name "project/.agent-shell/transcripts" root))
         (file (expand-file-name "2026-08-24-10-00-00.md" transcript-dir))
         (agent-recall--index (make-hash-table :test 'equal))
         (agent-recall--index-loaded-p t)
         (agent-recall-browse-preview nil)
         (agent-recall-auto-transcript-mode t)
         (agent-recall-show-provider-icons nil)
         (was-vertico-mode (bound-and-true-p vertico-mode))
         (old-f12 (lookup-key global-map [f12]))
         (old-f11 (lookup-key global-map [f11]))
         picker visited caught-quit)
    (make-directory transcript-dir t)
    (with-temp-file file (insert "# Transcript\n\n## User\nhello\n"))
    (puthash file
             (list :project "project"
                   :dir transcript-dir
                   :timestamp "2026-08-24-10-00-00"
                   :preview "hello")
             agent-recall--index)
    (unwind-protect
        (progn
          (vertico-mode 1)
          (switch-to-buffer (get-buffer-create " *agent-recall-before-quit*"))
          (global-set-key
           [f12]
           (lambda ()
             (interactive)
             (vertico--exhibit)
             (vertico--goto 0)
             (agent-recall-consult--visit)
             (setq unread-command-events '(f11))))
          (global-set-key
           [f11]
           (lambda ()
             (interactive)
             (sit-for 0.1)
             (let* ((session
                     (buffer-local-value
                      'agent-recall--picker-navigation-session picker))
                    (transcript
                     (and session
                          (agent-recall--navigation-session-transcript-buffer
                           session))))
               (when (buffer-live-p transcript)
                 (setq visited t)
                 (with-current-buffer transcript
                   (agent-recall-quit-transcript))))))
          (setq unread-command-events '(f12))
          (minibuffer-with-setup-hook
              (lambda () (setq picker (current-buffer)))
            (condition-case nil
                (with-timeout (5 (ert-fail "Timed out aborting picker from transcript"))
                  (agent-recall-browse))
              (quit (setq caught-quit t))))
          (should visited)
          (should caught-quit)
          (should-not (active-minibuffer-window))
          (should-not agent-recall--navigation-sessions)
          (should (eq (window-buffer (selected-window))
                      (get-buffer " *agent-recall-before-quit*"))))
      (define-key global-map [f12] old-f12)
      (define-key global-map [f11] old-f11)
      (when-let ((buffer (get-file-buffer file))) (kill-buffer buffer))
      (when (get-buffer " *agent-recall-before-quit*")
        (kill-buffer " *agent-recall-before-quit*"))
      (unless was-vertico-mode (vertico-mode -1))
      (delete-directory root t))))

(ert-deftest test-picker-real-control-g-restores-pre-picker-window ()
  "A real `C-g' unwind should restore windows and remove session state."
  (skip-unless (and (not noninteractive)
                    (require 'consult nil t)
                    (require 'vertico nil t)
                    (require 'vertico-suspend nil t)
                    (require 'agent-recall-consult nil t)))
  (let* ((root (make-temp-file "agent-recall-control-g-" t))
         (transcript-dir
          (expand-file-name "project/.agent-shell/transcripts" root))
         (file (expand-file-name "2026-08-24-10-00-00.md" transcript-dir))
         (agent-recall--index (make-hash-table :test 'equal))
         (agent-recall--index-loaded-p t)
         (agent-recall-browse-preview t)
         (agent-recall-auto-transcript-mode t)
         (agent-recall-show-provider-icons nil)
         (was-vertico-mode (bound-and-true-p vertico-mode))
         (old-f12 (lookup-key global-map [f12]))
         (old-f10 (lookup-key global-map [f10]))
         preview-active caught-quit)
    (make-directory transcript-dir t)
    (with-temp-file file (insert "# Transcript\n\n## User\nhello\n"))
    (puthash file
             (list :project "project"
                   :dir transcript-dir
                   :timestamp "2026-08-24-10-00-00"
                   :preview "hello")
             agent-recall--index)
    (unwind-protect
        (progn
          (vertico-mode 1)
          (switch-to-buffer (get-buffer-create " *agent-recall-before-c-g*"))
          (global-set-key
           [f12]
           (lambda ()
             (interactive)
             (vertico--exhibit)
             (vertico--goto 0)
             (setq unread-command-events '(f10))))
          (global-set-key
           [f10]
           (lambda ()
             (interactive)
             (sit-for 0.2)
             (let* ((buffer
                     (window-buffer (minibuffer-selected-window)))
                    (name (buffer-name buffer)))
               (setq preview-active
                     (and name (string-prefix-p " Preview:" name))))
             (call-interactively (key-binding (kbd "C-g")))))
          (setq unread-command-events '(f12))
          (condition-case nil
              (with-timeout (5 (ert-fail "Timed out aborting picker with C-g"))
                (agent-recall-browse))
            (quit (setq caught-quit t)))
          (should preview-active)
          (should caught-quit)
          (should-not (active-minibuffer-window))
          (should-not agent-recall--navigation-sessions)
          (should (eq (window-buffer (selected-window))
                      (get-buffer " *agent-recall-before-c-g*")))
          (should-not (get-file-buffer file)))
      (define-key global-map [f12] old-f12)
      (define-key global-map [f10] old-f10)
      (when-let ((buffer (get-file-buffer file))) (kill-buffer buffer))
      (when (get-buffer " *agent-recall-before-c-g*")
        (kill-buffer " *agent-recall-before-c-g*"))
      (unless was-vertico-mode (vertico-mode -1))
      (delete-directory root t))))

(ert-deftest test-picker-real-quit-keeps-layout-after-resume ()
  "Quit a real suspended picker without moving a resumed chat's pane."
  (skip-unless (and (not noninteractive)
                    (require 'consult nil t)
                    (require 'vertico nil t)
                    (require 'vertico-suspend nil t)
                    (require 'agent-recall-consult nil t)))
  (let* ((file (make-temp-file "recall-quit-" nil ".md" "# Transcript\n"))
         (origin (generate-new-buffer " *recall-quit-origin*"))
         (chat (generate-new-buffer " *recall-quit-resumed*"))
         (agent-recall-browse-preview t)
         (agent-recall-auto-transcript-mode t)
         (old-f12 (lookup-key global-map [f12]))
         (old-f11 (lookup-key global-map [f11]))
         (was-vertico (bound-and-true-p vertico-mode))
         expected chat-window caught-quit)
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer origin)
          (vertico-mode 1)
          (global-set-key [f12]
                          (lambda ()
                            (interactive)
                            (vertico--exhibit)
                            (agent-recall-consult--visit)
                            (setq unread-command-events '(f11))))
          (global-set-key
           [f11]
           (lambda ()
             (interactive)
             (sit-for 0.1)
             (let* ((session (car agent-recall--navigation-sessions))
                    (transcript (agent-recall--navigation-session-transcript-buffer
                                 session))
                    (transcript-window (get-buffer-window transcript)))
               (unless transcript-window (ert-fail "Transcript did not open"))
               (select-window transcript-window)
               (setq-local agent-recall--transcript-session-id "test-session")
               ;; No provider process: resume an already-live test chat through
               ;; the real resume command and display path.
               (let ((display-buffer-alist nil)
                     (display-buffer-overriding-action
                      '(display-buffer-pop-up-window)))
                 (cl-letf (((symbol-function 'agent-recall--find-session-buffer)
                            (lambda (_) chat)))
                   (agent-recall-resume-current)))
               (setq chat-window (get-buffer-window chat))
               (select-window transcript-window)
               (setq expected (mapcar #'window-edges (window-list nil 'nomini)))
               (agent-recall-quit-transcript))))
          (setq unread-command-events '(f12))
          (condition-case nil
              (with-timeout (5 (ert-fail "Timed out quitting suspended picker"))
                (agent-recall-consult--browse-read
                 (list (agent-recall--make-candidate "one" file nil 'browse))
                 #'ignore))
            (quit (setq caught-quit t)))
          (should caught-quit)
          (should-not (active-minibuffer-window))
          (should-not agent-recall--navigation-sessions)
          (should (equal expected (mapcar #'window-edges (window-list nil 'nomini))))
          (should (eq (window-buffer chat-window) chat))
          (should-not (get-buffer-window (get-file-buffer file))))
      (define-key global-map [f12] old-f12)
      (define-key global-map [f11] old-f11)
      (when-let ((buffer (get-file-buffer file))) (kill-buffer buffer))
      (kill-buffer origin)
      (kill-buffer chat)
      (delete-file file)
      (unless was-vertico (vertico-mode -1)))))

(provide 'test-picker-integration)
;;; test-picker-integration.el ends here
