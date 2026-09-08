;;; test-transcript-mode-evil.el --- Evil bindings in transcript-mode -*- lexical-binding: t; -*-

;;; Commentary:
;; `global-agent-recall-transcript-mode' enables transcript-mode from
;; `after-change-major-mode-hook', which can run before evil has
;; initialized the buffer.  Enabling the mode must not depend on evil's
;; buffer-local state, and the normal-state keys must still resolve once
;; evil is active.
;; Needs evil and agent-shell loadable, so run against a full config:
;;   emacs --batch -l ~/.emacs.d/init.el \
;;     -l test/test-transcript-mode-evil.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)

;; Batch runs from a full init: flush Elpaca's queues so evil and
;; agent-shell are actually on the load path before requiring them.
(when noninteractive
  (when (fboundp 'elpaca-process-queues) (elpaca-process-queues))
  (when (fboundp 'elpaca-wait) (elpaca-wait)))

(require 'evil)
(require 'agent-recall)

(defmacro with-transcript-test-file (&rest body)
  "Run BODY with `file' bound to a fresh transcript under .agent-shell/transcripts."
  (declare (indent 0))
  `(let* ((root (make-temp-file "agent-recall-evil-" t))
          (dir (expand-file-name ".agent-shell/transcripts" root))
          (file (expand-file-name "2026-01-01-00-00-00.md" dir)))
     (make-directory dir t)
     (with-temp-file file (insert "# Transcript\n\n**User:** hi\n"))
     (unwind-protect
         (progn ,@body)
       (when-let ((buf (get-file-buffer file)))
         (with-current-buffer buf (set-buffer-modified-p nil))
         (kill-buffer buf))
       (delete-directory root t))))

(defmacro with-global-transcript-mode (&rest body)
  "Run BODY with `global-agent-recall-transcript-mode' on, restoring it after."
  (declare (indent 0))
  `(let ((was-on (bound-and-true-p global-agent-recall-transcript-mode)))
     (global-agent-recall-transcript-mode 1)
     (unwind-protect (progn ,@body)
       (unless was-on (global-agent-recall-transcript-mode -1)))))

(ert-deftest test-transcript-mode-global-enable-does-not-break-evil ()
  "Visiting a transcript with the globalized mode on must leave evil initialized.
The globalized mode runs from `after-change-major-mode-hook' ahead of
evil's own enable-in-buffer hook; transcript-mode must not touch evil's
buffer-local maps there, or the error aborts the hook chain and the
buffer is left without evil and in `fundamental-mode'."
  (should (bound-and-true-p evil-mode))
  (with-global-transcript-mode
    (with-transcript-test-file
      (with-current-buffer (find-file-noselect file)
        (should agent-recall-transcript-mode)
        (should (bound-and-true-p evil-local-mode))
        (should-not (eq major-mode 'fundamental-mode))))))

(ert-deftest test-transcript-mode-normal-state-keys-resolve ()
  "In normal state, transcript keys bind to the transcript commands."
  (with-transcript-test-file
    (with-current-buffer (find-file-noselect file)
      (agent-recall-transcript-mode 1)
      (evil-local-mode 1)
      (evil-normal-state)
      (should (eq (key-binding (kbd "r")) #'agent-recall-resume-current))
      (should (eq (key-binding (kbd "q")) #'agent-recall-quit-transcript))
      (should (eq (key-binding (kbd "C-j")) #'agent-recall-next-user-message))
      (should (eq (key-binding (kbd "gk")) #'agent-recall-prev-user-message))
      (should (eq (key-binding (kbd "b")) #'agent-recall-browse-from-transcript))
      ;; `s' is evil-snipe in the user's config; the mode map must win.
      (should (eq (key-binding (kbd "s")) #'agent-recall-catalogue)))))

(provide 'test-transcript-mode-evil)
;;; test-transcript-mode-evil.el ends here
