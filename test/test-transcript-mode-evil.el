;;; test-transcript-mode-evil.el --- Evil bindings in transcript-mode -*- lexical-binding: t; -*-

;;; Commentary:
;; `global-agent-recall-transcript-mode' enables transcript-mode from
;; `after-change-major-mode-hook', which can run before evil has
;; initialized the buffer.  Enabling the mode must not depend on evil's
;; buffer-local state, and the normal-state keys must still resolve once
;; evil is active.
;; Requires Evil and agent-shell on the load path, but no personal init file.

;;; Code:

(require 'ert)

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
  `(let ((was-on (bound-and-true-p global-agent-recall-transcript-mode))
         (evil-was-on (bound-and-true-p evil-mode))
         (auto-mode-alist (cons '("\\.md\\'" . text-mode) auto-mode-alist)))
     (evil-mode 1)
     (global-agent-recall-transcript-mode 1)
     (unwind-protect (progn ,@body)
       (unless was-on (global-agent-recall-transcript-mode -1))
       (unless evil-was-on (evil-mode -1)))))

(ert-deftest test-transcript-mode-activates-keys-with-late-evil ()
  "Bindings must work immediately when Evil is enabled after Agent Recall."
  (with-transcript-test-file
    (with-current-buffer (find-file-noselect file)
      (agent-recall-transcript-mode 1)
      (evil-local-mode 1)
      (should (eq (key-binding (kbd "r")) #'agent-recall-resume-current))
      (should (eq (key-binding (kbd "q")) #'agent-recall-quit-transcript)))))

(ert-deftest test-transcript-mode-global-enable-does-not-break-evil ()
  "Visiting a transcript with the globalized mode on must leave evil initialized.
The globalized mode runs from `after-change-major-mode-hook' ahead of
evil's own enable-in-buffer hook; transcript-mode must not touch evil's
buffer-local maps there, or the error aborts the hook chain and the
buffer is left without evil and in `fundamental-mode'."
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

(ert-deftest test-transcript-mode-defaults-without-evil ()
  "Standard transcript actions remain available without Evil."
  (with-transcript-test-file
    (with-current-buffer (find-file-noselect file)
      (evil-local-mode -1)
      (agent-recall-transcript-mode 1)
      (dolist (binding '(("r" . agent-recall-resume-current)
                         ("R" . agent-recall-force-resume-current)
                         ("c" . agent-recall-clean-view)
                         ("b" . agent-recall-browse-from-transcript)
                         ("s" . agent-recall-catalogue)
                         ("q" . agent-recall-quit-transcript)
                         ("C-c C-n" . agent-recall-next-user-message)
                         ("C-c C-p" . agent-recall-prev-user-message)))
        (should (eq (key-binding (kbd (car binding))) (cdr binding)))))))

(ert-deftest test-transcript-mode-personal-overrides-update-header ()
  "User overrides survive mode activation and appear in the header."
  (let ((saved-map (copy-keymap agent-recall-transcript-mode-map)))
    (unwind-protect
        (progn
          (define-key agent-recall-transcript-mode-map (kbd "b") nil)
          (define-key agent-recall-transcript-mode-map (kbd "s") nil)
          (evil-define-key* 'normal agent-recall-transcript-mode-map
            (kbd "b") nil
            (kbd "s") nil
            (kbd "u") #'agent-recall-browse-from-transcript
            (kbd "a") #'agent-recall-catalogue)
          (with-transcript-test-file
            (with-current-buffer (find-file-noselect file)
              (evil-local-mode 1)
              (evil-normal-state)
              (agent-recall-transcript-mode 1)
              (should (eq (key-binding (kbd "u")) #'agent-recall-browse-from-transcript))
              (should (eq (key-binding (kbd "a")) #'agent-recall-catalogue))
              (should (eq (key-binding (kbd "b")) #'evil-backward-word-begin))
              (should (string-match-p "u Back" (agent-recall--header-line nil)))
              (should (string-match-p "a Save" (agent-recall--header-line nil))))))
      (setcdr agent-recall-transcript-mode-map (cdr saved-map)))))

(provide 'test-transcript-mode-evil)
;;; test-transcript-mode-evil.el ends here
