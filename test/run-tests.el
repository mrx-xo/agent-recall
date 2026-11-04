;;; run-tests.el --- Batch ERT entry point -*- lexical-binding: t; -*-

;;; Commentary:
;; Run from the repository root with dependencies installed:
;; emacs -Q --batch -l test/run-tests.el

;;; Code:

(require 'package)
(package-initialize)
(require 'ert)

(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (root (file-name-directory (directory-file-name test-dir))))
  (add-to-list 'load-path root)
  (require 'agent-recall)
  (dolist (file (directory-files test-dir t "^test-.*\\.el$"))
    (load file nil t)))

(ert-run-tests-batch-and-exit)

;;; run-tests.el ends here
