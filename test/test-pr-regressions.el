;;; test-pr-regressions.el --- GitHub contribution regressions -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise header boundaries and index/search synchronization with real files.

;;; Code:

(require 'ert)
(require 'agent-recall)

(defmacro agent-recall-test--with-index (&rest body)
  "Run BODY with an isolated index and temporary transcript directory."
  (declare (indent 0))
  `(let* ((root (make-temp-file "agent-recall-pr-" t))
          (agent-recall-index-file (expand-file-name "index.el" root))
          (agent-recall--index (make-hash-table :test 'equal))
          (agent-recall--index-loaded-p t)
          (agent-recall--symlink-dir nil))
     (unwind-protect (progn ,@body)
       (delete-directory root t))))

(ert-deftest test-session-title-does-not-replace-conversation-text ()
  (dolist (fixture '((".md" . "# Transcript\n\n---\n\n## User\n**Title:** quoted example\n")
                     (".org" . "#+TITLE: Transcript\n\n* User\n#+PROPERTY: Session_Title quoted example\n")))
    (agent-recall-test--with-index
      (let ((file (expand-file-name (concat "chat" (car fixture)) root)))
        (with-temp-file file (insert (cdr fixture)))
        (should-not (agent-recall--read-session-title file))
        (agent-recall--write-session-title-to-file file "Actual title")
        (should (equal "Actual title" (agent-recall--read-session-title file)))
        (with-temp-buffer
          (insert-file-contents file)
          (should (search-forward "quoted example" nil t)))))))

(ert-deftest test-session-title-blank-first-line-is-no-op ()
  (dolist (title '(" " "\nsecond line" "\r\nsecond line"))
    (agent-recall-test--with-index
      (let ((file (expand-file-name "chat.md" root))
            (original "# Transcript\n\n---\n\n## User\nHello\n"))
        (with-temp-file file (insert original))
        (agent-recall--write-session-title-to-file file title)
        (with-temp-buffer
          (insert-file-contents file)
          (should (equal original (buffer-string))))))))

(ert-deftest test-session-title-org-without-title-keeps-first-property ()
  (agent-recall-test--with-index
    (let ((file (expand-file-name "chat.org" root)))
      (with-temp-file file
        (insert "#+PROPERTY: Session abc\n\n* User\nHello\n"))
      (agent-recall--write-session-title-to-file file "New title")
      (should (equal "abc" (agent-recall--org-read-property file "Session")))
      (should (equal "New title" (agent-recall--read-session-title file))))))

(ert-deftest test-session-title-long-header-round-trip ()
  (agent-recall-test--with-index
    (let ((file (expand-file-name "chat.md" root))
          (title (make-string 3100 ?a)))
      (with-temp-file file (insert "# Transcript\n\n---\n\n## User\nHello\n"))
      (agent-recall--write-session-title-to-file file title)
      (should (equal title (agent-recall--read-session-title file))))))

(ert-deftest test-session-id-does-not-truncate-invalid-token ()
  (agent-recall-test--with-index
    (let ((file (expand-file-name "chat.md" root)))
      (dolist (id '("ses_good-bad" "ses_" "12345678-1234-1234-1234-123456789abcEXTRA"))
        (with-temp-file file (insert "**Session ID:** " id "\n\n---\n"))
        (should-not (agent-recall--read-embedded-session-id file))))))

(ert-deftest test-session-id-does-not-truncate-at-read-limit ()
  (agent-recall-test--with-index
    (let ((file (expand-file-name "chat.md" root)))
      (with-temp-file file
        (insert (make-string 1474 ?x) "\n**Session ID:** ses_AbC1234567890\n\n---\n"))
      (let ((id (agent-recall--read-embedded-session-id file)))
        (should (or (null id) (equal id "ses_AbC1234567890")))))))

(ert-deftest test-session-id-ignores-conversation-examples ()
  (dolist (fixture '((".md" . "# Transcript\n\n---\n\n## User\n**Session ID:** ses_Example\n")
                     (".org" . "#+TITLE: Transcript\n\n* User\n#+PROPERTY: Session ses_Example\n")))
    (agent-recall-test--with-index
      (let ((file (expand-file-name (concat "chat" (car fixture)) root)))
        (with-temp-file file (insert (cdr fixture)))
        (should-not (agent-recall--read-embedded-session-id file))))))

(ert-deftest test-index-save-refreshes-search-projects ()
  (agent-recall-test--with-index
    (let* ((dir (expand-file-name "project/.agent-shell/transcripts" root))
           (file (expand-file-name "chat.md" dir)))
      (make-directory dir t)
      (with-temp-file file (insert "# Transcript\n\n## User\nsearchable\n"))
      (agent-recall--index-add file)
      (should (file-exists-p (expand-file-name "project/chat.md" agent-recall--symlink-dir)))
      (clrhash agent-recall--index)
      (agent-recall--index-save)
      (should-not (file-exists-p (expand-file-name "project/chat.md" agent-recall--symlink-dir))))))

(ert-deftest test-index-save-preserves-existing-search-directory ()
  (agent-recall-test--with-index
    (let* ((search (expand-file-name "search" root))
           (keep (expand-file-name "keep.txt" search)))
      (make-directory search)
      (with-temp-file keep (insert "unrelated file"))
      (let ((inode (file-attribute-inode-number (file-attributes search))))
        (agent-recall--index-save)
        (should (file-exists-p keep))
        (should (equal inode (file-attribute-inode-number (file-attributes search))))))))

(ert-deftest test-session-title-event-refreshes-index-and-picker ()
  (agent-recall-test--with-index
    (let* ((dir (expand-file-name "project/.agent-shell/transcripts" root))
           (file (expand-file-name "2026-10-27-10-00-00.md" dir))
           (agent-recall-capture-functions nil)
           (agent-recall-show-provider-icons nil)
           (agent-recall--metadata (make-hash-table :test 'equal))
           (agent-recall--metadata-loaded-p t))
      (make-directory dir t)
      (with-temp-file file (insert "# Transcript\n\n---\n\n## User\nHello\n"))
      (with-temp-buffer
        (setq major-mode 'agent-shell-mode)
        (setq-local agent-shell--state
                    (agent-shell--make-state :buffer (current-buffer)))
        (setq-local agent-shell--transcript-file file)
        (map-put! (map-elt agent-shell--state :session) :id "ses_abc123")
        (agent-recall-track-sessions)
        (dolist (title '("Initial title" "Updated title"))
          (setf (map-elt (map-elt agent-shell--state :session) :title) title)
          (agent-shell--emit-event :event 'session-title-changed)
          (should (equal title (plist-get (gethash file agent-recall--index) :title)))
          (should (equal "ses_abc123" (agent-recall--read-embedded-session-id file)))
          (should (string-match-p (regexp-quote title)
                                  (caar (agent-recall--list-transcripts)))))))))

(ert-deftest test-session-title-survives-reindex-in-both-layouts ()
  (agent-recall-test--with-index
    (let* ((dir (expand-file-name "project/.agent-shell/transcripts" root))
           (extra (expand-file-name "org" root))
           (markdown (expand-file-name "chat.md" dir))
           (org (expand-file-name "chat.org" extra))
           (agent-recall-search-paths (list root))
           (agent-recall-extra-transcript-dirs (list (list :dir extra))))
      (make-directory dir t)
      (make-directory extra t)
      (with-temp-file markdown
        (insert "# Transcript\n**Title:** Markdown title\n\n---\n\n## User\nHi\n"))
      (with-temp-file org
        (insert "#+TITLE: Transcript\n#+PROPERTY: Session_Title Org title\n\n** User\nHi\n"))
      (agent-recall-reindex)
      (should (equal "Markdown title" (plist-get (gethash markdown agent-recall--index) :title)))
      (should (equal "Org title" (plist-get (gethash org agent-recall--index) :title))))))

(ert-deftest test-opencode-id-all-header-variants ()
  (dolist (fixture '((".md" . "**Session:**") (".md" . "**Session ID:**")
                     (".org" . "#+PROPERTY: Session") (".org" . "#+PROPERTY: Session_ID")))
    (agent-recall-test--with-index
      (let ((file (expand-file-name (concat "chat" (car fixture)) root)))
        (with-temp-file file (insert (cdr fixture) " ses_AbC123 \n"))
        (should (equal "ses_AbC123" (agent-recall--read-embedded-session-id file)))))))

(provide 'test-pr-regressions)
;;; test-pr-regressions.el ends here
