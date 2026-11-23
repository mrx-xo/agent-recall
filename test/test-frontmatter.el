;;; test-frontmatter.el --- Tests for YAML frontmatter transcript headers -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'agent-recall)

(defconst agent-recall-test--session-id
  "980dd1d5-b729-48a5-b6d5-7a432713a0ae")

(defun agent-recall-test--frontmatter-transcript (dir &optional prefix
                                                       no-session-id
                                                       extra-fields)
  "Build a YAML frontmatter transcript string for DIR."
  (concat (or prefix "")
          "---\n"
          "agent: \"Claude\"\n"
          "started: 2026-11-23T12:51:25+01:00\n"
          (format "working_directory: %s\n" (json-encode-string dir))
          (unless no-session-id
            (format "session_id: \"%s\"\n" agent-recall-test--session-id))
          "model: \"claude-opus-5-5\"\n"
          (or extra-fields "")
          "---\n\n"
          "# Agent Shell Transcript\n\n"
          "## User (2026-11-23 12:51:25)\n\n"
          "**Agent:** quoted example in the conversation\n"))

(defmacro agent-recall-test--with-transcript (contents-form &rest body)
  "Run BODY with `file' and `root' bound to a temp transcript containing CONTENTS-FORM."
  (declare (indent 1))
  `(let* ((root (file-name-as-directory
                 (make-temp-file "agent-recall-frontmatter-" t)))
          (file (expand-file-name "chat.md" root)))
     (unwind-protect
         (progn
           (with-temp-file file (insert ,contents-form))
           ,@body)
       (delete-directory root t))))

(ert-deftest test-frontmatter-header-fields ()
  (dolist (prefix (list nil
                        (format "**Session:** %s\n\n**Title:** A title\n\n"
                                agent-recall-test--session-id)))
    (agent-recall-test--with-transcript
        (agent-recall-test--frontmatter-transcript root prefix)
      (should (equal "Claude" (agent-recall--read-agent-name file)))
      (should (equal root (agent-recall--read-working-directory file)))
      (should (equal agent-recall-test--session-id
                     (agent-recall--read-embedded-session-id file)))
      (should (equal "2026-11-23 11:51:25"
                     (format-time-string
                      "%F %T" (agent-recall--parse-transcript-timestamp file)
                      t))))))

(ert-deftest test-frontmatter-value-decoding ()
  (should (equal "/a/\"b\"/c" (agent-recall--frontmatter-value "\"/a/\\\"b\\\"/c\"")))
  (should (equal "2026-11-23T12:51:25+01:00"
                 (agent-recall--frontmatter-value " 2026-11-23T12:51:25+01:00 ")))
  (should-not (agent-recall--frontmatter-value ""))
  (should-not (agent-recall--frontmatter-value "null")))

(ert-deftest test-frontmatter-legacy-header-still-read ()
  (agent-recall-test--with-transcript
      (concat "# Agent Shell Transcript\n\n"
              "**Agent:** Codex\n"
              "**Started:** 2026-10-05 09:37:14\n"
              (format "**Working Directory:** %s\n" root)
              (format "**Session ID:** %s\n\n" agent-recall-test--session-id)
              "---\n\n"
              "## User (2026-10-05 09:37:14)\n\nagent: \"Claude\"\n")
    (should (equal "Codex" (agent-recall--read-agent-name file)))
    (should (equal root (agent-recall--read-working-directory file)))
    (should (equal agent-recall-test--session-id
                   (agent-recall--read-embedded-session-id file)))
    (should (agent-recall--parse-transcript-timestamp file))))

(ert-deftest test-frontmatter-ignores-conversation-text ()
  (agent-recall-test--with-transcript
      "# Transcript\n\n---\n\nagent: \"Claude\"\n\n---\n"
    (should-not (agent-recall--read-agent-name file))))

(ert-deftest test-frontmatter-session-id-not-duplicated ()
  (agent-recall-test--with-transcript
      (agent-recall-test--frontmatter-transcript root)
    (agent-recall--write-session-id-to-file file agent-recall-test--session-id)
    (with-temp-buffer
      (insert-file-contents file)
      (should (looking-at-p "---\n"))
      (should-not (search-forward "**Session:**" nil t)))))

(ert-deftest test-frontmatter-resume-picks-transcript-agent ()
  (agent-recall-test--with-transcript
      (agent-recall-test--frontmatter-transcript root)
    (let* ((claude '((:identifier . claude-code)
                     (:mode-line-name . "Claude")
                     (:buffer-name . "Claude")))
           (codex '((:identifier . codex)
                    (:mode-line-name . "Codex")
                    (:buffer-name . "Codex")))
           (agent-shell-agent-configs (list codex claude)))
      (cl-letf (((symbol-function 'agent-shell--resolve-preferred-config)
                 #'ignore)
                ((symbol-function 'agent-shell--resolved-agent-configs)
                 (lambda () agent-shell-agent-configs)))
        (should (eq claude
                    (agent-recall--agent-config-for-transcript file)))))))

(defun agent-recall-test--file-string (file)
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(defun agent-recall-test--should-read-all-fields (file root)
  (should (equal "Claude" (agent-recall--read-agent-name file)))
  (should (equal root (agent-recall--read-working-directory file)))
  (should (equal agent-recall-test--session-id
                 (agent-recall--read-embedded-session-id file)))
  (should (agent-recall--parse-transcript-timestamp file)))

(ert-deftest test-frontmatter-title-written-below-frontmatter ()
  (agent-recall-test--with-transcript
      (agent-recall-test--frontmatter-transcript root)
    (agent-recall--write-session-title-to-file file "New title")
    (let ((contents (agent-recall-test--file-string file)))
      (should (string-prefix-p "---\n" contents))
      (should (string-match-p
               "^---\n\n# Agent Shell Transcript\n\n\\*\\*Title:\\*\\* New title\n\n## User"
               contents)))
    (should (equal "New title" (agent-recall--read-session-title file)))
    (agent-recall--write-session-title-to-file file "Renamed")
    (should (equal "Renamed" (agent-recall--read-session-title file)))
    (should (= 1 (with-temp-buffer
                   (insert-file-contents file)
                   (how-many "^\\*\\*Title:\\*\\*"))))
    (agent-recall-test--should-read-all-fields file root)))

(ert-deftest test-frontmatter-session-written-below-frontmatter ()
  (agent-recall-test--with-transcript
      (agent-recall-test--frontmatter-transcript root nil t)
    (should-not (agent-recall--read-embedded-session-id file))
    (agent-recall--write-session-id-to-file file agent-recall-test--session-id)
    (should (string-prefix-p "---\n" (agent-recall-test--file-string file)))
    (agent-recall-test--should-read-all-fields file root)))

(ert-deftest test-frontmatter-writers-repair-lines-above-frontmatter ()
  (dolist (write (list (lambda (file)
                         (agent-recall--write-session-title-to-file file "Old title"))
                       (lambda (file)
                         (agent-recall--write-session-id-to-file
                          file agent-recall-test--session-id))))
    (agent-recall-test--with-transcript
        (agent-recall-test--frontmatter-transcript
         root
         (format "**Session:** %s\n\n**Title:** Old title\n\n"
                 agent-recall-test--session-id))
      (funcall write file)
      (let ((contents (agent-recall-test--file-string file)))
        (should (string-prefix-p "---\n" contents))
        (with-temp-buffer
          (insert contents)
          (should (= 1 (how-many "^\\*\\*Session:\\*\\*" (point-min))))
          (should (= 1 (how-many "^\\*\\*Title:\\*\\*" (point-min))))))
      (should (equal "Old title" (agent-recall--read-session-title file)))
      (agent-recall-test--should-read-all-fields file root))))

(ert-deftest test-frontmatter-long-title-does-not-hide-fields ()
  (let ((title (make-string 3100 ?x)))
    (dolist (prefix (list nil (format "**Title:** %s\n\n" title)))
      (agent-recall-test--with-transcript
          (agent-recall-test--frontmatter-transcript root prefix)
        (unless prefix
          (agent-recall--write-session-title-to-file file title))
        (should (equal title (agent-recall--read-session-title file)))
        (agent-recall-test--should-read-all-fields file root)))))

(ert-deftest test-frontmatter-larger-than-first-read-chunk ()
  (agent-recall-test--with-transcript
      (agent-recall-test--frontmatter-transcript
       root nil nil (format "notes: %s\n"
                            (json-encode-string (make-string 5000 ?n))))
    (agent-recall-test--should-read-all-fields file root)))

(ert-deftest test-frontmatter-unterminated-is-not-a-header ()
  (agent-recall-test--with-transcript
      "---\nagent: \"Claude\"\n\n## User\nhello\n"
    (should-not (agent-recall--read-agent-name file))
    (should-not (agent-recall--read-embedded-session-id file))))

;;; test-frontmatter.el ends here
