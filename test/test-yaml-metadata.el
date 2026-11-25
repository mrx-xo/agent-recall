;;; test-yaml-metadata.el --- Native transcript compatibility -*- lexical-binding: t; -*-
(require 'ert)
(require 'agent-recall)

(defmacro agent-recall-yaml-test--with-file (prefix &rest body)
  (declare (indent 1))
  `(let* ((root (make-temp-file "recall-yaml-" t))
          (file (expand-file-name "chat.md" root))
          (sid "11111111-1111-1111-1111-111111111111"))
     (unwind-protect
         (progn
           (with-temp-file file
             (insert ,prefix "---\nagent: \"Codex\"\nstarted: 2026-11-13T09:20:04-06:00\n"
                     "working_directory: " (json-encode-string root) "\n"
                     "session_id: \"" sid "\"\nmodel: \"gpt-test\"\n---\n\n"
                     "# Agent Shell Transcript\n\n## User\nKeep this body.\n"
                     "**Title:** body example\n---\nagent: Claude\n---\n"))
           ,@body)
       (delete-directory root t))))

(ert-deftest recall-yaml-native-and-mixed-headers ()
  (dolist (prefix '("" "**Session:** 11111111-1111-1111-1111-111111111111\n\n**Title:** Existing title\n\n"))
    (agent-recall-yaml-test--with-file prefix
      (should (equal "Codex" (agent-recall--read-agent-name file)))
      (should (equal root (agent-recall--read-working-directory file)))
      (should (equal sid (agent-recall--read-embedded-session-id file)))
      (should (equal "2026-11-13T15:20:04Z"
                     (format-time-string "%FT%TZ" (agent-recall--parse-transcript-timestamp file) t))))))

(ert-deftest recall-yaml-native-id-write-is-no-op ()
  (agent-recall-yaml-test--with-file ""
    (let ((before (with-temp-buffer (insert-file-contents file) (buffer-string))))
      (agent-recall--write-session-id-to-file file sid)
      (should (equal before (with-temp-buffer (insert-file-contents file) (buffer-string)))))))

(ert-deftest recall-yaml-long-header-and-escaped-fields ()
  (agent-recall-yaml-test--with-file ""
    (let ((title (concat (make-string 4000 ?a) " \"quoted\" \\ path")))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min)) (forward-line 1)
        (insert "title: " (json-encode-string title) "\n")
        (write-region (point-min) (point-max) file nil 'silent))
      (should (equal title (agent-recall--read-session-title file)))
      (should (equal "Codex" (agent-recall--read-agent-name file)))
      (should (equal sid (agent-recall--read-embedded-session-id file))))))

(ert-deftest recall-yaml-body-is-not-metadata ()
  (agent-recall-yaml-test--with-file ""
    (with-temp-file file
      (insert "# Transcript\n\n---\n\n## User\n---\nagent: Codex\ntitle: Wrong\nsession_id: \"" sid "\"\n---\n"))
    (should-not (agent-recall--read-agent-name file))
    (should-not (agent-recall--read-session-title file))
    (should-not (agent-recall--read-embedded-session-id file))))

(ert-deftest recall-yaml-resume-selects-original-agent-and-directory ()
  (agent-recall-yaml-test--with-file ""
    (let ((agent-recall-resume-restore-preferences nil)
          (agent-recall-resume-continue-transcript nil)
          (codex '((:identifier . codex) (:mode-line-name . "Codex")))
          (claude '((:identifier . claude-code) (:mode-line-name . "Claude"))))
      ;; Stop at the process-launch boundary: no agent process or API call.
      (cl-letf (((symbol-function 'agent-shell--resolve-preferred-config) (lambda () claude))
                ((symbol-function 'agent-shell--resolved-agent-configs) (lambda () (list claude codex)))
                ((symbol-function 'agent-shell--start)
                 (lambda (&rest args) (throw 'launch (list (plist-get args :config) default-directory (plist-get args :session-id))))))
        (should (equal (list codex root sid)
                       (catch 'launch (agent-recall--start-resume sid file))))))))

(ert-deftest recall-yaml-unknown-agent-prompts-instead-of-defaulting ()
  (agent-recall-yaml-test--with-file ""
    (with-temp-file file (insert "# Transcript\n\n---\n\n## User\nHello\n"))
    (cl-letf (((symbol-function 'agent-shell--resolve-preferred-config)
               (lambda () (ert-fail "Must not silently choose preferred agent")))
              ((symbol-function 'agent-shell-select-config)
               (lambda (&rest _) (throw 'selection 'prompted))))
      (should (eq 'prompted (catch 'selection (agent-recall--start-resume sid file)))))))

(ert-deftest recall-yaml-malformed-block-does-not-read-body ()
  (agent-recall-yaml-test--with-file ""
    (with-temp-file file
      (insert "---\nagent: \"Codex\n## User\n**Agent:** Claude\n**Title:** Not metadata\n"))
    (should-not (agent-recall--read-agent-name file))
    (should-not (agent-recall--read-session-title file))))

(ert-deftest recall-yaml-single-quoted-title-update-keeps-other-fields ()
  (agent-recall-yaml-test--with-file ""
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min)) (forward-line 1)
      (insert "title: 'Dad''s notes'\n")
      (write-region (point-min) (point-max) file nil 'silent))
    (agent-recall--write-session-title-to-file file "Updated notes")
    (should (equal "Updated notes" (agent-recall--read-session-title file)))
    (should (equal "Codex" (agent-recall--read-agent-name file)))
    (should (equal sid (agent-recall--read-embedded-session-id file)))))

(ert-deftest recall-yaml-empty-title-is-not-a-display-title ()
  (agent-recall-yaml-test--with-file ""
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min)) (forward-line 1)
      (insert "title: \"\"\n")
      (write-region (point-min) (point-max) file nil 'silent))
    (should-not (agent-recall--read-session-title file))))

(ert-deftest recall-yaml-resume-only-attaches-transcript-after-matching-session ()
  (dolist (succeeds '(t nil))
    (agent-recall-yaml-test--with-file ""
      (let ((agent-recall-resume-restore-preferences nil)
            (agent-recall-resume-continue-transcript t))
        (with-temp-buffer
          (setq major-mode 'agent-shell-mode)
          (setq-local agent-shell--state (agent-shell--make-state :buffer (current-buffer)))
          (setq-local agent-shell--transcript-file "new-transcript.md")
          (let ((shell (current-buffer)))
            (cl-letf (((symbol-function 'agent-shell--resolved-agent-configs)
                       (lambda () '(((:identifier . codex) (:mode-line-name . "Codex")))))
                      ((symbol-function 'agent-shell--start) (lambda (&rest _) shell))
                      ((symbol-function 'agent-recall--display-buffer) #'ignore))
              (agent-recall--start-resume sid file)
              (should (equal "new-transcript.md" agent-shell--transcript-file))
              (map-put! (map-elt agent-shell--state :session) :id (if succeeds sid "another-session"))
              (agent-shell--emit-event :event 'init-session)
              (should (equal (if succeeds file "new-transcript.md") agent-shell--transcript-file)))))))))

(ert-deftest recall-yaml-title-below-block-remains-readable ()
  (agent-recall-yaml-test--with-file ""
    (with-temp-buffer
      (insert-file-contents file)
      (search-forward "# Agent Shell Transcript\n")
      (insert "\n**Title:** Saved outside YAML\n")
      (write-region (point-min) (point-max) file nil 'silent))
    (should (equal "Saved outside YAML" (agent-recall--read-session-title file)))
    (agent-recall--write-session-title-to-file file "Updated title")
    (should (equal "Updated title" (agent-recall--read-session-title file)))))

(ert-deftest recall-yaml-native-id-wins-over-stale-markdown ()
  (agent-recall-yaml-test--with-file
      "**Session:** 22222222-2222-2222-2222-222222222222\n\n"
    (should (equal sid (agent-recall--read-embedded-session-id file)))
    (agent-recall--write-session-id-to-file file sid)
    (should (equal sid (agent-recall--read-embedded-session-id file)))))

(ert-deftest recall-yaml-title-update-preserves-native-fields-and-body ()
  (agent-recall-yaml-test--with-file ""
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min)) (forward-line 1)
      (insert "title: \"Native title\"\n")
      (write-region (point-min) (point-max) file nil 'silent))
    (agent-recall--write-session-title-to-file file "A \"quoted\" title")
    (should (equal "A \"quoted\" title" (agent-recall--read-session-title file)))
    (should (equal sid (agent-recall--read-embedded-session-id file)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (= 1 (how-many "^title:")))
      (should (search-forward "title: \"A \\\"quoted\\\" title\"" nil t))
      (should (string-suffix-p "## User\nKeep this body.\n**Title:** body example\n---\nagent: Claude\n---\n" (buffer-string))))))

(provide 'test-yaml-metadata)
