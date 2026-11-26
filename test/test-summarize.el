;;; test-summarize.el --- Tests for transcript summarization -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the ACP session handling in `agent-recall-summarize'.
;; Run with:
;;   emacs --batch -l agent-recall.el -l test/test-summarize.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'agent-recall)

(ert-deftest agent-recall-summarize-stderr-notice-does-not-tear-down ()
  "Agent stderr output is a notice, not a fatal error.

acp.el turns every non-empty stderr line into an error handler call
\(for example the acp-multiplex startup banner).  The summarizer must
log it and keep the session alive; only request failures tear down."
  (let ((work-buffer (generate-new-buffer " *test-summarize-work*"))
        (progress-buffer (generate-new-buffer " *test-summarize-progress*"))
        (shutdown-calls 0))
    (unwind-protect
        (cl-letf (((symbol-function 'acp-shutdown)
                   (lambda (&rest _) (cl-incf shutdown-calls))))
          (with-current-buffer work-buffer
            (setq agent-recall--summarize-client '((:process . nil))))
          (agent-recall--summarize-handle-agent-error
           work-buffer progress-buffer
           '((code . -32603)
             (message . "acp-multiplex: socket /tmp/x.sock, log /tmp/x.log")))
          (should (buffer-live-p work-buffer))
          (should (= shutdown-calls 0))
          (should (with-current-buffer work-buffer
                    agent-recall--summarize-client))
          (should (string-match-p "acp-multiplex: socket"
                                  (with-current-buffer progress-buffer
                                    (buffer-string)))))
      (ignore-errors (kill-buffer work-buffer))
      (ignore-errors (kill-buffer progress-buffer)))))

(ert-deftest agent-recall-summary-parent-file-maps-summary-to-transcript ()
  "A TIMESTAMP.summary.md hit resolves to its TIMESTAMP.md transcript."
  (should (equal (agent-recall--summary-parent-file
                  "/p/.agent-shell/transcripts/2026-07-09-16-24-09.summary.md")
                 "/p/.agent-shell/transcripts/2026-07-09-16-24-09.md"))
  (should (equal (agent-recall--summary-parent-file
                  "/p/.agent-shell/transcripts/2026-07-09-16-24-09.summary.org")
                 "/p/.agent-shell/transcripts/2026-07-09-16-24-09.org"))
  (should (equal (agent-recall--summary-parent-file
                  "/p/.agent-shell/transcripts/2026-07-09-16-24-09.md")
                 "/p/.agent-shell/transcripts/2026-07-09-16-24-09.md"))
  (should-not (agent-recall--summary-parent-file nil)))

(ert-deftest agent-recall-summary-parent-file-preserves-match-data ()
  "Callers read match data after mapping the file; it must survive."
  (should (string-match "\\(b\\)" "abc"))
  (agent-recall--summary-parent-file "/p/x.summary.md")
  (should (equal (match-string 1 "abc") "b"))
  (should (= (match-end 0) 2)))

(ert-deftest agent-recall-summarize-nul-preserves-unicode-and-json ()
  "NUL must not turn UTF-8 text into raw bytes that JSON cannot encode."
  (dolist (nul '("" "\0"))
    (let ((file (make-temp-file "recall-nul-" nil ".md")))
      (unwind-protect
          (progn
            (let ((coding-system-for-write 'utf-8-unix))
              (with-temp-file file
                (insert "# Transcript\n\n## User (now)\nCafé 日本語 " nul "fin\n"
                        "### Tool Call\nDiscard this tool output\n"
                        "## Agent (now)\nRéponse\n")))
            (let* ((before (with-temp-buffer
                             (insert-file-contents-literally file)
                             (buffer-string)))
                   (clean (agent-recall--clean-transcript-string file)))
              ;; Exercise the same native JSON serializer ACP uses.
              (should (stringp (json-serialize `((text . ,clean)))))
              (should (string-match-p "Café 日本語 fin" clean))
              (should (string-match-p "Réponse" clean))
              (should-not (string-match-p "\0" clean))
              (should-not (string-match-p "Discard this tool output" clean))
              (should (equal before (with-temp-buffer
                                      (insert-file-contents-literally file)
                                      (buffer-string))))))
        (delete-file file)))))

(ert-deftest agent-recall-summarize-send-error-advances-without-timeout ()
  "A local send failure must report immediately, then process the next file."
  (let* ((root (make-temp-file "recall-send-error-" t))
         (first (expand-file-name "first.md" root))
         (second (expand-file-name "second.md" root))
         (work (generate-new-buffer " *recall-send-work*"))
         (progress (generate-new-buffer " *recall-send-progress*"))
         (agent-recall--index (make-hash-table :test 'equal))
         (client (acp-make-client :command "unused-test-agent"))
         (attempts 0)
         (sent nil)
         failed-timer)
    (unwind-protect
        (progn
          (with-temp-file first (insert "## User (now)\nFirst transcript\n"))
          (with-temp-file second (insert "## User (now)\nSecond transcript\n"))
          (with-current-buffer work
            (setq agent-recall--summarize-client client
                  agent-recall--summarize-session-id "test-session"))
          ;; Keep ACP request construction and JSON serialization real; only
          ;; replace process startup and transport so no agent is launched.
          (cl-letf (((symbol-function 'acp--client-started-p) (lambda (_) t))
                    ((symbol-function 'process-send-string)
                     (lambda (_process json)
                       (cl-incf attempts)
                       (if (= attempts 1)
                           (progn
                             (setq failed-timer (buffer-local-value
                                                 'agent-recall--summarize-timeout-timer work))
                             (error "Synthetic transport failure"))
                         (push (json-parse-string json :object-type 'alist) sent)))))
            (agent-recall--summarize-next (list first second) work progress 2 0)
            ;; ACP may have registered callbacks before transport failed.
            ;; A late callback must not complete or skip another transcript.
            (funcall (map-elt (cdr (car (map-elt client :pending-requests)))
                              :on-success) nil)
            (should-not (file-exists-p (agent-recall--summary-file first)))
            (let ((deadline (+ (float-time) 2)))
              (while (and (< attempts 2) (< (float-time) deadline))
                (accept-process-output nil 0.01))))
          (should (= attempts 2))
          (should (with-current-buffer progress
                    (string-match-p "send failed: Synthetic transport failure"
                                    (buffer-string))))
          (should-not (memq failed-timer timer-list))
          (should-not (file-exists-p (agent-recall--summary-file first)))
          (let* ((message (car sent))
                 (prompt (alist-get 'prompt (alist-get 'params message))))
            (should (equal "session/prompt" (alist-get 'method message)))
            (should (string-match-p "Second transcript"
                                    (alist-get 'text (aref prompt 0)))))
          ;; Completing the second request must write its summary and end the
          ;; batch, cancelling the remaining timer and spinner.
          (let* ((request (cdr (car (map-elt client :pending-requests))))
                 (callback (map-elt request :on-success)))
            (with-current-buffer work
              (setq agent-recall--summarize-response-text "# Summary\nSecond result"))
            (funcall callback nil))
          (should (equal "# Summary\nSecond result\n"
                         (with-temp-buffer
                           (insert-file-contents (agent-recall--summary-file second))
                           (buffer-string))))
          (should-not (buffer-live-p work)))
      (when (buffer-live-p work)
        (with-current-buffer work
          (setq agent-recall--summarize-client nil
                agent-recall--summarize-session-id nil))
        (agent-recall--summarize-cleanup work)
        (kill-buffer work))
      (kill-buffer progress)
      (delete-directory root t))))

(ert-deftest agent-recall-summarize-json-error-is-reported-before-transport ()
  "Even an unencodable prompt must finish without waiting for a timeout."
  (let* ((file (make-temp-file "recall-json-error-" nil ".md" "## User (now)\nText\n"))
         (work (generate-new-buffer " *recall-json-work*"))
         (progress (generate-new-buffer " *recall-json-progress*"))
         (transport-called nil)
         (agent-recall--index (make-hash-table :test 'equal))
         (client (acp-make-client
                  :command "unused-test-agent"
                  :outgoing-request-decorator
                  (lambda (request)
                    ;; Inject an undecoded byte after Recall's cleaning to
                    ;; exercise an actual ACP serialization error.
                    (map-put! request :params
                              `((text . ,(string-to-multibyte (unibyte-string 255)))))
                    request))))
    (unwind-protect
        (progn
          (with-current-buffer work
            (setq agent-recall--summarize-client client
                  agent-recall--summarize-session-id "test-session"))
          (cl-letf (((symbol-function 'acp--client-started-p) (lambda (_) t))
                    ((symbol-function 'process-send-string)
                     (lambda (&rest _)
                       (setq transport-called t)
                       (ert-fail "Unencodable prompt reached transport"))))
            (agent-recall--summarize-next (list file) work progress 1 0))
          ;; Emacs 29 and newer serializers signal different error types.
          ;; Require a visible error and no transport, not a particular name.
          (let ((status (with-current-buffer progress (buffer-string))))
            (should (string-match-p "send failed: [^\n]+" status)))
          (should-not transport-called)
          (should-not (buffer-local-value 'agent-recall--summarize-timeout-timer work))
          (should-not (buffer-local-value 'agent-recall--summarize-spinner-timer work))
          (let ((deadline (+ (float-time) 2)))
            (while (and (buffer-live-p work) (< (float-time) deadline))
              (accept-process-output nil 0.01)))
          (should-not (buffer-live-p work))
          (should-not (file-exists-p (agent-recall--summary-file file))))
      (when (buffer-live-p work)
        (with-current-buffer work
          (setq agent-recall--summarize-client nil
                agent-recall--summarize-session-id nil))
        (agent-recall--summarize-cleanup work)
        (kill-buffer work))
      (kill-buffer progress)
      (delete-file file))))

(provide 'test-summarize)
;;; test-summarize.el ends here
