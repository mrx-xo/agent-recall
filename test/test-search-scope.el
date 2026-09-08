;;; test-search-scope.el --- Tests for search scope and summary topics -*- lexical-binding: t; -*-

;;; Commentary:
;; Covers the summaries-only search scope and the summary-topic annotation.
;; Run with:
;;   emacs --batch -l agent-recall.el -l test/test-search-scope.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'agent-recall)

(defmacro agent-recall-test--with-transcript (var &rest body)
  "Bind VAR to a temporary transcript path and run BODY.
The transcript and any sidecar written beside it are removed after."
  (declare (indent 1))
  `(let* ((dir (make-temp-file "agent-recall-test" t))
          (,var (expand-file-name "2026-01-02-03-04-05.md" dir)))
     (unwind-protect
         (progn (with-temp-file ,var (insert "## User (1)\n\nhey\n"))
                ,@body)
       (delete-directory dir t))))

;;;; Search scope

(ert-deftest agent-recall-scoped-globs-excludes-summaries-by-default ()
  "A transcript search must exclude sidecars, or every hit is reported twice."
  (let ((agent-recall--search-scope 'transcripts)
        (agent-recall-file-patterns '("*.md" "*.org")))
    (let ((globs (agent-recall--scoped-globs)))
      (should (member "*.md" globs))
      (should (member "*.org" globs))
      (should (member "!*.summary.*" globs))
      (should-not (member "*.summary.*" globs)))))

(ert-deftest agent-recall-scoped-globs-targets-only-summaries ()
  "In `summaries' scope nothing but the sidecars is searched."
  (let ((agent-recall--search-scope 'summaries)
        (agent-recall-file-patterns '("*.md" "*.org")))
    (should (equal (agent-recall--scoped-globs)
                   '("--glob" "*.summary.*")))))

(ert-deftest agent-recall-grep-includes-follow-the-scope ()
  "The grep backend gets --exclude for transcripts and --include for summaries."
  (let ((agent-recall-file-patterns '("*.md")))
    (let ((agent-recall--search-scope 'transcripts))
      (should (string-match-p "--exclude=" (agent-recall--file-patterns-as-includes))))
    (let ((agent-recall--search-scope 'summaries))
      (let ((args (agent-recall--file-patterns-as-includes)))
        (should (string-match-p "--include=" args))
        (should-not (string-match-p "--exclude=" args))
        (should-not (string-match-p "\\*\\.md" args))))))

(ert-deftest agent-recall-search-summaries-binds-the-scope ()
  "`agent-recall-search-summaries' hands the backend a summaries-only scope."
  (let (seen)
    (cl-letf (((symbol-function 'agent-recall--index-dirs) (lambda () '("/tmp")))
              ((symbol-function 'agent-recall--search-via-grep)
               (lambda (&rest _) (setq seen agent-recall--search-scope))))
      (let ((agent-recall-search-function 'grep))
        (agent-recall-search-summaries "anything"))
      (should (eq seen 'summaries))
      ;; and the binding does not leak past the command
      (should (eq agent-recall--search-scope 'transcripts)))))

;;;; Summary topics

(ert-deftest agent-recall-summary-topic-reads-the-topic-line ()
  "The Topic line of a sidecar is what the picker shows."
  (agent-recall-test--with-transcript transcript
    (with-temp-file (agent-recall--summary-file transcript)
      (insert "# Summary\n\n**Topic:** Fixing sketchybar spacing\n"
              "**Problem:** widgets overlapped\n"))
    (clrhash agent-recall--summary-topic-cache)
    (should (equal (agent-recall--summary-topic transcript)
                   "Fixing sketchybar spacing"))))

(ert-deftest agent-recall-summary-topic-nil-without-a-sidecar ()
  "No sidecar means no topic, and no error."
  (agent-recall-test--with-transcript transcript
    (clrhash agent-recall--summary-topic-cache)
    (should-not (agent-recall--summary-topic transcript))))

(ert-deftest agent-recall-summary-topic-refreshes-when-the-sidecar-changes ()
  "The cache is keyed by mtime, so a re-summarized transcript updates."
  (agent-recall-test--with-transcript transcript
    (let ((summary (agent-recall--summary-file transcript)))
      (clrhash agent-recall--summary-topic-cache)
      (with-temp-file summary (insert "**Topic:** first\n"))
      (should (equal (agent-recall--summary-topic transcript) "first"))
      (with-temp-file summary (insert "**Topic:** second\n"))
      (set-file-times summary (time-add (current-time) 10))
      (should (equal (agent-recall--summary-topic transcript) "second")))))

(ert-deftest agent-recall-candidate-description-prefers-the-topic ()
  "Topic wins over the indexed preview; the preview is the fallback."
  (agent-recall-test--with-transcript transcript
    (clrhash agent-recall--summary-topic-cache)
    (should (equal (agent-recall--candidate-description transcript "hey")
                   "  hey"))
    (with-temp-file (agent-recall--summary-file transcript)
      (insert "**Topic:** Wiring the Emacs daemon\n"))
    (clrhash agent-recall--summary-topic-cache)
    (should (equal (agent-recall--candidate-description transcript "hey")
                   "  Wiring the Emacs daemon"))))

(ert-deftest agent-recall-candidate-description-nil-when-empty ()
  "An empty preview and no sidecar annotates nothing at all."
  (agent-recall-test--with-transcript transcript
    (clrhash agent-recall--summary-topic-cache)
    (should-not (agent-recall--candidate-description transcript ""))
    (should-not (agent-recall--candidate-description transcript nil))))

(provide 'test-search-scope)
;;; test-search-scope.el ends here
