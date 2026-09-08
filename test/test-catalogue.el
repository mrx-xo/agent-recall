;;; test-catalogue.el --- Tests for catalogue commands and browse narrowing -*- lexical-binding: t; -*-

;;; Commentary:
;; The data layer is covered in test/test-metadata.el.  This file covers
;; the layer above it: the catalogue browser's candidate list, browse
;; narrowing, and the transcript-mode key and header entry.
;; Run with:
;;   emacs --batch -l agent-recall.el -l test/test-catalogue.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'agent-recall)

(defmacro with-catalogue-fixture (&rest body)
  "Run BODY with a temp metadata store and a two-transcript index.
Binds `file-a' (session `id-a', project alpha, newer) and `file-b'
\(session `id-b', project beta, older)."
  (declare (indent 0))
  `(let* ((dir (file-truename (make-temp-file "agent-recall-catalogue-" t)))
          (agent-recall-metadata-file (expand-file-name "metadata.el" dir))
          (agent-recall--metadata nil)
          (agent-recall--metadata-loaded-p nil)
          (agent-recall--index (make-hash-table :test 'equal))
          (agent-recall--index-loaded-p t)
          (agent-recall-browse-sort 'date-desc)
          (file-a (expand-file-name "2026-09-05-10-00-00.md" dir))
          (file-b (expand-file-name "2026-09-01-10-00-00.md" dir))
          (id-a "aaaaaaaa-0000-0000-0000-000000000001")
          (id-b "bbbbbbbb-0000-0000-0000-000000000002"))
     (with-temp-file file-a (insert "## User (1)\n\nalpha\n"))
     (with-temp-file file-b (insert "## User (1)\n\nbeta\n"))
     (puthash file-a (list :project "alpha" :dir dir
                           :timestamp "2026-09-05-10-00-00"
                           :session-id id-a :preview "alpha")
              agent-recall--index)
     (puthash file-b (list :project "beta" :dir dir
                           :timestamp "2026-09-01-10-00-00"
                           :session-id id-b :preview "beta")
              agent-recall--index)
     (unwind-protect
         (progn ,@body)
       (delete-directory dir t))))

;;;; Catalogue browser candidates

(ert-deftest test-catalogue-transcripts-only-catalogued ()
  "The catalogue browser lists catalogued sessions only."
  (with-catalogue-fixture
    (should-not (agent-recall--catalogue-transcripts))
    (agent-recall-catalogue-put id-b :note "keep")
    (let ((rows (agent-recall--catalogue-transcripts)))
      (should (= 1 (length rows)))
      (should (equal file-b (cdr (car rows)))))))

(ert-deftest test-catalogue-transcripts-newest-save-first ()
  "Rows follow the save timestamp, not the transcript date."
  (with-catalogue-fixture
    (agent-recall-catalogue-put id-a)
    (agent-recall-catalogue-put id-b)
    ;; Force the older transcript to be the newer save.
    (agent-recall-metadata-put id-a 'catalogued "2026-09-06T00:00:00+0000")
    (agent-recall-metadata-put id-b 'catalogued "2026-09-07T00:00:00+0000")
    (should (equal (list file-b file-a)
                   (mapcar #'cdr (agent-recall--catalogue-transcripts))))))

(ert-deftest test-catalogue-transcripts-narrow-to-tag ()
  "A tag narrows the browser to sessions carrying exactly that tag."
  (with-catalogue-fixture
    (agent-recall-catalogue-put id-a :tags '("syzygy" "resume"))
    (agent-recall-catalogue-put id-b :tags '("dotfiles"))
    (should (equal (list file-a)
                   (mapcar #'cdr (agent-recall--catalogue-transcripts "syzygy"))))
    (should-not (agent-recall--catalogue-transcripts "syz"))))

(ert-deftest test-catalogue-transcripts-display-carries-tags ()
  "The candidate row shows project, label, and #tags."
  (with-catalogue-fixture
    (agent-recall-metadata-put id-a 'label "resume-from-History")
    (agent-recall-catalogue-put id-a :note "why" :tags '("syzygy"))
    (let ((display (substring-no-properties
                    (car (car (agent-recall--catalogue-transcripts))))))
      (should (string-match-p "alpha" display))
      (should (string-match-p "resume-from-History" display))
      (should (string-match-p "#syzygy" display)))))

(ert-deftest test-catalogue-transcripts-skip-unindexed-sessions ()
  "A catalogued session whose transcript left the index yields no row."
  (with-catalogue-fixture
    (agent-recall-catalogue-put "cccccccc-0000-0000-0000-000000000003")
    (should-not (agent-recall--catalogue-transcripts))))

;;;; Browse narrowing

(ert-deftest test-browse-transcripts-narrow-catalogued ()
  "Browse with the catalogued filter drops uncatalogued transcripts."
  (with-catalogue-fixture
    (agent-recall-catalogue-put id-a)
    (should (equal (list file-a file-b)
                   (mapcar #'cdr (agent-recall--list-transcripts))))
    (should (equal (list file-a)
                   (mapcar #'cdr (agent-recall--list-transcripts 'catalogued))))))

;;;; Transcript mode

(ert-deftest test-catalogue-transcript-mode-key ()
  "`s' in transcript-mode catalogues the transcript."
  (should (eq #'agent-recall-catalogue
              (lookup-key agent-recall-transcript-mode-map (kbd "s")))))

(ert-deftest test-catalogue-header-entry ()
  "The header shows a Catalogued entry with the note only when saved."
  (with-catalogue-fixture
    (let ((plain (substring-no-properties (agent-recall--header-line id-a))))
      (should-not (string-match-p "Catalogued" plain)))
    (agent-recall-catalogue-put id-a :note "Go struct dropped resumable")
    (let ((saved (substring-no-properties (agent-recall--header-line id-a))))
      (should (string-match-p "Catalogued" saved))
      (should (string-match-p "Go struct dropped resumable" saved)))))

;;;; Session resolution

(ert-deftest test-catalogue-session-id-from-transcript-buffer ()
  "In a transcript buffer the session comes from the mode's local variable."
  (with-catalogue-fixture
    (with-temp-buffer
      (setq-local agent-recall--transcript-session-id id-a)
      (should (equal id-a (agent-recall--catalogue-session-id))))))

(ert-deftest test-catalogue-session-id-from-agent-shell-state ()
  "In a live agent-shell buffer the session comes from `agent-shell--state'."
  (with-catalogue-fixture
    (with-temp-buffer
      (setq-local agent-shell--state `((:session . ((:id . ,id-b)))))
      (should (equal id-b (agent-recall--catalogue-session-id))))))

(provide 'test-catalogue)
;;; test-catalogue.el ends here
