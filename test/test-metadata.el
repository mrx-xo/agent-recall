;;; test-metadata.el --- Tests for the session metadata sidecar store -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the sidecar metadata store, capture merge semantics, and
;; resume preference restoration guards.
;; Run with:
;;   emacs --batch -l agent-recall.el -l test/test-metadata.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'agent-recall)

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defmacro with-temp-metadata-store (&rest body)
  "Run BODY against a fresh, temp-file-backed metadata store."
  (declare (indent 0))
  `(let* ((dir (make-temp-file "agent-recall-test-" t))
          (agent-recall-metadata-file (expand-file-name "metadata.el" dir))
          (agent-recall--metadata nil)
          (agent-recall--metadata-loaded-p nil))
     (unwind-protect
         (progn ,@body)
       (delete-directory dir t))))

(defconst test-md-session-id "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
  "Test session UUID.")

;; ---------------------------------------------------------------------------
;; Store round-trip
;; ---------------------------------------------------------------------------

(ert-deftest test-metadata-put-get-roundtrip ()
  "Values stored with put should come back with get."
  (with-temp-metadata-store
    (agent-recall-metadata-put test-md-session-id 'model "opus")
    (should (equal "opus" (agent-recall-metadata-get test-md-session-id 'model)))))

(ert-deftest test-metadata-persists-across-reload ()
  "Stored metadata should survive a reload from disk."
  (with-temp-metadata-store
    (agent-recall-metadata-put test-md-session-id 'model "opus")
    (agent-recall-metadata-put test-md-session-id 'effort "high")
    ;; Simulate a fresh Emacs session.
    (setq agent-recall--metadata nil
          agent-recall--metadata-loaded-p nil)
    (should (equal "opus" (agent-recall-metadata-get test-md-session-id 'model)))
    (should (equal "high" (agent-recall-metadata-get test-md-session-id 'effort)))))

(ert-deftest test-metadata-corrupt-file-recovers ()
  "A corrupt metadata file should yield an empty store, not an error."
  (with-temp-metadata-store
    (with-temp-file agent-recall-metadata-file
      (insert "(((((not a hash table"))
    (should-not (agent-recall-metadata test-md-session-id))
    ;; Store still usable afterwards.
    (agent-recall-metadata-put test-md-session-id 'model "opus")
    (should (equal "opus" (agent-recall-metadata-get test-md-session-id 'model)))))

(ert-deftest test-metadata-unknown-session-nil ()
  "Unknown session IDs should return nil, not error."
  (with-temp-metadata-store
    (should-not (agent-recall-metadata "nonexistent"))
    (should-not (agent-recall-metadata-get "nonexistent" 'model))
    (should-not (agent-recall-metadata nil))))

;; ---------------------------------------------------------------------------
;; Merge semantics
;; ---------------------------------------------------------------------------

(ert-deftest test-metadata-merge-last-write-wins ()
  "Merging an existing key should overwrite its value."
  (with-temp-metadata-store
    (agent-recall-metadata-merge test-md-session-id '((model . "opus") (effort . "high")))
    (agent-recall-metadata-merge test-md-session-id '((model . "sonnet")))
    (should (equal "sonnet" (agent-recall-metadata-get test-md-session-id 'model)))
    (should (equal "high" (agent-recall-metadata-get test-md-session-id 'effort)))))

(ert-deftest test-metadata-merge-nil-removes-key ()
  "A nil value in a merge should remove the key."
  (with-temp-metadata-store
    (agent-recall-metadata-merge test-md-session-id '((model . "opus") (label . "my label")))
    (agent-recall-metadata-merge test-md-session-id '((label . nil)))
    (should-not (agent-recall-metadata-get test-md-session-id 'label))
    (should (equal "opus" (agent-recall-metadata-get test-md-session-id 'model)))))

(ert-deftest test-metadata-merge-unchanged-skips-write ()
  "Merging identical values should not rewrite the file."
  (with-temp-metadata-store
    (agent-recall-metadata-merge test-md-session-id '((model . "opus")))
    (let ((mtime (file-attribute-modification-time
                  (file-attributes agent-recall-metadata-file))))
      ;; Make any rewrite observable regardless of timestamp resolution.
      (set-file-times agent-recall-metadata-file (encode-time 0 0 0 1 1 2000))
      (agent-recall-metadata-merge test-md-session-id '((model . "opus")))
      (should (time-equal-p (file-attribute-modification-time
                             (file-attributes agent-recall-metadata-file))
                            (encode-time 0 0 0 1 1 2000)))
      (ignore mtime))))

;; ---------------------------------------------------------------------------
;; Capture
;; ---------------------------------------------------------------------------

(ert-deftest test-capture-runs-hooks-and-merges ()
  "Capture should merge results from all capture functions."
  (with-temp-metadata-store
    (with-temp-buffer
      (setq-local agent-shell--state
                  `((:session . ((:id . ,test-md-session-id)))))
      (let ((agent-recall-capture-functions
             (list (lambda () '((model . "opus")))
                   (lambda () '((label . "window pane"))))))
        (agent-recall--session-metadata-capture)))
    (should (equal "opus" (agent-recall-metadata-get test-md-session-id 'model)))
    (should (equal "window pane" (agent-recall-metadata-get test-md-session-id 'label)))))

(ert-deftest test-capture-broken-hook-does-not-block-others ()
  "One failing capture function should not prevent the rest."
  (with-temp-metadata-store
    (with-temp-buffer
      (setq-local agent-shell--state
                  `((:session . ((:id . ,test-md-session-id)))))
      (let ((agent-recall-capture-functions
             (list (lambda () (error "boom"))
                   (lambda () '((model . "opus"))))))
        (agent-recall--session-metadata-capture)))
    (should (equal "opus" (agent-recall-metadata-get test-md-session-id 'model)))))

(ert-deftest test-capture-without-session-id-is-noop ()
  "Capture should do nothing when no session ID is known yet."
  (with-temp-metadata-store
    (with-temp-buffer
      (let ((agent-recall-capture-functions
             (list (lambda () '((model . "opus"))))))
        (agent-recall--session-metadata-capture)))
    (should (zerop (hash-table-count (progn (agent-recall--metadata-ensure)
                                            agent-recall--metadata))))))

;; ---------------------------------------------------------------------------
;; Restore guards
;; ---------------------------------------------------------------------------

(ert-deftest test-restore-allowed-p-respects-defcustom ()
  "Restore gating should follow `agent-recall-resume-restore-preferences'."
  (let ((metadata '((model . "opus"))))
    (let ((agent-recall-resume-restore-preferences t))
      (should (agent-recall--restore-allowed-p metadata))
      (should-not (agent-recall--restore-allowed-p nil)))
    (let ((agent-recall-resume-restore-preferences nil))
      (should-not (agent-recall--restore-allowed-p metadata)))))

(ert-deftest test-config-with-preferences-prepends-overrides ()
  "Saved model/mode should yield closures prepended onto the config."
  (let* ((config '((:identifier . "claude-code")))
         (result (agent-recall--config-with-preferences
                  config '((model . "opus") (permission-mode . "plan")))))
    (should (functionp (map-elt result :default-model-id)))
    (should (functionp (map-elt result :default-session-mode-id)))
    (should (equal "claude-code" (map-elt result :identifier)))))

(ert-deftest test-config-with-preferences-validates-against-live-models ()
  "The model closure should return the id only when the session offers it."
  (let* ((result (agent-recall--config-with-preferences
                  '((:identifier . "claude-code"))
                  '((model . "opus"))))
         (closure (map-elt result :default-model-id)))
    (cl-letf (((symbol-function 'agent-shell--get-available-models)
               (lambda (_state) '(((:model-id . "opus")) ((:model-id . "sonnet"))))))
      (with-temp-buffer
        (setq-local agent-shell--state '((:session . ((:id . "x")))))
        (should (equal "opus" (funcall closure)))))
    (cl-letf (((symbol-function 'agent-shell--get-available-models)
               (lambda (_state) '(((:model-id . "sonnet"))))))
      (with-temp-buffer
        (setq-local agent-shell--state '((:session . ((:id . "x")))))
        (should-not (funcall closure))))))

(ert-deftest test-config-with-preferences-no-metadata-untouched ()
  "Without saved model/mode the config should pass through unchanged."
  (let ((config '((:identifier . "claude-code"))))
    (should (eq config (agent-recall--config-with-preferences config nil)))
    (should (eq config (agent-recall--config-with-preferences
                        config '((label . "just a label")))))))

;; ---------------------------------------------------------------------------
;; Echo-area summary
;; ---------------------------------------------------------------------------

(ert-deftest test-metadata-summary-format ()
  "Summary should render keys human-readably, values verbatim."
  (let ((summary (agent-recall--metadata-summary
                  '((model . "opus") (permission-mode . "plan") (label . "my label")))))
    (should (equal "model: opus  |  permission mode: plan  |  label: my label"
                   (substring-no-properties summary)))))

;; ---------------------------------------------------------------------------
;; Label accessor + display suffix
;; ---------------------------------------------------------------------------

(ert-deftest test-metadata-session-label ()
  "Label accessor returns stored labels, nil for missing/empty/nil id."
  (with-temp-metadata-store
    (should-not (agent-recall-session-label nil))
    (should-not (agent-recall-session-label test-md-session-id))
    (agent-recall-metadata-put test-md-session-id 'label "refactor tangle")
    (should (equal "refactor tangle"
                   (agent-recall-session-label test-md-session-id)))
    ;; An empty label counts as no label.
    (agent-recall-metadata-put test-md-session-id 'label "")
    (should-not (agent-recall-session-label test-md-session-id))))

(ert-deftest test-metadata-label-suffix ()
  "Display suffix is a propertized \"  LABEL\", empty string otherwise."
  (with-temp-metadata-store
    (should (equal "" (agent-recall--label-suffix test-md-session-id)))
    (agent-recall-metadata-put test-md-session-id 'label "wip")
    (let ((suffix (agent-recall--label-suffix test-md-session-id)))
      (should (equal "  wip" (substring-no-properties suffix)))
      (should (eq 'agent-recall-label (get-text-property 2 'face suffix))))))

;; ---------------------------------------------------------------------------
;; Catalogue
;; ---------------------------------------------------------------------------

(ert-deftest test-catalogue-put-get-roundtrip ()
  "Cataloguing stores a timestamp, note, and tags under the session."
  (with-temp-metadata-store
    (should-not (agent-recall-catalogue-get test-md-session-id))
    (agent-recall-catalogue-put test-md-session-id
                                :note "why kept" :tags '("syzygy" "resume"))
    (let ((entry (agent-recall-catalogue-get test-md-session-id)))
      (should (stringp (alist-get 'catalogued entry)))
      (should (equal "why kept" (alist-get 'note entry)))
      (should (equal '("syzygy" "resume") (alist-get 'tags entry))))))

(ert-deftest test-catalogue-uncatalogue-keeps-note-and-tags ()
  "Uncataloguing should preserve the note and tags in metadata."
  (with-temp-metadata-store
    (agent-recall-catalogue-put test-md-session-id
                              :note "why kept" :tags '("syzygy" "resume"))
    (agent-recall-catalogue-remove test-md-session-id)
    (should-not (agent-recall-catalogue-get test-md-session-id))
    (should (equal "why kept"
                   (agent-recall-metadata-get test-md-session-id 'note)))
    (should (equal '("syzygy" "resume")
                   (agent-recall-metadata-get test-md-session-id 'tags)))))

(ert-deftest test-catalogue-resave-refreshes-timestamp-edit-preserves-it ()
  "Re-saving should set a fresh timestamp that later edits preserve."
  (with-temp-metadata-store
    (agent-recall-catalogue-put test-md-session-id
                              :note "why kept" :tags '("syzygy"))
    (agent-recall-metadata-put test-md-session-id 'catalogued
                               "2000-01-01T00:00:00+0000")
    (agent-recall-catalogue-remove test-md-session-id)
    (should-not (agent-recall-metadata-get test-md-session-id 'catalogued))
    (agent-recall-catalogue-put test-md-session-id)
    (should (agent-recall-catalogue-get test-md-session-id))
    (let ((stamp (agent-recall-metadata-get test-md-session-id 'catalogued)))
      (should (stringp stamp))
      (should-not (equal "2000-01-01T00:00:00+0000" stamp))
      (should-not (agent-recall-metadata-get test-md-session-id 'tags))
      (cl-letf (((symbol-function 'format-time-string)
                 (lambda (&rest _args) "2099-01-01T00:00:00+0000")))
        (agent-recall-catalogue-put test-md-session-id :note "updated note"))
      (should (equal "updated note"
                     (agent-recall-metadata-get test-md-session-id 'note)))
      (should (equal stamp
                     (agent-recall-metadata-get test-md-session-id 'catalogued))))))

(ert-deftest test-catalogue-entries-newest-first ()
  "Entries should be ordered by descending catalogue timestamp."
  (with-temp-metadata-store
    (let ((id-01 test-md-session-id)
          (id-03 "session-03")
          (id-02 "session-02"))
      (dolist (id (list id-01 id-03 id-02))
        (agent-recall-catalogue-put id))
      (agent-recall-metadata-put id-01 'catalogued "2026-09-01T00:00:00+0000")
      (agent-recall-metadata-put id-03 'catalogued "2026-09-03T00:00:00+0000")
      (agent-recall-metadata-put id-02 'catalogued "2026-09-02T00:00:00+0000")
      (should (equal (list id-03 id-02 id-01)
                     (mapcar #'car (agent-recall-catalogue-entries)))))))

(ert-deftest test-catalogue-entries-filtered-by-exact-tag ()
  "Tag filtering should normalize a leading # and require an exact match."
  (with-temp-metadata-store
    (agent-recall-catalogue-put test-md-session-id :tags '("syzygy" "resume"))
    (agent-recall-catalogue-put "other-session" :tags '("dotfiles"))
    (should (equal (list test-md-session-id)
                   (mapcar #'car (agent-recall-catalogue-entries "syzygy"))))
    (should (equal (list test-md-session-id)
                   (mapcar #'car (agent-recall-catalogue-entries "#syzygy"))))
    (should-not (agent-recall-catalogue-entries "syz"))))

(ert-deftest test-catalogue-tags-normalised ()
  "Stored tags should be trimmed, lowercase, nonempty, and deduplicated."
  (with-temp-metadata-store
    (agent-recall-catalogue-put test-md-session-id
                              :tags '("#Syzygy" " resume " "syzygy" ""))
    (should (equal '("syzygy" "resume")
                   (agent-recall-metadata-get test-md-session-id 'tags)))))

(ert-deftest test-catalogue-tags-sorted-union ()
  "Tags should form a sorted union of catalogued sessions only."
  (with-temp-metadata-store
    (should-not (agent-recall-catalogue-tags))
    (agent-recall-catalogue-put test-md-session-id :tags '("syzygy" "resume"))
    (agent-recall-catalogue-put "other-session" :tags '("resume" "dotfiles"))
    (should (equal '("dotfiles" "resume" "syzygy")
                   (agent-recall-catalogue-tags)))
    (agent-recall-catalogue-remove test-md-session-id)
    (should (equal '("dotfiles" "resume") (agent-recall-catalogue-tags)))
    (agent-recall-catalogue-remove "other-session")
    (should-not (agent-recall-catalogue-tags))))

(ert-deftest test-catalogue-empty-note-removes-key ()
  "An empty note should remove the note key from an existing entry."
  (with-temp-metadata-store
    (agent-recall-catalogue-put test-md-session-id :note "why kept")
    (agent-recall-catalogue-put test-md-session-id :note "")
    (let ((entry (agent-recall-catalogue-get test-md-session-id)))
      (should entry)
      (should-not (assq 'note entry)))
    (should-not (assq 'note (agent-recall-metadata test-md-session-id)))))

(ert-deftest test-catalogue-get-uncatalogued-stray-note ()
  "A stray note should not make an uncatalogued session a catalogue entry."
  (with-temp-metadata-store
    (agent-recall-metadata-put test-md-session-id 'note "stray note")
    (should-not (agent-recall-catalogue-get test-md-session-id))))

(ert-deftest test-catalogue-remove-get-nil-or-unknown-session ()
  "Removing or getting nil and unknown session IDs should return nil."
  (with-temp-metadata-store
    (dolist (id '(nil "unknown-session"))
      (should-not (agent-recall-catalogue-get id))
      (should-not (agent-recall-catalogue-remove id))
      (should-not (agent-recall-catalogue-get id)))))

(provide 'test-metadata)
;;; test-metadata.el ends here
