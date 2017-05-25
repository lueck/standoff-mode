:; exec emacs -Q --script "$0" -- "$@"

;;; Commentary:
;; WARNING: Close your production files before running tests! Restart
;; Emacs before going from testing to production again, because it is
;; not shure, that all configuration is restored correctly. You might
;; loose data! -- OR BETTER: Run this test file in batch mode.

;;; Code

(when noninteractive
  ;; set load path to . and ..
  (setq standoff-lib-dir (concat (file-name-directory load-file-name) "/.."))
  (push standoff-lib-dir load-path)
  (push (file-name-directory load-file-name) load-path)
  ;; pop "--" from argv
  (setq argv (cdr argv))
  )

(require 'ert)
(require 'standoff-test-utils)
(require 'standoff-json)

(defconst standoff-json-test-debug nil
  "Set this to non-nil, if you would like to get various messages.")

(defconst standoff-json-test-kill-buffers t
  "Kill buffers on tear down.")

;;; Functions for set up and tear down

(defun standoff-json-test-setup-source-buffer ()
  "Save the current configuration and setup testing environment.
Returns a test buffer with some content."
  (let ((test-buffer (generate-new-buffer "json-test")))
    (standoff-test-utils-save-old-config)
    ;; create and fill test buffer
    (set-buffer test-buffer)
    (insert "Was kann das Licht in unſer Auge mahlen?
Was ſich mahlen laͤßt, Bilder. Wie auf der
weißen Wand der dunklen Kammer, ſo faͤllt auf
die Netzhaut des Auges ein Stralenpinſel von
allem, was vor ihm ſtehet, und kann nichts, als
was […] was da ſteht, eine Flaͤche, ein Nebeneinander
aller und der verſchiedenſten ſichtbaren Gegenſtaͤnde
zeichnen. Dinge hinter einander, oder ſolide,
maſſive Dinge als ſolche dem Auge zu geben, iſt
ſo unmoͤglich, als den Liebhaber hinter der dicken
Tapete, den Bauer innerhalb der Windmuͤhle
ſingend zu mahlen.")
    ;; Return test buffer
    test-buffer))

(defun standoff-json-test-teardown (buf)
  "Tear down the test setup for dummy testing."
  (let ((json-buf (standoff-json/file-get-json-buffer buf)))
    (when standoff-json-test-kill-buffers (kill-buffer json-buf))
    (kill-buffer buf)
    (standoff-test-utils-restore-old-config)))

(defun standoff-json-test-positions (buf)
  (when (not (null standoff-json-test-debug))
    (with-current-buffer buf
      (message "JSON Position:")
      (message "MD5 sum: %s"
	       (standoff-json/file-get-or-parse-position "md5sum-start"))
      (message "Markup: %s %s"
	       (standoff-json/file-get-or-parse-position "MarkupRanges-start")
	       (standoff-json/file-get-or-parse-position "MarkupRanges-insert"))
      )))

;;; Tests

(ert-deftest standoff-json-test-get-json-buffer ()
  (let ((source-buffer (standoff-json-test-setup-source-buffer))
	(json-buffer nil)
	(markup-id nil))
    (set-buffer source-buffer)
    (setq json-buffer (standoff-json/file-get-json-buffer source-buffer))
    ;; The buffer is not nil
    (should json-buffer)
    ;; still the same buffer
    (should (equal json-buffer (standoff-json/file-get-json-buffer source-buffer)))
    (standoff-json-test-teardown source-buffer)))

(ert-deftest standoff-json-test-create-read-markup ()
  (let ((source-buffer (standoff-json-test-setup-source-buffer))
	(json-buffer nil)
	(json-buffer-size1 nil)
	(json-buffer-size2 nil)
	(markup-id nil)
	(makrup-id2 nil))
    (set-buffer source-buffer)
    (setq json-buffer (standoff-json/file-get-json-buffer source-buffer))
    (standoff-json-test-positions json-buffer)
    (setq json-buffer-size1 (buffer-size json-buffer))
    ;; reading markup before any was created
    (should (equal '() (standoff-json/file-read-markup source-buffer)))
    ;; create markup elements
    (setq markup-id (standoff-json/file-create-markup source-buffer 23 42 "example"))
    (standoff-json-test-positions json-buffer)
    (should (> (buffer-size json-buffer) json-buffer-size1))
    (setq markup-id (standoff-json/file-create-markup source-buffer 52 64 "marker"))
    (standoff-json-test-positions json-buffer)
    ;; Read
    (setq ranges (standoff-json/file-read-markup source-buffer))
    (should (= 2 (length ranges)))
    (setq ranges (standoff-json/file-read-markup source-buffer nil nil "example"))
    (should (= 1 (length ranges)))
    (setq ranges (standoff-json/file-read-markup source-buffer nil nil nil markup-id))
    (should (= 1 (length ranges)))
    (setq ranges (standoff-json/file-read-markup source-buffer 51 65))
    (should (= 1 (length ranges)))
    (setq ranges (standoff-json/file-read-markup source-buffer 1 19 "example"))
    (should (= 0 (length ranges)))
    (should-error (standoff-json/file-read-markup source-buffer 1))
    (should-error (standoff-json/file-read-markup source-buffer nil 2))
    ;; Add range (discontinous markup)
    (setq markup-id2 (standoff-json/file-add-range source-buffer 67 68 markup-id))
    ;; Read again
    (should (= 3 (length (standoff-json/file-read-markup source-buffer))))
    (should (= 2 (length (standoff-json/file-read-markup source-buffer nil nil nil markup-id))))
    ;; Try adding range with unknow element id
    (setq markup-id2 (standoff-util/create-uuid))
    (should-error (standoff-json/file-add-range source-buffer 67 68 markup-id2))
    
    ;; tear down
    (standoff-json-test-teardown source-buffer)))



;; run tests and exit
(when noninteractive
  (ert-run-tests-batch-and-exit (car argv)))

;;; standoff-json-test.el ends here
