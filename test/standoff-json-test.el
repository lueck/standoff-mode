:; exec emacs -Q --script "$0" -- "$@"

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
  (kill-buffer buf)
  (standoff-test-utils-restore-old-config))

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

(ert-deftest standoff-json-test-create-markup ()
  (let ((source-buffer (standoff-json-test-setup-source-buffer))
	(json-buffer nil)
	(json-buffer-size1 nil)
	(json-buffer-size2 nil)
	(markup-id nil))
    (set-buffer source-buffer)
    (setq json-buffer (standoff-json/file-get-json-buffer source-buffer))
    (setq json-buffer-size1 (buffer-size json-buffer))
    (setq markup-id (standoff-json/file-add-markup source-buffer 23 42 "example"))
    (should (> (buffer-size json-buffer) json-buffer-size1))
    (setq markup-id (standoff-json/file-add-markup source-buffer 52 64 "marker"))
    (standoff-json-test-teardown source-buffer)))



;; run tests and exit
(when noninteractive
  (ert-run-tests-batch-and-exit (car argv)))

;;; standoff-json-test.el ends here
