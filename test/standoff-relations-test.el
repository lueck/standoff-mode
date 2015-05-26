:; exec emacs -Q --script "$0" -- "$@"

;; WARNING: Close your production files before running tests! Restart
;; Emacs before going from testing to production again, because it is
;; not shure, that all configuration is restored correctly. You might
;; loose data! -- OR BETTER: Run this test file in batch mode.

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
(require 'standoff-mode)

;; Tests

(ert-deftest standoff-relations--markup-string-test ()
  (let ((test-buffer (standoff-test-utils-setup-source-buffer))
	(markup-id)
	(ranges))
    (setq markup-id (funcall standoff-markup-create-function test-buffer 1 9 "w-frage"))
    (funcall standoff-markup-range-add-function test-buffer 34 41 markup-id)
    (setq ranges (funcall standoff-markup-read-function test-buffer nil nil nil markup-id))
    (should (equal (standoff-relations--markup-string ranges) "Was kann … mahlen?"))
    (standoff-test-utils-teardown-source-buffer test-buffer)))

;; run tests and exit
(when noninteractive
  (ert-run-tests-batch-and-exit (car argv)))

;;; standoff-relations-test.el ends here.
