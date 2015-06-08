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
(require 'standoff-api)

(defun standoff-test-evolve-make-value (buf item data-symbol cell old-api)
  "supplement")

(ert-deftest standoff-api-evolve-from-first-test ()
  "Test evolution of api from \"first\" version to the current."
  (let* ((old-markup '((1 "example" 0 4 "Hello")
		       (2 "example" 6 10 "World")))
	 (old-relations '((1 "continues" 2)))
	 (standoff-api-evolve-make-value-function 'standoff-test-evolve-make-value)
	 (old-api (cdr (assoc "first" standoff-api-generations)))
	 (test-buffer nil)
	 (new-markup (standoff-api-evolve test-buffer :markup old-markup old-api))
	 (new-relations (standoff-api-evolve test-buffer :relations old-relations old-api))
	 )
    (should-not (equal old-markup new-markup))
    (should (equal (car new-markup) '(1 "example" 0 4 "Hello" "supplement" "supplement")))
    (should-not (equal old-relations new-relations))
    (should (equal (nth 0 (car new-relations)) "supplement"))
    (should (equal (nth 1 (car new-relations)) (nth 0 (car old-relations))))
    (should (equal (nth 2 (car new-relations)) (nth 1 (car old-relations))))
    (should (equal (nth 3 (car new-relations)) (nth 2 (car old-relations))))
    (standoff-test-utils-teardown-source-buffer test-buffer)))

;; run tests and exit
(when noninteractive
  (ert-run-tests-batch-and-exit (car argv)))

;;; standoff-api-test.el ends here.

