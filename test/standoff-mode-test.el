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

(ert-deftest standoff-markup-types-from-overlay-definition-test ()
  "Testing the function that gets allowed markup types from overlay definitions."
  (standoff-test-utils-save-old-config)
  (standoff-test-utils-setup-overlays)
  (should (member "beispiel" (standoff-markup-types-from-overlay-definition)))
  (should-not (member "fail" (standoff-markup-types-from-overlay-definition)))
  (standoff-test-utils-restore-old-config))
  
(ert-deftest standoff-markup-type-completion-test ()
  "Testing the completion list for markup types.
This list depends on the value of
`standoff-markup-require-name-require-match'."
  (let ((test-buffer (standoff-test-utils-setup-source-buffer)))
    (standoff-test-utils-setup-overlays)
    (setq standoff-markup-type-require-match t)
    (standoff-dummy-create-markup test-buffer 445 482 "example")
    (should (member "beispiel" (standoff-markup-type-completion test-buffer)))
    (should-not (member "example" (standoff-markup-type-completion test-buffer)))
    (setq standoff-markup-type-require-match 'confirm)
    (should (member "example" (standoff-markup-type-completion test-buffer)))
    (setq standoff-markup-type-require-match nil)
    (should (member "example" (standoff-markup-type-completion test-buffer)))
    (should (member "beispiel" (standoff-markup-type-completion test-buffer)))
    (standoff-test-utils-teardown-source-buffer test-buffer)))

(ert-deftest standoff-markup-number-mapping-test ()
  "Testing the mapping of markup IDs to numbers."
  (let ((test-buffer (standoff-test-utils-setup-source-buffer)))
    (standoff-markup-number-mapping-setup)
    ;; should first create the numer and then subsequently get the same
    (setq number-test1 (standoff-markup-get-number test-buffer "test1"))
    (should (= (standoff-markup-get-number test-buffer "test1") number-test1))
    ;; number should be of type number 
    (should (numberp number-test1))
    ;; should increment by 1
    (setq number-test2 (standoff-markup-get-number test-buffer "test2"))
    (should (= number-test2 (+ number-test1 1)))
    ;; should get markup ID for number
    (should (equal (standoff-markup-get-by-number test-buffer number-test1) "test1"))
    ;; should get nil
    (should-not (standoff-markup-get-by-number test-buffer 23))
    (standoff-test-utils-teardown-source-buffer test-buffer)))

(ert-deftest standoff-markup-highlightning-test ()
  "Testing highlightning."
  (let ((test-buffer (standoff-test-utils-setup-source-buffer)))
    (standoff-markup-number-mapping-setup)
    ;; 1. Create highlightning
    (standoff-highlight-markup-range test-buffer 445 483 "example" "first")
    (standoff-highlight-markup-range test-buffer 528 537 "example" "first")
    (standoff-highlight-markup-range test-buffer 484 537 "example" "second")
    ;; 1.a) overlays should be created
    (should (= (length (overlays-at 445)) 1))
    ;; 1.b) should not create a similar overlay again
    (standoff-highlight-markup-range test-buffer 445 483 "example" "first")
    (should (= (length (overlays-at 445)) 1))
    ;; 1.c) should create overlay on the same range if not similar
    (make-overlay 429 439)
    (standoff-highlight-markup-range test-buffer 429 439 "example" "first")
    (should (= (length (overlays-at 429)) 2))
    (standoff-highlight-markup-range test-buffer 429 439 "example" "second")
    (should (= (length (overlays-at 429)) 3))
    ;; 2. Hide markup / Delete highlightning ;;
    ;; 2.a) should not delete non-standoff overlays +
    ;; 2.b) filter region
    (standoff-hide-markup 429 430)
    (should (= (length (overlays-at 528)) 2))
    ;; non-standoff overlays should stay
    (should (= (length (overlays-at 429)) 1))
    ;; 2.c) filter id
    (standoff-hide-markup nil nil nil (standoff-markup-get-number test-buffer "second"))
    (should (= (length (overlays-at 528)) 1))
    ;; non-standoff overlays should stay
    (should (= (length (overlays-at 429)) 1))
    ;; 2.d) filter for type
    (standoff-highlight-markup-range test-buffer 484 537 "beispiel" "third")
    (standoff-hide-markup nil nil "beispiel")
    (should (= (length (overlays-at 528)) 1))
    ;; non-standoff overlays should stay
    (should (= (length (overlays-at 429)) 1))
    ;; 3. Select
    ;; should not find non-standoff overlays
    (should-error (standoff-highlight-markup--select 430))
    (should (= (length (overlays-at 528)) 1))
    ;; should find
    (should (standoff-highlight-markup--select 528))
    (standoff-highlight-markup-range test-buffer 484 537 "escalmpel" "fourth")
    ;; should fail because ambiguous
    (should-error (standoff-highlight-markup--select 528))
    (standoff-test-utils-teardown-source-buffer test-buffer)
    ))

(ert-deftest standoff-markup-highlightning-with-backend-test ()
  "Test highlightning markup from dummy backend."
  (let ((test-buffer (standoff-test-utils-setup-source-buffer)))
    (standoff-markup-number-mapping-setup)
    ;; create some markup in the backend
    (setq id1 (standoff-dummy-create-markup test-buffer 445 483 "example"))
    (standoff-dummy-add-range test-buffer 528 537 id1)
    (standoff-dummy-add-range test-buffer 429 439 id1)
    (setq id2 (standoff-dummy-create-markup test-buffer 484 537 "example"))
    (standoff-dummy-add-range test-buffer 429 439 id2)
    (setq id3 (standoff-dummy-create-markup test-buffer 426 444 "marker"))
    ;; should highlight all
    (standoff-highlight-markup)
    (should (= (length (overlays-at 429)) 3))
    ;; should hide 
    (standoff-hide-markup-region 429 430)
    (should (= (length (overlays-at 429)) 0))
    (should (= (length (overlays-at 528)) 2))
    (standoff-hide-markup-by-number id1)
    (should (= (length (overlays-at 528)) 1))
    (goto-char 528)
    (standoff-hide-markup-at-point)
    (should (= (length (overlays-at 528)) 0))
    ;; testing buffer wide functions
    (standoff-highlight-markup-buffer "marker")
    (should (= (length (overlays-at 429)) 1))
    (standoff-highlight-markup-buffer "!")
    (should (= (length (overlays-at 429)) 3))
    (standoff-hide-markup-buffer "marker")
    (should (= (length (overlays-at 429)) 2))
    (standoff-hide-markup-buffer "!")
    (should (= (length (overlays-at 429)) 0))
    ;; testing regional functions
    (standoff-highlight-markup-region 1 429 "marker")
    (should (= (length (overlays-at 429)) 1))
    (standoff-highlight-markup-region 1 429 "!")
    (should (= (length (overlays-at 429)) 3))
    (should (= (length (overlays-at 528)) 0))
    (standoff-highlight-markup-buffer)
    (standoff-hide-markup-region 1 429 "marker")
    (should (= (length (overlays-at 429)) 2))
    ;; see what overlapping means!
    (standoff-hide-markup-region 1 429 "!")
    (should (= (length (overlays-at 429)) 2))
    (standoff-hide-markup-region 1 430 "!")
    (should (= (length (overlays-at 429)) 0))
    (should (= (length (overlays-at 528)) 2))
    ;; testing numerical functions
    (let ((n3 (standoff-markup-get-number test-buffer id3)))
      (standoff-highlight-markup-by-number n3)
      ;;(should (= (length (overlays-at 429)) 1));; Fixme!
      (standoff-highlight-markup-buffer)
      (standoff-hide-markup-by-number n3)
      (should (= (length (overlays-at 429)) 2))
      )
    (standoff-test-utils-teardown-source-buffer test-buffer)))

(ert-deftest standoff-markup-creation-deletion-test ()
  "Test highlightning markup from dummy backend."
  (let ((test-buffer (standoff-test-utils-setup-source-buffer))
	(n1)
	(standoff-y-or-n-p t))
    (standoff-markup-number-mapping-setup)
    ;; 1. standoff-markup-region
    (standoff-markup-region 445 483 "example")
    ;; should have stored markup to the backend
    (should (= (length (funcall standoff-markup-read-function test-buffer)) 1))
    ;; should have highlightened markup
    (should (= (length (overlays-at 445)) 1))
    ;; 2. standoff-markup-region-continue
    ;; get the number
    (setq n1 (standoff-markup-get-number test-buffer (nth standoff-pos-markup-inst-id (car (funcall standoff-markup-read-function test-buffer))))) 
    (standoff-markup-region-continue 528 537 n1)
    ;; should have stored to backend
    (should (= (length (funcall standoff-markup-read-function test-buffer)) 2))
    ;; should be of same type
    (should
     (equal
      (nth standoff-pos-markup-type (car (funcall standoff-markup-read-function test-buffer)))
      (nth standoff-pos-markup-type (cadr (funcall standoff-markup-read-function test-buffer)))))
    ;; should have highlightened markup
    (should (= (length (overlays-at 528)) 1))
    ;; 3. standoff-markup-delete-range-at-point
    (standoff-markup-delete-range-at-point 445)
    ;; should have removed from the backend
    (should (= (length (funcall standoff-markup-read-function test-buffer)) 1))    
    ;; should have removed the overlay
    (should (= (length (overlays-at 445)) 0))
    (standoff-test-utils-teardown-source-buffer test-buffer)))

;; run tests and exit
(when noninteractive
  (ert-run-tests-batch-and-exit (car argv)))
