;; WARNING: Close your production files before running tests! Restart
;; Emacs before going from testing to production again, because it is
;; not shure, that all configuration is restored correctly. You might
;; loose data!

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
    (should (= (length (overlays-at 429)) 1))
    (should (= (length (overlays-at 528)) 2))
    ;; 2.c) filter id
    (standoff-hide-markup nil nil nil (standoff-markup-get-number test-buffer "second"))
    (should (= (length (overlays-at 528)) 1))
    ;; 2.d) filter for type
    (standoff-highlight-markup-range test-buffer 484 537 "beispiel" "third")
    (standoff-hide-markup nil nil "beispiel")
    (should (= (length (overlays-at 528)) 1))
    (standoff-test-utils-teardown-source-buffer test-buffer)
    ))

