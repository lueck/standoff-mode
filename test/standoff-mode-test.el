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
  
