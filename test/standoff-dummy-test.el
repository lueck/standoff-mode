(require 'standoff-dummy)

(ert-deftest standoff-dummy-markup-create-test ()
  (let ((test-buffer (generate-new-buffer "dummy-test"))
	(marrkup-id nil))
    (set-buffer test-buffer)
    ;;(setq standoff-dummy-create-id-function 'standoff-dummy-create-uuid)
    (standoff-dummy--backend-reset)
    (setq markup-id (standoff-dummy-create-markup test-buffer 23 42 "example"))
    ;; should be appended to backend
    (should (= (length standoff-dummy-markup) 1))
    ;; should have a return value of defined type
    (should
     (equal
      (type-of markup-id)
      (type-of (funcall standoff-dummy-create-id-function standoff-dummy-markup 0))))
    ;;(standoff-dummy-backend-inspect)
    (kill-buffer test-buffer)))

(ert-deftest standoff-dummy-create-uuid-test ()
  ;; return value should be a string
  (should
   (equal (type-of (standoff-dummy-create-uuid)) 'string))
  ;; should match xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx$
  (should
   (equal (string-match "[[:xdigit:]]\\{8\\}-[[:xdigit:]]\\{4\\}-[[:xdigit:]]\\{4\\}-[[:xdigit:]]\\{4\\}-[[:xdigit:]]\\{12\\}$" (standoff-dummy-create-uuid)) 0)))

(ert-deftest standoff-dummy-create-intid-test ()
  (let ((test-buffer (generate-new-buffer "dummy-test")))
    (set-buffer test-buffer)
    (setq standoff-dummy-create-id-function 'standoff-dummy-create-intid)
    (standoff-dummy--backend-reset)
    ;; test create function
    (should
     (=
      (standoff-dummy-create-intid standoff-dummy-markup 0)
      1))
    ;; test funcall
    (should
     (=
      (funcall standoff-dummy-create-id-function standoff-dummy-markup 0)
      1))
    ;; test markup creation 
    (should
     (= 
      (standoff-dummy-create-markup test-buffer 23 42 "example")
      1))
    ;; subsequent creation of markup should increment id
    (should
     (= 
      (standoff-dummy-create-markup test-buffer 23 42 "example")
      2))    
    (kill-buffer test-buffer)))
					  
(ert-deftest standoff-dummy-add-range-test ()
  (let ((test-buffer (generate-new-buffer "dummy-test"))
	(markup-id nil))
    (set-buffer test-buffer)
    (setq standoff-dummy-create-id-function 'standoff-dummy-create-intid)
    (standoff-dummy--backend-reset)
    (setq markup-id (standoff-dummy-create-markup test-buffer 23 42 "example"))
    ;; test return value
    (should
     (= (standoff-dummy-add-range test-buffer 47 49 markup-id)
	markup-id))
    ;; test if in backend
    (should (= (length standoff-dummy-markup) 2))
    ;; should have the same markup-type
    (should (equal (nth 1 (car standoff-dummy-markup))
		   (nth 1 (cadr standoff-dummy-markup))))
    (should (equal (nth 1 (car standoff-dummy-markup)) "example"))
    (kill-buffer test-buffer)))

(ert-deftest standoff-dummy-delete-range-test ()
  (let ((test-buffer (generate-new-buffer "dummy-test"))
	(markup-id nil))
    (set-buffer test-buffer)
    (setq standoff-dummy-create-id-function 'standoff-dummy-create-intid)
    (standoff-dummy--backend-reset)
    (setq markup-id (standoff-dummy-create-markup test-buffer 23 42 "example"))
    (standoff-dummy-create-markup test-buffer 16 22 "marker")
    (standoff-dummy-add-range test-buffer 47 49 markup-id)
    (should (equal (length standoff-dummy-markup) 3))
    (standoff-dummy-delete-range test-buffer 47 49 "example" markup-id)
    (should (equal (length standoff-dummy-markup) 2))
    ;; delete inexistant markup range
    (should-error (standoff-dummy-delete-range test-buffer 23 24 "example" markup-id))
    (should (equal (length standoff-dummy-markup) 2))
    (kill-buffer test-buffer)))

(ert-deftest standoff-dummy-read-markup-test ()
  (let ((test-buffer (generate-new-buffer "dummy-test"))
	(markup-id nil))
    (set-buffer test-buffer)
    (setq standoff-dummy-create-id-function 'standoff-dummy-create-intid)
    (standoff-dummy--backend-reset)
    (setq markup-id (standoff-dummy-create-markup test-buffer 23 42 "example"))
    (standoff-dummy-create-markup test-buffer 16 27 "marker")
    (standoff-dummy-add-range test-buffer 47 49 markup-id)
    (should (= (length (standoff-dummy-read-markup test-buffer)) 3))
    (should (= (length (standoff-dummy-read-markup test-buffer nil nil "example")) 2))
    (should (= (length (standoff-dummy-read-markup test-buffer nil nil nil markup-id)) 2))
    ;; test startchar / endchar boundaries
    (should (= (length (standoff-dummy-read-markup test-buffer 18 22)) 1))
    (should (= (length (standoff-dummy-read-markup test-buffer 1 22)) 1))
    (should (= (length (standoff-dummy-read-markup test-buffer 48 55)) 1))
    (should (= (length (standoff-dummy-read-markup test-buffer 22 32)) 2))
    ;; should return the same if startchar < endchar
    (should (= (length (standoff-dummy-read-markup test-buffer 32 22)) 2))
    (should-error (standoff-dummy-read-markup test-buffer 22))
    ;; combinations of range and markup-type and markup-inst-id 
    (should (= (length (standoff-dummy-read-markup test-buffer 22 32 "example")) 1))
    (should (= (length (standoff-dummy-read-markup test-buffer 22 32 nil markup-id)) 1))
    (should (= (length (standoff-dummy-read-markup test-buffer 22 32 "marker" markup-id)) 0))
    (kill-buffer test-buffer)))

(ert-deftest standoff-dummy-markup-types-test ()
  (let ((test-buffer (generate-new-buffer "dummy-test"))
	(markup-id nil))
    (set-buffer test-buffer)
    (setq standoff-dummy-create-id-function 'standoff-dummy-create-intid)
    (standoff-dummy--backend-reset)
    (setq markup-id (standoff-dummy-create-markup test-buffer 23 42 "example"))
    (standoff-dummy-create-markup test-buffer 16 27 "marker")
    (standoff-dummy-add-range test-buffer 47 49 markup-id)
    ;; "example" and "marker" should be member of returned list
    (should (member "example" (standoff-dummy-markup-types test-buffer)))
    (should (member "marker" (standoff-dummy-markup-types test-buffer)))
    (should-not (member "fail" (standoff-dummy-markup-types test-buffer)))
    ;; duplicates should be removed
    (should (= (length (standoff-dummy-markup-types test-buffer)) 2))
    (kill-buffer test-buffer)))
    
