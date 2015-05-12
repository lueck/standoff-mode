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
  ;; should be a 36 character string
  (should
   (equal (length (standoff-dummy-create-uuid)) 36)))

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
