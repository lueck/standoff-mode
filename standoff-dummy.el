;;
;; Dummy backend
;;

(defconst standoff-pos-markup-inst-id 0
  "Position of the ID of the markup instance in a list
  representing a markup element (or a range of a markup element
  in the case of discontinous markup).

  Example of a list representing a markup element:
  '(1 \"example\" 23 42 &rest)

  This represents the markup element with the ID 1, its type is
  \"example\" and it's ranging from character 23 to 42.")

(defconst standoff-pos-markup-type 1
  "Position of the markup type in a list representing a markup
  element (or a range of a markup element in the case of
  discontinous markup)")

(defconst standoff-pos-startchar 2
  "Position of the offset of the starting character in a list
  representing a markup element (or a range of a markup element
  in the case of discontinous markup)")

(defconst standoff-pos-endchar 3
  "Position of the offset of the end character in a list
  representing a markup element (or a range of a markup element
  in the case of discontinous markup)")

(defun standoff-dummy-create-markup (buf startchar endchar markup-type)
  (let ((markup-inst-id (funcall standoff-dummy-create-id-function
				 standoff-dummy-markup
				 standoff-pos-markup-inst-id)))
    (setq standoff-dummy-markup
	  (cons (list markup-inst-id markup-type startchar endchar)
		standoff-dummy-markup))
    markup-inst-id))

(defun standoff-dummy-add-range (buf startchar endchar markup-inst-id)
  (let ((markup-data standoff-dummy-markup)
	(markup-type nil))
    (while markup-data
      (if (equal markup-inst-id (nth standoff-pos-markup-inst-id (car markup-data)))
	  (progn
	    (setq markup-type (nth standoff-pos-markup-type (car markup-data)))
	    (setq markup-data nil))
	(setq markup-data (cdr markup-data))))
    (unless markup-type
      (error "Invalid ID"))
    (setq standoff-dummy-markup (cons (list markup-inst-id markup-type startchar endchar)
				      standoff-dummy-markup))
    markup-inst-id))

(defun standoff-dummy-read-markup (buf &optional startchar endchar markup-type markup-inst-id)
  "Return the markup, apply filter given by STARTCHAR
ENDCHAR MARKUP-TYPE MARKUP-INST-ID."
  (let ((backend standoff-dummy-markup)
	(ranges-to-return '())
	(range))
    (or (and (not startchar) (not endchar))
	(and startchar endchar)
	(error "Ether give startchar and endchar or nether of them"))  
    (while backend
      (setq range (car backend))
      (when (and (or (and (not startchar) (not endchar))
		     (or (and (<= (nth standoff-pos-startchar range) startchar)
			      (>= (nth standoff-pos-endchar range) startchar))
			 (and (<= (nth standoff-pos-startchar range) endchar)
			      (>= (nth standoff-pos-endchar range) endchar))))
		 (or (not markup-type)
		     (equal (nth standoff-pos-markup-type range) markup-type))
		 (or (not markup-inst-id)
		     (equal (nth standoff-pos-markup-inst-id range) markup-inst-id)))
	(setq ranges-to-return (cons range ranges-to-return)))
      (setq backend (cdr backend)))
    ranges-to-return))

(defun standoff-dummy-delete-range (buf startchar endchar markup-type markup-inst-id)
  "Delete a markup range from the dummy backend."
  (let ((old-markup standoff-dummy-markup) ;; make error tolerant
	(old-length (length standoff-dummy-markup))
	(new-markup '())
	(range))
    (while old-markup
      (setq range (car old-markup))
      (when (not (and (equal (nth standoff-pos-markup-inst-id range) markup-inst-id)
		      (equal (nth standoff-pos-markup-type range) markup-type)
		      (equal (nth standoff-pos-startchar range) startchar)
		      (equal (nth standoff-pos-endchar range) endchar)))
	(setq new-markup (cons range new-markup)))
      (setq old-markup (cdr old-markup)))
    (if (= (length new-markup) old-length)
	(error "No markup found")
      (setq standoff-dummy-markup new-markup))))

(defun standoff-dummy-markup-types (buf)
  "Return a list of the types of markup used in buffer BUF."
  (let ((markup standoff-dummy-markup)
	(typel)
	(used-types '()))
    (while markup
      (setq typel (nth standoff-pos-markup-type (pop markup)))
      (unless (member typel used-types)
	(setq used-types (cons typel used-types))))
    used-types))

(defun standoff-dummy-create-intid (data pos)
  "Create an integer ID for the next item in the dummy backend,
where the item's list is given by DATA and should ether be
standoff-dummy-markup or standoff-dummy-relations, and POS gives
the position (column) of the ID in lists, data is composed
of. E.g. 0 is the POS of the ids in standoff-dummy-markup."
  (if data
      (+ (apply 'max (mapcar '(lambda (x) (nth pos x)) data)) 1)
    1))

(defun standoff-dummy-create-uuid (&optional data pos)
  "Create a UUID. This uses a simple hashing of variable data.
Example of a UUID: 1df63142-a513-c850-31a3-535fc3520c3dq

Note: this code uses https://en.wikipedia.org/wiki/Md5 , which is
not cryptographically safe. I'm not sure what's the implication
of its use here.

Written by Christopher Wellons, 2011, edited by Xah Lee and
other, taken from
http://ergoemacs.org/emacs/elisp_generate_uuid.html
"
  (let ((myStr (md5 (format "%s%s%s%s%s%s%s%s%s%s"
                            (user-uid)
                            (emacs-pid)
                            (system-name)
                            (user-full-name)
                            (current-time)
                            (emacs-uptime)
                            (garbage-collect)
                            (buffer-string)
                            (random)
                            (recent-keys)))))
    (format "%s-%s-4%s-%s%s-%s"
                    (substring myStr 0 8)
                    (substring myStr 8 12)
                    (substring myStr 13 16)
                    (format "%x" (+ 8 (random 4)))
                    (substring myStr 17 20)
                    (substring myStr 20 32))))
 
(defcustom standoff-dummy-create-id-function 'standoff-dummy-create-uuid
  "The function for creating IDs."
  :group 'standoff
  :type 'function)

(defun standoff-dummy-predicate-names (buf subj-id obj-id)
  "Returns a list of predicate names for relations between
subject given by SUBJ-ID and object given by OBJ-ID. Currently
this does not filter valid relation names for combination of
subject and object."
  (mapcar '(lambda (x) (nth 1 x)) standoff-dummy-relations))

(defun standoff-dummy-write-relation (buf subj-id predicate obj-id)
  "Store relation to the backend."
  (setq standoff-dummy-relations
	(cons (list subj-id predicate obj-id) standoff-dummy-relations)))

(defun standoff-dummy-backend-reset ()
  "Reset the dummy backend. It may be usefull during development
to make this an interactive function."
  (interactive)
  (when (yes-or-no-p "Do you really want to reset this buffer, which means that all markup information will be lost?")
    (standoff-dummy--backend-reset)))

(defun standoff-dummy--backend-reset ()
  "Reset the dummy backend."
  (setq-local standoff-dummy-checksum nil)
  (setq-local standoff-dummy-markup '())
  (setq-local standoff-dummy-relations '()))

(defun standoff-dummy-backend-setup ()
  ;; TODO: read from file if present
  (standoff-dummy-backend-reset))

(add-hook 'standoff-mode-hook 'standoff-dummy-backend-setup)

(defun standoff-dummy-backend-inspect ()
  "Display the dummy backend in the minibuffer. This may be
usefull for development."
  (interactive)
  (message "%s" (list standoff-dummy-checksum
		      standoff-dummy-markup
		      standoff-dummy-relations)))
