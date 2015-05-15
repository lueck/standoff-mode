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
  (let ((markup-type (standoff-dummy-markup-get-type-by-inst-id buf markup-inst-id)))
    (unless markup-type
      (error "Invalid ID. No markup type found"))
    (setq standoff-dummy-markup
	  (cons (list markup-inst-id markup-type startchar endchar)
		standoff-dummy-markup))
    markup-inst-id))

(defun standoff-dummy-markup-get-type-by-inst-id (buf markup-inst-id)
  "Return the markup type for the markup element given by
MARKUP-INST-ID. The buffer is given by BUF."
  (let ((markup-data standoff-dummy-markup)
	(markup-inst nil)
	(markup-type nil))
    (while markup-data
      (setq markup-inst (pop markup-data))
      (if (equal markup-inst-id (nth standoff-pos-markup-inst-id markup-inst))
	  (setq markup-type (nth standoff-pos-markup-type markup-inst)
		markup-data nil)))
    markup-type))

(defun standoff-dummy-read-markup (buf &optional startchar endchar markup-type markup-inst-id)
  "Return the markup, apply filter given by STARTCHAR
ENDCHAR MARKUP-TYPE MARKUP-INST-ID."
  (let ((backend standoff-dummy-markup)
	(ranges-to-return '())
	(range))
    (or (and (not startchar) (not endchar))
	(and startchar endchar)
	(error "Either give startchar *and* endchar or neither of them"))  
    (while backend
      (setq range (pop backend))
      ;; when COND
      (and (or (and (not startchar) (not endchar))
	       (or (and (<= (nth standoff-pos-startchar range) startchar)
			(>= (nth standoff-pos-endchar range) startchar))
		   (and (<= (nth standoff-pos-startchar range) endchar)
			(>= (nth standoff-pos-endchar range) endchar))))
	   (or (not markup-type)
	       (equal (nth standoff-pos-markup-type range) markup-type))
	   (or (not markup-inst-id)
	       (equal (nth standoff-pos-markup-inst-id range) markup-inst-id))
	   ;; BODY
	   (setq ranges-to-return (cons range ranges-to-return))))
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

(defconst standoff-pos-subject 0
  "The position of the subject in a list representing a relation
  between markup elements.")

(defconst standoff-pos-predicate 1
  "The position of the predicate in a list representing a
  relation between markup elements.")

(defconst standoff-pos-object 2
  "The position of the object in a list representing a
  relation between markup elements.")

(defun standoff-dummy-used-predicates (buf subj-id obj-id)
  "Returns a list of predicates used for relations between
subjects similar to the one given by SUBJ-ID and objects similar
to the one given by OBJ-ID. Similarity is equivalence of the
subject's and object's markup type respectivly, here."
  (let ((subj-type (standoff-dummy-markup-get-type-by-inst-id buf subj-id))
	(obj-type (standoff-dummy-markup-get-type-by-inst-id buf obj-id))
	(relations standoff-dummy-relations)
	(relation)
	(predicate)
	(predicates '()))
    (while relations
      (setq relation (pop relations)
	    subj (nth standoff-pos-subject relation)
	    predicate (nth standoff-pos-predicate relation)
	    obj (nth standoff-pos-object relation))
      ;; when COND
      (and (equal (standoff-dummy-markup-get-type-by-inst-id buf subj) subj-type)
	   (equal (standoff-dummy-markup-get-type-by-inst-id buf obj) obj-type)
	   (not (member predicate predicates))
	   ;; BODY
	   (setq predicates (cons predicate predicates))))
    predicates))

(defun standoff-dummy-create-relation (buf subj-id predicate obj-id)
  "Create a directed graph with the markup given by SUBJ-ID as
subject, the predicate given by PREDICATE and the markup given by
OBJ-ID as object. The buffer with the markup must be given by
BUF."
  (setq standoff-dummy-relations
	(cons (list subj-id predicate obj-id) standoff-dummy-relations)))

(defun standoff-dummy-read-relations (buf &optional subj-id predicate obj-id)
  "Get all relations in buffer BUF, filtered by a combination of
  subject given by SUBJ-ID, PREDICATE and object given by
  OBJ-ID. A value of `nil' in those positions will be treated as
  a wildcard."
  (let ((data standoff-dummy-relations)
	(relations '())
	(relation))
    (while data
      (setq relation (pop data))
      (and (or (not subj-id)
	       (equal subj-id (nth standoff-pos-subject relation)))
	   (or (not predicate)
	       (equal predicate (nth standoff-pos-predicate relation)))
	   (or (not obj-id)
	       (equal obj-id (nth standoff-pos-object relation)))
	   (setq relations (cons relation relations))))
    relations))

(defun standoff-dummy-delete-relation (buf subj-id predicate obj-id)
  (let ((data standoff-dummy-relations)
	(new-relations '())
	(relation))
    (while data
      (setq relation (pop data))
      (when (not (and (equal (nth standoff-pos-subject relation) subj-id)
		      (equal (nth standoff-pos-predicate relation) predicate)
		      (equal (nth standoff-pos-object relation) obj-id)))
	(setq new-relations (cons relation new-relations))))
    (setq standoff-dummy-relations new-relations)))
	

;; setup and reset backend

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
