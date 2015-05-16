;; standoff.el -- An Emacs mode for stand-off markup

(require 'standoff-dummy)

;;
;; API for backends
;;

(defvar standoff-markup-create-function 'standoff-dummy-create-markup
  "The function that writes a markup element to some backend.
This variable must be set to the function's symbol (name). The
function must take the following arguments:

BUFFER STARTCHAR ENDCHAR MARKUP-NAME

The function is expected to return the ID of the markup
element. When storing to the backend was not successfull, it
should return nil.")

(defvar standoff-markup-range-add-function 'standoff-dummy-add-range
  "The function that adds a range to a markup element in some backend.
This variable must be set to the function's symbol (name). The
function must take the following arguments:

BUFFER STARTCHAR ENDCHAR MARKUP-INST-ID

The function is expected to return the ID of the markup
element. When storing to the backend was not successfull, it
should return nil.")

(defvar standoff-markup-read-function 'standoff-dummy-read-markup
  "The function that gets the markup from some backend.
This variable must be set to the function's symbol (name). The
function must take the following arguments:

BUFFER &optional STARTCHAR ENDCHAR MARKUP-NAME MARKUP-INST-ID

The optional parameters should be used for filtering the returned
markup. If STARTCHAR *and* ENDCHAR are numerical values, the
function should return only markup elements that overlap this
portion of BUFFER. If MARKUP-NAME is given, only markup elements
of this type should be returned. If MARKUP-INST-ID is given, only
the markup with this id should be returned. So, if none of
STARTCHAR, ENDCHAR, MARKUP-NAME and MARKUP-ID is given (or all of
them are nil), the function should return all markup element
ranges for the buffer.

The function is expected to return a list of markup ranges which
are again represented as true lists, as follows:

(markup markup markup ...)

where markup takes the form of a true list as

(MARKUP-INST-ID MARKUP-TYPE STARTCHAR ENDCHAR &rest)

If a markup element consists of more than one
range--i.e. discontinous markup--the same values for MARKUP-NAME
and MARKUP-INST-ID must occur in more than one of those markup
lists.")

(defvar standoff-markup-delete-range-function 'standoff-dummy-delete-range
  "The function that deletes (a range of a) markup from some backend.
This variable must be set to the function's symbol (name). The
function should delete a markup element or only a range of this
markup element in case of discountinous markup. The element or
range respectively is given by the following parameters all of
which the function must take:

BUFFER STARTCHAR ENDCHAR MARKUP-NAME MARKUP-INST-ID

The function should return nil or throw an error if the range
could not be deleted and t on successfull deletion. It's up to
the backend to control deletion preconditions which might be:

- any relation to other markup elements. If the markup element
  would consist of only one range then the relation would get
  invalid after deletion. The backend may interact with the user
  in this case.

")

(defvar standoff-markup-types-used-function 'standoff-dummy-markup-types
  "The function that returns names the types of markup in use.
This variable must be set to the function's symbol (name). The
function must take the following arguments:

BUFFER

The function ist expected to return a true list of markup
types.")

(defvar standoff-markup-changed-functions nil
  "A hook for handlers that need to be called if the markup on a
buffer was changed. This can be used for evalution, updating the
highlightning etc. etc.

In terms of elisp this is a so called abnormal hook, i.e. the
hooked functions must take arguments. Arguments are:

BUFFER

BUFFER: the buffer the markup relates to, aka the source document

The return value of the hooked functions is not evaluated at all.")

(defvar standoff-predicates-used-function 'standoff-dummy-used-predicates
  "The function which returns a list of used relation predicates from some backend. 
This variable must be set to the function's symbol (name). The
function must take the following arguments:

BUFFER SUBJECT-ID OBJECT-ID 

The returned list of predicate names must be valid predicate
names for the combination of subject and object. Should return
nil or an empty true list if there are no valid predicates for
this combination.")

(defvar standoff-relation-create-function 'standoff-dummy-create-relation
  "The function which writes a new relation to some backend.
This variable must be set to the function's symbol (name). The
function must take the following arguments:

BUFFER SUBJECT-ID PREDICATE-NAME OBJECT-ID

The function is expected to return a non-nil value, if writing
the relation to the backend was successful, nil in case of
failure.")

(defvar standoff-relations-read-function 'standoff-dummy-read-relations
  "The function which reads relations from some backend. 
This variable must be set to the function's symbol (name). The
function must take the following arguments:

BUFFER &optional SUBJ-ID PREDICATE OBJ-ID

The optional arguments should be interpreted as filter parameters
and a value of nil in either of them should be interpreted as a
wildcard.")

(defvar standoff-relations-delete-function 'standoff-dummy-delete-relation
  "The function that deletes a relation from some backend.
This variable must be set to the function's symbol (name). The
function must take the following arguments:

BUFFER SUBJECT-ID PREDICATE OBJECT-ID

The relation that is to be deleted is given by the three last
arguments. All duplicates of the relation should be removed.")

;;
;; creating and deleting markup
;;

(defcustom standoff-markup-post-functions nil
  "A hook for handlers called when markup was successfully stored to some backend.
This hook can be used for notifications or to set some state. It
is a so called abnormal hook, cf. Info node `(emacs) Hooks',
because the hooked functions (aka handlers) must take the
following arguments:

BUFFER STARTCHAR ENDCHAR MARKUP-NAME MARKUP-INST-ID

"
  :group 'standoff
  :type 'hook
  :options '('standoff-markup-notify))

(defcustom standoff-markup-type-require-match 'confirm
  "Defines how restrictive the markup schema is handled.
This has effect when adding new markup and choosing its type. If
set to `t' then the entered type must be amongst the members of
the list returned by `standoff-markup-types-allowed-function'. If
set to `nil', the user may exit his input with any name. If set
to `confirm' (symbol), the user may exit with any name, but is
asked to confirm his input."
  :group 'standoff)

(defcustom standoff-markup-types-allowed-function 'standoff-markup-types-from-overlay-definition
  "The function that returns a list of allowed markup types.
This variable must be set to the function's symbol (name)."
  :group 'standoff
  :type 'function
  :options '('standoff-markup-types-from-overlay-definition))

(defcustom standoff-markup-overlays '()
  "The overlay definition. This should be defined by the user."
  :group 'standoff)

(defun standoff-markup-types-from-overlay-definition ()
  "Return the list of user defined markup elements. This would be
a good alternative for standoff-dummy-markup-element-names as the
standoff-markup-names-function. "
  (mapcar 'car standoff-markup-overlays))

(defun standoff-markup-type-completion (buf)
  "Returns a list of completions for the markup type.
Depending on `standoff-markup-type-require-match' the list is
composed of a markup from a schema definition and markup types
used in the (current) buffer BUF."
  (cond ((equal standoff-markup-type-require-match t)
	 (funcall standoff-markup-types-allowed-function))
	(t ;; 'confirm OR nil
	 (append (funcall standoff-markup-types-used-function buf)
		 (funcall standoff-markup-types-allowed-function)))))

(defun standoff-markup-region (beg end markup-type)
  "Create markup for the selected region.
The region is given by BEG and END, the type of the markup is
given by MARKUP-TYPE. The id is automatically assigned by the
backend, e.g. by automatic incrementation of an integer."
  (interactive
   (list (region-beginning)
	 (region-end)
	 (completing-read
	  "Name of markup element: "
	  (standoff-markup-type-completion (current-buffer))
	  nil
	  standoff-markup-require-name-require-match)))
  (let ((markup-id nil))
    (save-restriction
      (widen)
      (save-excursion
	(setq markup-id (funcall standoff-markup-create-function
				 (current-buffer) beg end markup-type))
	(when markup-id
	  ;; highlight the new markup
	  (standoff-highlight-markup-range (current-buffer) beg end markup-type markup-id)
	  ;; run hook to notify success
	  (run-hook-with-args 'standoff-markup-post-functions
			      (current-buffer) beg end markup-type markup-id))))
    (deactivate-mark)))

(defun standoff-markup-region-continue (beg end markup-id)
  "Add selected region as a new range continueing an existing markup element.
The markup element is identified by MARKUP-ID. The range is given
by BEG and END or point and mark, aka the region. This function
enables the user to create discontinues markup."
  (interactive "r\nNIdentifying number of markup element to be continued: ")
  (let* ((markup-type (nth standoff-pos-markup-type (car (funcall standoff-markup-read-function (current-buffer) nil nil nil markup-id))))
	 (duplicate (funcall standoff-markup-read-function (current-buffer) beg end markup-type markup-id))
	 (markup-id-from-backend nil))
    (if (not markup-type)
	(error "No markup element with ID %i found" markup-id)
      (if duplicate
	  (error "Overlapping markup with the same id and element name! Not creating a duplicate")
	(save-restriction
	  (widen)
	  (save-excursion
	    (setq markup-id-from-backend (funcall standoff-markup-range-add-function (current-buffer) beg end markup-id))
	    ;;(message "Hi: %s" markup-id-from-backend)
	    (when markup-id-from-backend
	      ;; highlight the new markup
	      (standoff-highlight-markup-range (current-buffer) beg end markup-type markup-id-from-backend)
	      ;; run hook to notify success
	      (run-hook-with-args 'standoff-markup-post-functions (current-buffer) beg end markup-type markup-id-from-backend))))
	(deactivate-mark)))))

(defun standoff-markup-notify (buf startchar endchar markup-type markup-id)
  "Notify the creation of a markup element or range.
This is a handler function that can be hooked to the
`standoff-markup-post-functions' hook, which is called when ever
an markup is being done. This function does nothing but notify
the user with a message in the minibuffer (and in the *Messages*
buffer)."
  (message "Annotating from %i to %i as %s with id %i." 
	   startchar endchar markup-type markup-id))

(add-hook 'standoff-markup-post-functions 'standoff-markup-notify)

(defun standoff-markup-delete-range-at-point ()
  "Delete the range of a markup element at point.
The range is identified by the overlay's properties. So this
works only if there is one and exactly one overlay."
;; TODO: Collect information about related items when
;; asking "Do you really ...? (yes or no)"
  (interactive)
  (save-restriction
    (widen)
    (overlay-recenter (point))
    (let* ((overlays (overlays-at (point)))
	   (ovly (car overlays))
	   (startchar)
	   (endchar)
	   (markup-name)
	   (markup-id)
	   (markup-ranges)
	   (precondition)
	   (deleted))
      (if (> (length overlays) 1)
	  (error "There is more than one markup element at point. Not deleting anything")
	(setq startchar (overlay-start ovly))
	(setq endchar (overlay-end ovly))
	(setq markup-name (standoff--overlay-property-get ovly "name"))
	(setq markup-id (string-to-number (standoff--overlay-property-get ovly "id")))
	;; (message (format "%i %i %s %i" startchar endchar markup-name markup-id))
	(setq markup-ranges (funcall standoff-markup-read-ranges-function (current-buffer) nil nil nil markup-id))
	(message "%s" (length markup-ranges))
	(if (> (length markup-ranges) 1)
	    (setq precondition (y-or-n-p (format "Do you really want to delete this range of '%s' %s? " markup-name markup-id)))
	  (setq precondition (yes-or-no-p (format "Do you really want to delete markup element %s, which is a '%s', and all it's related items? " markup-id markup-name))))
	(when precondition
	  (setq deleted (funcall standoff-markup-delete-range-function (current-buffer) startchar endchar markup-name markup-id))
	  (when deleted
	    (message "... deleted.")
	    (standoff-hide-markup-at-point)))))))

;;
;; HIGHLIGHTNING
;;

;; We use overlays for highlightning markup elements

(defcustom standoff-markup-overlays nil
  "This should be a alist defined by the user."
  :group 'standoff
  :type 'alist)

(defcustom standoff-markup-overlays-front nil
  "This should be a alist defined by the user."
  :group 'standoff
  :type 'alist)

(defcustom standoff-markup-overlays-after nil
  "This should be a alist defined by the user."
  :group 'standoff
  :type 'alist)

(defcustom standoff-markup-overlays-default 
  '(('face (:background "light grey")))
  "Overlay properties for markup elements not defined in
`standoff-markup-overlays'."
  :group 'standoff
  :type 'alist)

(defcustom standoff-markup-overlays-front-default 
  '(('face (:background "light grey" :foreground "dark grey")))
  "Text properties of the front string of markup
oferlays. This is used for markup elements not defined in
`standoff-markup-overlays-after'."
  :group 'standoff
  :type 'alist)

(defcustom standoff-markup-overlays-after-default 
  '(('face (:background "light grey" :foreground "dark grey")))
  "Text properties of the after string which trails markup
overlays. This is used for markup elements not defined in
`standoff-markup-overlays-after'."
  :group 'standoff
  :type 'alist)

(defun standoff--overlay-property-obarray-init ()
  "When we store the parameters of markup elements as key value
pairs of overlay properties, they are interned to a special
obarray in order to avoid namespace collisions. We also make this
special obarray buffer local."
;;TODO
  (setq standoff--overlay-property-obarray nil))
(add-hook 'standoff-mode-hook 'standoff--overlay-property-obarray-init)

(defvar standoff--overlay-property-value-format
  "standoff-markup-element-property-symbol-%s-%s")

(defun standoff--overlay-property-format-key (key)
  "Overlay properties are key value pairs where key and value are
symbols. This function returns the key as an interned
symbol. Interference with symbol names of other emacs packages
prevented if you use this function."
  (intern (format "%s" key)))

(defun standoff--overlay-property-format-value (key value &optional setting)
  "Overlay properties are key value pairs where key and value are
symbols. This function returns the value as an interned symbol
whichs name is made from the key and the value. Interference with
symbol names of other emacs packages prevented if you use this
function."
  (let ((value-formatted (format standoff--overlay-property-value-format key value)))
    (if setting
	(intern value-formatted)
      (intern-soft value-formatted))))

(defun standoff--overlay-property-set (ovly key value)
  "A convience function to set the property of the overlay OVLY
given as KEY and VALUE."
  (overlay-put ovly 
	       (standoff--overlay-property-format-key key)
	       (standoff--overlay-property-format-value key value t)))

(defun standoff--overlay-property-get (ovly key)
  "A convience function to get the property of an overlay.
The value of property KEY of the overlay OVLY is returned as a
string."
  (let ((value-front-length (length (format standoff--overlay-property-value-format key "")))
	(value-symbol (overlay-get ovly (standoff--overlay-property-format-key key))))
    (if (not (and value-symbol (intern-soft value-symbol)))
	nil
      ;; we use (format "%s" ...) to make a string from the symbol
      (substring (format "%s" value-symbol) value-front-length))))

(defun standoff--overlay-get-markup-id-at-point (error-message)
  (let ((overlays (overlays-at (point))))
    (if (> (length overlays) 1)
	(error error-message)
      (string-to-number (standoff--overlay-property-get (car overlays) "id")))))

(defun standoff-markup-number-mapping-setup ()
  "Make a new hashtable for mapping markup instance ids to numbers."
  (setq-local standoff-markup-number-mapping (make-hash-table :test 'equal)))

(add-hook 'standoff-mode-hook 'standoff-markup-number-mapping-setup)
  
(defun standoff-markup-get-number (buf markup-inst-id)
  "Return the number associated to a markup instance.
This returns an integer for the markup instance given by
MARKUP-INST-ID in the buffer BUF. If there is not yet a number
assiciated with this instance, a new unique number is created."
  (let ((number (gethash markup-inst-id standoff-markup-number-mapping nil))
	(numbers '()))
    (if number
	number
      (maphash (lambda (k _v) (push _v numbers)) standoff-markup-number-mapping)
      (puthash markup-inst-id
	       ;; max fails for an empty list, so we cons 0
	       (setq number (+ (apply 'max (cons 0 numbers)) 1))
	       standoff-markup-number-mapping)
      number)))

(defun standoff-markup-get-by-number (buf number)
  "Return the markup instance ID for a number.
If no ID maps to number, nil is returned."
  (let ((markup-inst-id nil))
    (maphash (lambda (k _v) (when (equal _v number) (setq markup-inst-id k)))
	     standoff-markup-number-mapping)
    markup-inst-id))

(defun standoff-highlight-markup-range (buf startchar endchar markup-type markup-inst-id)
"Highlight a markup range.
This is the workhorse for highlightning markup in standoff
mode. It highlights a range of text given by STARTCHAR and
ENDCHAR in the context of buffer BUF. The range is highlightened
as MARKUP-TYPE and is assigned MARKUP-INST-ID. A number is also
assigned to it for easy access by the user. This number is not
stable over working sessions, but assigned on a per session
basis. The highlightning is done by creating overlays. This
overlay is assigned the key value `\"standoff\" t'."
(save-restriction
 (widen)
 (let ((ovlys (overlays-at startchar))
       (ovly)
       (ovly-present nil))
   ;; don't create the overlay if there is already a similar one
   (while ovlys
     (setq ovly (pop ovlys))
     ;; when COND
     (and (equal (standoff--overlay-property-get ovly "id") markup-inst-id)
	  (equal (overlay-start ovly) startchar)
	  (equal (overlay-end ovly) endchar)
	  (equal (standoff--overlay-property-get ovly "type") markup-type)
	  (equal (standoff--overlay-property-get ovly "standoff") (symbol-name t))
	  ;; BODY (of when)
	  (setq ovly-present t
		ovlys nil)))
   (unless ovly-present
     ;; create the overlay
     (setq ovly (make-overlay startchar endchar buf))
     (let* ((ovly-props (or (cdr (assoc markup-type standoff-markup-overlays))
			    standoff-markup-overlays-default))
	    (front-props (or (cdr (assoc markup-type standoff-markup-overlays-front))
			     standoff-markup-overlays-front-default))
	    (after-props (or (cdr (assoc markup-type standoff-markup-overlays-after))
			     standoff-markup-overlays-after-default))
	    (number (standoff-markup-get-number buf markup-inst-id))
	    (front-str (format "[%i" number))
	    (after-str (format "%i]" number))
	    (hlp-echo (format "%s no=%i ID=%s" markup-type number markup-inst-id))
	    (front-string (car (mapcar #'(lambda (x) (propertize front-str (car(cdar x)) (cadr x))) front-props)))
	    (after-string (car (mapcar #'(lambda (x) (propertize after-str (car(cdar x)) (cadr x))) after-props))))
	    ;;(after (propertize after-str (quote face) (quote(:background "light grey"))))
       ;;(mapcar #'(lambda (x) (overlay-put ovly (car (cdar x)) (car (cdr x)))) ovly-props)
       ;;(message (format "%s" ovly-props))
       (mapcar #'(lambda (x) (overlay-put ovly (car (cdar x)) (cadr x))) ovly-props)
       (overlay-put ovly 'help-echo hlp-echo)
       (overlay-put ovly 'before-string front-string)
       (overlay-put ovly 'after-string after-string)
       (standoff--overlay-property-set ovly "standoff" t)
       (standoff--overlay-property-set ovly "type" markup-type)
       (standoff--overlay-property-set ovly "id" markup-inst-id)
       (standoff--overlay-property-set ovly "number" number)
       ;;(overlay-put ovly 'local-map standoff-markup-range-local-map)
       )))))

(defun standoff-hide-markup (&optional area-start area-end markup-type markup-number)
  "Hide markup. A general function with a filter.
This function is the workhorse of hiding markup and is being
reused by more specific interactive functions for hiding markup."
  (let* ((startchar (or area-start (point-min)))
	 (endchar (or area-end (point-max)))
	 (ovlys-present)
	 (ovlys-found '())
	 (ovly)
	 (markup-number-string (cond ((numberp markup-number) (number-to-string markup-number))
				     (t markup-number))))
    (overlay-recenter (/ (+ startchar endchar) 2))
    (setq ovlys-present (overlays-in startchar endchar))
    (while ovlys-present
      (setq ovly (pop ovlys-present))
      ;; when COND
      (and (equal (standoff--overlay-property-get ovly "standoff") (symbol-name t))
	   (or (not markup-type)
	       (equal (standoff--overlay-property-get ovly "type") markup-type))
	   (or (not markup-number)
	       (equal (standoff--overlay-property-get ovly "number") markup-number-string))
	   ;; BODY
	   (delete-overlay ovly)))))

(defun standoff-hide-markup-buffer (&optional markup-type)
  "Hide markup in the current buffer, i.e. remove all overlays."
  (interactive
   (list (completing-read
	  "Markup type to hide, <!> for all: "
	  (append "!" (standoff-markup-type-completion (current-buffer)))
	  nil
	  standoff-markup-require-name-require-match)))
  (save-excursion
    (if (or (not markup-type) (equal markup-type "!"))
	(standoff-markup-hide)
      (standoff-markup-hide nil nil markup-type))))

(defun standoff-hide-markup-region (area-start area-end &optional markup-type)
  "Hide markup in the region, optionally filtered by type."
  (interactive
   (list (region-beginning)
	 (region-end)
	 (completing-read
	  "Markup type to hide, <!> for all: "
	  (append "!" (standoff-markup-type-completion (current-buffer)))
	  nil
	  standoff-markup-require-name-require-match)))
  (save-excursion
    (if (or (not markup-type) (equal markup-type "!"))
	(standoff-markup-hide area-start area-end)
      (standoff-markup-hide area-start area-end markup-type))))

(defun standoff-hide-markup-at-point ()
  "Hide markup at point."
  (interactive)
  (save-excursion
    (standoff-markup-hide (point) (point))))

(defun standoff-hide-markup-by-number (markup-number)
  "Hide all markup with number MARKUP-NUMBER."
  (interactive "NNumber of markup element to hide: ")
  (save-excursion
    (standoff-markup-hide nil nil nil markup-number)))

(defun standoff--assert-integer-stringrange (range)
  (if (and (numberp (nth 0 range)) 
	   (numberp (nth 1 range)) 
	   (numberp (nth 3 range)))
      range
    ;; TODO
    (list (string-to-number (nth 0 range))
	  (string-to-number (nth 1 range))
	  (nth 2 range)
	  (string-to-number (nth 3 range)))))

(defun standoff-highlight-markup-region (beg end &optional markup-name)
  "Create overlays for all markup in the backend."
  (interactive 
   (list 
    (region-beginning)
    (region-end)
    (completing-read "Name of markup elements to show up, <!> for all: "
			  (cons "!" (funcall standoff-markup-names-function))
			  nil t)))
  (let* ((markup-name-or-nil (cond ((equal markup-name "!") nil)
				  (t markup-name)))
	(markup-elements (funcall standoff-markup-read-ranges-function (current-buffer) beg end markup-name-or-nil)))
    ;; First remove overlays because else they get doubled.
    ;; TODO: This makes it impossible to use this function several
    ;; times in order to sequetially show up parts of the markup.
    (standoff-hide-markup-region beg end markup-name-or-nil)
    ;; build a list from BUF STARTCHAR ENDCHAR MARKUP-NAME MARKUP-ID and apply it to ..
    (dolist (range markup-elements)
      (apply 'standoff-highlight-markup-range (cons (current-buffer) (standoff--assert-integer-stringrange range))))))

(defun standoff-highlight-markup-buffer (&optional markup-name)
  "Create overlays for all markup in the backend."
  (interactive 
   (list (completing-read "Name of markup elements to show up, <!> for all: "
			  (cons "!" (funcall standoff-markup-names-function))
			  nil t)))
  (standoff-highlight-markup-region (point-min) (point-max) markup-name))

(defun standoff-highlight-markup-by-id (markup-id)
  "Create overlays for all ranges of the markup element
identified by MARKUP-ID."
  (interactive "NId (number) of markup element to highlight: ")
  (let ((ranges (funcall standoff-markup-read-ranges-function (current-buffer) nil nil nil markup-id)))
    ;; First remove overlays because else they get doubled.
    ;; TODO: This makes it impossible to use this function several
    ;; times in order to sequetially show up parts of the markup.
    (standoff-hide-markup-buffer)
    ;; build a list from BUF STARTCHAR ENDCHAR MARKUP-NAME MARKUP-ID and apply it to ..
    (dolist (range ranges)
      (apply 'standoff-highlight-markup-range (cons (current-buffer) (standoff--assert-integer-stringrange range))))))


;;
;; Navigate
;;

(defun standoff-navigate-next ()
  (interactive)
  (overlay-recenter (point))
  (let ((pos (next-overlay-change (point))))
    (if (equal pos (point-max))
	(error "Last highlightened markup element in buffer")
      (goto-char pos))))

(defun standoff-navigate-previous ()
  (interactive)
  (overlay-recenter (point))
  (let ((pos (previous-overlay-change (point))))
    (if (equal pos (point-min))
	(error "First highlightened markup element in buffer")
      (goto-char pos))))

;;
;; Relations
;;

(defcustom standoff-markup-relation-name-require-match 'confirm
  "How restrictive has input of relation names to be? `t' for no
other names than already know names, `'confirm' to allow other
than already known names, but ask for confirmation."
  :group 'standoff
  :type 'symbol)

(defun standoff-markup-relate (subject-id predicate object-id)
  "Create a relation between the markup element given by
SUBJECT-ID and the markup element given by OBJECT-ID. Relations
have the form of a predication like in RDF: subject predicate
object. The markup element at point serves as subject, the object
must be given as OBJECT-ID, the relation given by RELATION."
  (interactive
   (let* ((subj-id (standoff--overlay-get-markup-id-at-point "This needs exactly one highlightend markup element at point"))
	  (obj-id (read-number (format "A relation has the form <subject> <predicate> <object>. The subject is identified by the point (aka curser), it's ID is %i. Please enter the ID (number) of the markup element that serves as the relation's object: " subj-id)))
	  (obj-id-passed (if (equal subj-id obj-id)
			     (error "The relation's object must not be the relation's subject")
			   obj-id))
	  (predicate-name (completing-read "Predicate: "
					   (funcall standoff-predicate-names-function (current-buffer) subj-id obj-id)
					   nil
					   standoff-markup-relation-name-require-match)))
     (if (equal subj-id obj-id)
	 (error "The relation's object must not be the relation's subject")
       (list subj-id predicate-name obj-id-passed))))
  (message "Creating relation %s %s %s." subject-id predicate object-id)
  (if (funcall standoff-markup-write-relation (current-buffer) subject-id predicate object-id)
      (run-hook-with-args 'standoff-markup-changed (current-buffer))
    (error "Storing relation failed")))

;;
;; Major mode
;;

;; Keymap and Menu

(defvar standoff-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "m" 'standoff-markup-region)
    (define-key map "M" 'standoff-markup-region-continue)
    (define-key map "d" 'standoff-markup-delete-range-at-point)
    ;;(define-key map "D" 'standoff-markup-delete-element-at-point)
    (define-key map "h" 'standoff-hide-markup-at-point)
    (define-key map "ħ" 'standoff-hide-markup-by-number)
    (define-key map "H" 'standoff-hide-markup-buffer)
    (define-key map "l" 'standoff-highlight-markup-by-id)
    (define-key map "L" 'standoff-highlight-markup-buffer)
    (define-key map "r" 'standoff-relate-markup-element-at-point)
    (define-key map "s" 'standoff-store-markup-element-at-point)
    (define-key map "a" 'standoff-annotate-markup-element-at-point)
    (define-key map "n" 'standoff-navigate-next)
    (define-key map "p" 'standoff-navigate-previous)
    map))

(easy-menu-define standoff-menu standoff-mode-map
  "Menu for standoff mode"
  '("Standoff"
    ["Create new markup element" standoff-markup-region]
    ["Continue markup element" standoff-markup-region-continue]
    ["--" nil]
    ["Delete markup at point" standoff-markup-delete-range-at-point]
    ;;["Delete markup element at point" standoff-markup-delete-element-at-point]
    ["--" nil]
    ["Store markup element as relation object" standoff-store-markup-element-at-point]
    ["Relate markup element to stored object" standoff-relate-markup-at-point]
    ["--" nil]
    ["Annotate markup element" standoff-annotate-markup-element-at-point]
    ["--" nil]
    ["Highlight markup in buffer" standoff-highlight-markup-buffer]
    ["Highlight markup in region" standoff-highlight-markup-region]
    ["Highlight markup with id" standoff-highlight-markup-by-id]
    ["Hide markup in buffer" standoff-hide-markup-buffer]
    ["Hide markup in region" standoff-hide-markup-region]
    ["Hide markup at point" standoff-hide-markup-at-point]
    ["Hide markup with number" standoff-hide-markup-by-number]
    ["--" nil]
    ["Navigate to next highlightened element" standoff-navigate-next]
    ["Navigate to previous highlightened element" standoff-navigate-previous]
    ))

(defvar standoff-markup-range-local-map
  (let ((map (make-sparse-keymap)))
    (define-key map "d" 'standoff-markup-delete-range-at-point)
    ;; (define-key map "D" 'standoff-markup-delete-element-at-point)
    (define-key map "h" 'standoff-hide-markup-at-point)
    (define-key map "H" 'standoff-hide-markup-by-number)
    map))


(define-derived-mode standoff-mode special-mode "Standoff"
  "Standoff mode is an Emacs major mode for creating standoff
markup. It makes the file (the buffer) which is linked by the
markup read only. 

Since text insertion to a file linked by standoff markup is not
sensible at all, keyboard letters don't allow inserting text but
are bound to commands instead.

\\{standoff-mode-map}
"
  )



(provide 'standoff-mode)
