;; standoff-mode.el -- An Emacs mode for stand-off markup

(require 'standoff-api)
(require 'standoff-dummy)

;;
;; creating and deleting markup
;;

(defcustom standoff-markup-type-require-match 'confirm
  "Defines how restrictive the markup schema is handled.
This has effect when adding new markup and choosing its type. If
set to `t' then the entered type must be amongst the members of
the list returned by calling the handler function given by
`standoff-markup-types-allowed-function'. If set to `nil', the
user may exit his input with any type. If set to
`confirm' (symbol), the user may exit with any type, but is asked
to confirm his input. Cf. `completing-read'."
  :group 'standoff)

(defcustom standoff-markup-types-allowed-function 'standoff-markup-types-from-overlay-definition
  "Points to the function called for a list of allowed markup types.
This variable must be set to the function's symbol (name)."
  :group 'standoff
  :type 'function
  :options '('standoff-markup-types-from-overlay-definition))

(defcustom standoff-markup-overlays '()
  "The overlay definition. This should be defined by the user."
  :group 'standoff)

(defun standoff-markup-types-from-overlay-definition ()
  "Return the list of user defined markup elements.
This might serve as simple handler called using
`standoff-markup-types-allowed-function'. "
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
	;; (message "%s" (length markup-ranges))
	(if (> (length markup-ranges) 1)
	    (setq precondition (y-or-n-p (format "Do you really want to delete this range of '%s' %s? " markup-name markup-id)))
	  (setq precondition (yes-or-no-p (format "Do you really want to delete markup element %s, which is a '%s', and all it's related items? " markup-id markup-name))))
	(when precondition
	  (setq deleted (funcall standoff-markup-delete-range-function (current-buffer) startchar endchar markup-name markup-id))
	  (when deleted
	    (message "... deleted.")
	    (standoff-hide-markup-at-point)))))))

;;
;; Highlighning and hiding markup
;;

;; We use overlays to highlight markup elements

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

(defun standoff--normalize-markup-inst-id (id)
  "Cast ID of markup instance to the format returned by `standoff--overlay-property-get'.
This is used in `standoff-highlight-markup-range' to check if a
similar overlay is already
present. `standoff--overlay-property-get' returns strings, after
`standoff--overlay-property-set' used (`format' \"...%s\"
... value) to gerate the value. So using `format' here is quite
save. But depending on the the format of the IDs for markup
instances this functions might need to be rewritten."
  (format "%s" id))

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
     (and (equal (standoff--overlay-property-get ovly "id") (standoff--normalize-markup-inst-id markup-inst-id))
	  (equal (overlay-start ovly) startchar)
	  (equal (overlay-end ovly) endchar)
	  (equal (standoff--overlay-property-get ovly "type") (format "%s" markup-type))
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
  (let ((startchar (or area-start (point-min)))
	(endchar (or area-end (point-max)))
	(ovlys-present)
	(ovlys-found '())
	(ovly)
	(markup-number-string (format "%s" markup-number)))
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
	(standoff-hide-markup)
      (standoff-hide-markup nil nil markup-type))))

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
    (let ((markup-type-or-nil (if (equal markup-type "!") nil markup-type)))
      (standoff-hide-markup area-start area-end markup-type-or-nil))))

(defun standoff-hide-markup-at-point ()
  "Hide markup at point."
  (interactive)
  (save-excursion
    (standoff-hide-markup (point) (point))))

(defun standoff-hide-markup-by-number (markup-number)
  "Hide all markup with number MARKUP-NUMBER."
  (interactive "NNumber of markup element to hide: ")
  (save-excursion
    (standoff-hide-markup nil nil nil markup-number)))

(defun standoff--markup-offset-integer (offset)
  "Do a type cast for an offset value from the backend if neccessary.
Offset values have to be integer. Some backends may store them as
strings. So we use this function to assert that we have
integers. Takes OFFSET as argument."
  (cond ((numberp offset) offset)
	((stringp offset) (string-to-number offset));; TODO: better regexp?
	(t (error "Can not convert offset value to integer: %s" offset))))

(defun standoff-highlight-markup (&optional beg end markup-type markup-inst-id)
  "Apply a filter to the markup from the backend and highlight it.
This function can be reused by other more specific interactive
functions for highlightning markup in the current buffer. It
calls `standoff-highlight-markup-range' to actually highlight a
markup element or range."
  (let ((markup-elements (funcall standoff-markup-read-function (current-buffer) beg end markup-type markup-inst-id)))
    ;;(message "id: %s ; From Backend: %s" markup-inst-id markup-elements)
    ;; build a list from the markup returned by the backend and apply
    ;; it to standoff-highlight-markup-range
    (dolist (range markup-elements)
      (apply 'standoff-highlight-markup-range
	     (list (current-buffer)
		   (standoff--markup-offset-integer (nth standoff-pos-startchar range))
		   (standoff--markup-offset-integer (nth standoff-pos-endchar range))
		   (nth standoff-pos-markup-type range)
		   (nth standoff-pos-markup-inst-id range))))))

(defun standoff-highlight-markup-region (beg end &optional markup-type)
  "Create overlays for all markup in the backend."
  (interactive
   (list
    (region-beginning)
    (region-end)
    (completing-read "Type of markup to show up, <!> for all: "
		     (cons "!" (funcall standoff-markup-type-completion (current-buffer)))
		     nil t)))
  (let ((markup-type-or-nil (if (equal markup-type "!") nil markup-type)))
    (standoff-highlight-markup beg end markup-type-or-nil)))

(defun standoff-highlight-markup-buffer (&optional markup-type)
  "Highlight markup in the backend optionally filtered by markup type."
  (interactive
   (list (completing-read "Type of markup to show up, <!> for all: "
			  (cons "!" (funcall standoff-markup-type-completion (current-buffer)))
			  nil t)))
  (let ((markup-type-or-nil (if (equal markup-type "!") nil markup-type)))
    (standoff-highlight-markup nil nil markup-type-or-nil)))

(defun standoff-highlight-markup-by-number (number)
  "Highlight the markup element which's id is mapping to NUMBER."
  ;; Fixme!
  (interactive "NNumber of markup element to highlight: ")
  (let ((markup-inst-id (standoff-markup-get-by-number (current-buffer) number)))
    (unless markup-inst-id
      (error "No markup element mapping to number %s" number))
    ;;(message "n: %s, id: %s" number markup-inst-id)
    (standoff-highlight-markup (point-min) (point-max) nil markup-inst-id)))

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
    (define-key map "l" 'standoff-highlight-markup-by-number)
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
    ["Highlight markup with id" standoff-highlight-markup-by-number]
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
