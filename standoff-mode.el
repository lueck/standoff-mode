;;; standoff-mode.el --- Create stand-off annotations and markup

;; Copyright (C) 2015 Christian Lück

;; Author: Christian Lück <christian.lueck@ruhr-uni-bochum.de>
;; URL: https://github.com/lueck/standoff-mode

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with standoff-mode. If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Mode for creating and editing stand-off markup, aka external markup

;; Usage:

;; Add to your Emacs config:

;;  (add-to-list 'load-path "/path/to/standoff-mode-dir")
;;  (autoload 'standoff-mode "standoff-mode.el"
;;    "Mode for creating and editing stand-off markup, aka external markup" t)
;;  (add-to-list 'auto-mode-alist '("\\.TEI-P5.xml$" . standoff-mode))

;;; Code:
(require 'standoff-api)
(require 'standoff-dummy)
(require 'standoff-xml)

;;
;; Checksum of source document
;;

(make-variable-buffer-local 'standoff-source-md5)

(defun standoff-source-checksum ()
  (interactive)
  "Set the checksum of the source document if and only if not yet set.
Stand-off markup only makes sense, if the source document is
stable. Otherwise the references to it via character offsets get
broken. This function makes an md5 hash and stores it to the
buffer-local variable `standoff-source-md5'.  This function will
be called via a mode hook, so that the checksum is there right
away for running checks against it. It can be called
interactively, but will have no effect, if the hash was already
calculated. The hash will show up in the minibuffer."
  (unless standoff-source-md5
    (setq-local standoff-source-md5 (md5 (current-buffer))))
  (message "The document's md5 checksum is: %s" standoff-source-md5))

(add-hook 'standoff-mode-hook 'standoff-source-checksum)

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
	  standoff-markup-type-require-match)))
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

(defun standoff-markup-region-continue (beg end markup-number)
  "Add selected region as a new range continueing an existing markup element.
The markup element is identified by MARKUP-ID. The range is given
by BEG and END or point and mark, aka the region. This function
enables the user to create discontinues markup."
  (interactive "r\nNIdentifying number of markup element to be continued: ")
  (let* ((markup-id (or (standoff-markup-get-by-number (current-buffer) markup-number)
			(error "No markup element with number %i found" markup-number)))
	 (markup-type (nth standoff-pos-markup-type (car (funcall standoff-markup-read-function (current-buffer) nil nil nil markup-id))))
	 (duplicate (funcall standoff-markup-read-function (current-buffer) beg end markup-type markup-id))
	 (markup-id-from-backend nil))
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
      (deactivate-mark))))

(defun standoff-markup-notify (buf startchar endchar markup-type markup-id)
  "Notify the creation of a markup element or range.
This is a handler function that can be hooked to the
`standoff-markup-post-functions' hook, which is called when ever
an markup is being done. This function does nothing but notify
the user with a message in the minibuffer (and in the *Messages*
buffer)."
  (message "Annotating from %i to %i as %s with id %s." 
	   startchar endchar markup-type markup-id))

(add-hook 'standoff-markup-post-functions 'standoff-markup-notify)

(defun standoff-markup-delete-range-at-point (point)
  "Delete the range of a markup element at point.
The range is identified by the overlay's properties. So this
works only if there is one and exactly one overlay."
;; TODO: Collect information about related items when
;; asking "Do you really ...? (yes or no)"
  (interactive "d")
  (save-restriction
    (widen)
    (overlay-recenter (point))
    (let* ((ovly (standoff-highlight-markup--select point))
	   (startchar (overlay-start ovly))
	   (endchar (overlay-end ovly))
	   (markup-type (standoff--overlay-property-get ovly "type"))
	   (markup-number (string-to-number (standoff--overlay-property-get ovly "number")))
	   (markup-inst-id (standoff-markup-get-by-number (current-buffer) markup-number))
	   (markup-ranges (funcall standoff-markup-read-function (current-buffer) nil nil nil markup-inst-id))
	   (precondition)
	   (deleted nil)
	   (last-range nil))
      ;; (message "%s" (length markup-ranges))
      (if (> (length markup-ranges) 1)
	  (setq precondition (y-or-n-p (format "Do you really want to delete this range of '%s' %s? " markup-type markup-inst-id)))
	(setq precondition (yes-or-no-p (format "Do you really want to delete markup element %s, which is a '%s', and all it's related items? " markup-inst-id markup-type))
	      last-range t))
      (when precondition
	(setq deleted (funcall standoff-markup-delete-range-function (current-buffer) startchar endchar markup-type markup-inst-id))
	(when deleted
	  (when last-range
	    (standoff-markup-remove-number-mapping (current-buffer) markup-inst-id))
	  (delete-overlay ovly)
	  (message "... deleted."))))))

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

(defun standoff-markup-remove-number-mapping (buf markup-inst-id)
  "Remove a markup-inst-id to number mapping from the hashtable.
This should be called when all ranges of a markup instance have
been deleted."
  (remhash markup-inst-id standoff-markup-number-mapping))

(defun standoff-highlight-markup-range (buf startchar endchar markup-type markup-inst-id)
"Highlight a markup range.
This is the workhorse for highlightning markup in standoff
mode. It highlights a range of text given by STARTCHAR and
ENDCHAR in the context of buffer BUF. The range is highlighted
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
	  nil)))
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
	  nil)))
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
    ;; (message "from backend: %s" markup-elements)
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
		     (cons "!" (standoff-markup-type-completion (current-buffer)))
		     nil nil)))
  (let ((markup-type-or-nil (if (equal markup-type "!") nil markup-type)))
    (standoff-highlight-markup beg end markup-type-or-nil)))

(defun standoff-highlight-markup-buffer (&optional markup-type)
  "Highlight markup in the backend optionally filtered by markup type."
  (interactive
   (list (completing-read "Type of markup to show up, <!> for all: "
			  (cons "!" (standoff-markup-type-completion (current-buffer)))
			  nil nil)))
  (let ((markup-type-or-nil (if (equal markup-type "!") nil markup-type)))
    (standoff-highlight-markup nil nil markup-type-or-nil)))

(defun standoff-highlight-markup-by-number (number)
  "Highlight the markup element which's id is mapping to NUMBER."
  (interactive "NNumber of markup element to highlight: ")
  (let ((markup-inst-id (standoff-markup-get-by-number (current-buffer) number)))
    (unless markup-inst-id
      (error "No markup element mapping to number %s" number))
    (standoff-highlight-markup nil nil nil markup-inst-id)))

;; Selection of highlighted markup

(defun standoff-highlight-markup--select (point)
  "Returns the *overlay* for the markup range at point.
This will throw an error if there's more than one highlighted
standoff markup range at point, because the selection is
ambiguous then."
  (let ((ovlys '()))
    (mapcar #'(lambda (x) (when (equal (standoff--overlay-property-get x "standoff")
				       (symbol-name t))
			    (push x ovlys)))
	    (overlays-at point))
    (unless (car ovlys)
      (error "No highlighted markup element found"))
    (when (cdr ovlys)
      (error "More than one highlighted markup element found. Please use the functions for hiding markup to make your selection unambiguous"))
    (car ovlys)))

(defun standoff-highlight-markup--get-id (point error-message)
  "Returns the id of the highlighted markup element at POINT.
If there is not exactly one standoff highlightning at point, the
show ERROR-MESSAGE."
  (let ((ovlys '()))
    (mapcar #'(lambda (x) (when (equal (standoff--overlay-property-get x "standoff")
				       (symbol-name t))
			    (push x ovlys)))
	    (overlays-at point))
    (if (= (length ovlys) 1)
	(standoff-markup-get-by-number
	 (current-buffer)
	 (string-to-number (standoff--overlay-property-get (car ovlys) "number")))
      (error error-message))))

;;
;; Navigate
;;

(defun standoff-navigate-next ()
  (interactive)
  (overlay-recenter (point))
  (let ((pos (next-overlay-change (point))))
    (if (equal pos (point-max))
	(error "Last highlighted markup element in buffer")
      (goto-char pos))))

(defun standoff-navigate-previous ()
  (interactive)
  (overlay-recenter (point))
  (let ((pos (previous-overlay-change (point))))
    (if (equal pos (point-min))
	(error "First highlighted markup element in buffer")
      (goto-char pos))))

;;
;; Relations
;;

(defcustom standoff-predicate-require-match 'confirm
  "Defines how restrictive relation types are handled.
`t' for no other names than already know names, `confirm' to
allow other than already known names, but ask for confirmation."
  :group 'standoff
  :type 'symbol)

(defcustom standoff-predicates-allowed-function 'standoff-predicates-allowed-from-elisp
  "")

(defun standoff-predicates-allowed-from-elisp (buf subj-id obj-id)
  ""
  ;; TODO
  '())

(defun standoff-predicate-completion (buf subj-id obj-id)
  "Return a list of valid predicates for a combination of subject and object.
This may add labels to improve usability."
  ;; TODO: add labels
  (cond ((equal standoff-predicate-require-match t)
	 (funcall standoff-predicates-allowed-function buf subj-id obj-id))
	(t ;; 'confirm OR nil
	 (append (funcall standoff-predicates-used-function buf subj-id obj-id)
		 (funcall standoff-predicates-allowed-function buf subj-id obj-id)))))

(defun standoff-predicate-from-user-input (buf predicate)
  "Make a real predicate from user input.
User input may still contain the predicates label."
  ;; TODO: remove labels
  predicate)

(defun standoff-markup-relate (subject-id predicate object-id)
  "Create a directed graph modelling a relation between two markup elements.
This establishes a rdf-like relation between markup element as
subject given by SUBJECT-ID and a markup element as object given
by OBJECT-ID. The relation is of type PREDICATE, so the graph has
the form \"subject predicate object\". When called interactively,
the markup element at point serves as subject, the object must be
given by the number mapping to its id."
  (interactive
   (let* ((subj-ovly (standoff-highlight-markup--select (point)))
	  (subj-number (string-to-number (standoff--overlay-property-get subj-ovly "number")))
	  (subj-id (standoff-markup-get-by-number (current-buffer) subj-number))
	  ;;(subj-id (standoff-highlight-markup--get-id (point) "This needs exactly one highlighted markup element at point"))
	  (obj-number (read-number (format "A relation has the form <subject> <predicate> <object>. The subject is identified by the point (aka curser), it's number is %i. Please enter the number of the markup element that serves as the relation's object: " subj-number)))
	  (obj-is-not-subj (or (not (= subj-number obj-number))
			       (error "The relation's object must not be the relation's subject")))
	  (obj-id (or (standoff-markup-get-by-number (current-buffer) obj-number)
		      (error "Invalid markup number")))
	  (predicate-type
	   (completing-read "Predicate: "
			    (standoff-predicate-completion (current-buffer) subj-id obj-id)
			    nil
			    standoff-predicate-require-match))
	  (predicate (standoff-predicate-from-user-input (current-buffer) predicate-type)))
     (list subj-id predicate obj-id)))
  (message "Creating relation %s %s %s." subject-id predicate object-id)
  (if (funcall standoff-relation-create-function (current-buffer) subject-id predicate object-id)
      (run-hook-with-args 'standoff-markup-changed (current-buffer))
    (error "Creation of relation failed")))

;;
;; Dumping
;;

(defcustom standoff-dump-vars '(standoff-markup-read-function standoff-relations-read-function standoff-source-md5)
  "A list of variables and function pointers to be dumped to elisp expressions.
The dumper function `standoff-dump-elisp' will variables and even
try to call the function given in a function pointer. Such
functions should take a buffer as argument and should not require
further arguments."
  :group 'standoff
  :type 'list
  )

(defun standoff-dump-filename-default ()
  "Returns a default dump file name."
  (concat (buffer-file-name) ".dump.el" ))

(defun standoff-dump--print-quoted (to-buf var-name var-value)
  "Dump variable VAR-NAME with value VAR-VALUE that is a list to buffer TO-BUF."
  (print (list 'setq var-name (list 'quote var-value)) to-buf))

(defun standoff-dump--print (to-buf var-name var-value)
  "Dump variable VAR-NAME with value VAR-VALUE to buffer TO-BUF."
  (print (list 'setq var-name var-value) to-buf))

(defun standoff-dump-elisp (dump-file)
  "Dump the stand-off markup in the current buffer to file DUMP-FILE."
  (interactive
   (list (read-file-name "File to be dumped to: "
			 nil
			 nil
			 'confirm
			 (file-relative-name (standoff-dump-filename-default)))))
  (let ((source-buf (current-buffer))
	(dump-buf (find-file-noselect dump-file)))
    (save-excursion
      (set-buffer dump-buf)
      (erase-buffer)
      ;; make source buffer the current buffer, because the back-end
      ;; may be buffer-local like the dummy back-end
      (set-buffer source-buf)
      (dolist (var standoff-dump-vars)
	(let ((dump-var-name (intern (format "%s-dumped" var))))
	  (if (symbolp (symbol-value var))
	      (cond ((functionp (symbol-value var))
		     (standoff-dump--print-quoted dump-buf dump-var-name (funcall (symbol-value var) source-buf)))
		    (t (message "Left type %s: %s" var (type-of (symbol-value var)))))
	    (cond
	     ((stringp (symbol-value var))
	      (standoff-dump--print dump-buf dump-var-name (symbol-value var)))
	     (t (message "Left type %s: %s" var (type-of (symbol-value var))))))))
      (set-buffer dump-buf)
      (save-buffer)
      (kill-buffer))))

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
    (define-key map "r" 'standoff-markup-relate)
    ;;(define-key map "s" 'standoff-store-markup-element-at-point)
    (define-key map "a" 'standoff-annotate-markup-element-at-point)
    (define-key map "n" 'standoff-navigate-next)
    (define-key map "p" 'standoff-navigate-previous)
    (define-key map "u" 'standoff-dump-elisp)
    map))

(easy-menu-define standoff-menu standoff-mode-map
  "Menu for standoff-mode"
  '("Stand-Off"
    ["Create new markup element" standoff-markup-region]
    ["Continue markup element" standoff-markup-region-continue]
    ["--" nil]
    ["Delete markup at point" standoff-markup-delete-range-at-point]
    ;;["Delete markup element at point" standoff-markup-delete-element-at-point]
    ["--" nil]
    ;;["Store markup element as relation object" standoff-store-markup-element-at-point]
    ["Relate markup element at point to some other" standoff-markup-relate]
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
    ["Navigate to next highlighted element" standoff-navigate-next]
    ["Navigate to previous highlighted element" standoff-navigate-previous]
    ["--" nil]
    ["Hide/Show XML-tags" standoff-xml-tags-invisible]
    ["--" nil]
    ["Dump to file (SAVE)" standoff-dump-elisp]
    ))

(defvar standoff-markup-range-local-map
  (let ((map (make-sparse-keymap)))
    (define-key map "d" 'standoff-markup-delete-range-at-point)
    ;; (define-key map "D" 'standoff-markup-delete-element-at-point)
    (define-key map "h" 'standoff-hide-markup-at-point)
    (define-key map "H" 'standoff-hide-markup-by-number)
    map))

;;;###autoload
(define-derived-mode standoff-mode special-mode "Stand-Off"
  "Stand-Off mode is an Emacs major mode for creating stand-off
markup and annotations. It makes the file (the buffer) which the
the markup refers to read only, and the markup is stored
externally (stand-off).

Since text insertion to a file linked by standoff markup is not
sensible at all, keyboard letters don't allow inserting text but
are bound to commands instead.

\\{standoff-mode-map}
"
  )



(provide 'standoff-mode)

;;; standoff-mode.el ends here
