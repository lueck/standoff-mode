;; standoff.el -- An Emacs mode for stand-off markup

(defvar standoff-markup-write-range-function 'standoff-dummy-write-range
  "The function that writes a range of a markup element to some
backend (which should be persistent).

The function must take the following arguments:

BUFFER STARTCHAR ENDCHAR MARKUP-NAME MARKUP-ID

where MARKUP-ID can ether be an integer value or a single
character key signalling how to handle the ID:

`n': new, which requires automatic handling of the id,
     e.g. auto-incrementing. The backend has to take control over
     the ID.

The function is expected to return an integer ID of the markup
element (not the range). In case storing to the backend was not
successfull it is expected to return `nil'.
")

(defvar standoff-markup-read-ranges-function 'standoff-dummy-read-ranges
  "The function that gets markup ranges from some backend.

The function must take the following arguments:

BUFFER &optional STARTCHAR ENDCHAR MARKUP-NAME MARKUP-ID

If STARTCHAR *and* ENDCHAR are numerical values, the function
should return only markup elements that overlap this portion of
BUFFER. If MARKUP-NAME is given, only markup elements of this
type should be returned. If MARKUP-ID is given, only the ranges
of the markup-element with this id should be returned. So, if
none of STARTCHAR, ENDCHAR, MARKUP-NAME and MARKUP-ID is given,
the function should return all markup element ranges for the
buffer.

The function is expected to return a list of markup ranges which are
again represented as lists, as follows:

'('(STARTCHAR ENDCHAR MARKUP-NAME MARKUP-ID) (...))

So, if a markup element consists of more than one
range (discontinous markup), the same values for MARKUP-NAME and
MARKUP-ID must occur in more than one of those lists.

")

(defvar standoff-markup-delete-range-function 'standoff-dummy-delete-range
  "A pointer to the function that deletes a range of a markup
  element form the backend. The range is given by the following
  parameters which the function must take (all of them):

BUFFER STARTCHAR ENDCHAR MARKUP-NAME MARKUP-ID

The function should return nil or throw an error if the range
could not be deleted and t on successfull deletion. It's up to
the backend to control deletion preconditions which might be:

- any relation of the markup element, which the range belongs to,
  to other markup elements if there the markup element would have
  zero ranges after the deletion. The backend may interact with
  the user in this case.

")

(defvar standoff-markup-names-function 'standoff-dummy-markup-element-names
  "A function that returns names of defined (or already used)
markup elements. The function ist expected to return a list of
strings.")

(defcustom standoff-markup-changed-functions nil
  "A hook for handlers that need to be called if the markup on a
buffer was changed. This can be used for evalution, updating the
highlightning etc. etc.

In terms of elisp this is a so called abnormal hook, i.e. the
hooked functions must take arguments. Arguments are:

BUFFER

BUFFER: the buffer the markup relates to, aka the source document

The return value of the hooked functions is not evaluated at all. 
")

(defcustom standoff-markup-post-functions nil
  "A hook for handlers called when a region was marked up und the
markup was successfully stored to the backend. This hook should
be used for highlightning and notifications.

This is a so called `abnormal' hook, i.e. the hooked
functions (aka handlers) must take the following arguments:

BUFFER STARTCHAR ENDCHAR MARKUP-NAME MARKUP-ID

"
  :group 'standoff
  :type 'hook
  :options '(standoff-notify-markup))

(defvar standoff-predicate-names-function 'standoff-dummy-predicate-names
  "The function which returns a list of names of relation
predicates from the backend. It must take two arguments

SUBJECT-ID OBJECT-ID 

The returned list of predicate names must be valid predicate
names for the combination of subject and object. Should return
nil if there are no valid predicates for this combination.")

(defvar standoff-markup-write-relation 'standoff-dummy-write-relation
  "The function which writes a new relation to the backend. It
  must take the following arguments:

BUFFER SUBJECT-ID PREDICATE-NAME OBJECT-ID

The function is expected to return a non-nil value, if writing
the relation to the backend was successful, nil in case of
failure.")

;;
;; creating and deleting stand-off markup
;;

(defun standoff-markup-region (beg end markup-name)
  "Create stand-off markup for the selected region given by BEG
and END, the name of the markup element is given by MARKUP-NAME.
The id is automatically assigned by the backend, e.g. by
automatic incrementation. This function will create a new markup
element."
  (interactive
   (list (region-beginning)
	 (region-end)
	 (completing-read "Name of markup element: "
			  (funcall standoff-markup-names-function)
			  nil
			  standoff-markup-require-name-require-match)))
  (let ((markup-id nil))
    (save-restriction
      (widen)
      (save-excursion
	(setq markup-id (funcall standoff-markup-write-range-function (current-buffer) beg end markup-name "n"))
	(when markup-id
	  ;; run hook to notify success and highlight the new markup
	  (run-hook-with-args 'standoff-markup-post-functions (current-buffer) beg end markup-name markup-id))))
    (deactivate-mark)))

(defun standoff-markup-region-continue (beg end markup-id)
  "Add a range for the markup element identified by
MARKUP-ID. The range is given by BEG and END or point and mark,
aka the region (which may be inactive). This function enables the
user to create continues markup (which should better be called
discontinues markup, because there will be kind of gaps in the
markup produced)."
  (interactive "r\nNId (number) of markup element to be continued: ")
  (let* ((markup-name (nth 2 (car (funcall standoff-markup-read-ranges-function (current-buffer) nil nil nil markup-id))))
	 (duplicate (funcall standoff-markup-read-ranges-function (current-buffer) beg end markup-name markup-id))
	 (markup-id-from-backend nil))
    (if (not markup-name)
	(error "No markup element with ID %i found" markup-id)
      (if duplicate
	  (error "Overlapping markup with the same id and element name! Not creating a duplicate")
	(save-restriction
	  (widen)
	  (save-excursion
	    (setq markup-id-from-backend (funcall standoff-markup-write-range-function (current-buffer) beg end markup-name markup-id))
	    (message "Hi: %s" markup-id-from-backend)
	    (when markup-id-from-backend
	      ;; run hook to notify success and highlight the new markup
	      (run-hook-with-args 'standoff-markup-post-functions (current-buffer) beg end markup-name markup-id-from-backend))))
	(deactivate-mark)))))

(defun standoff-markup-region-deprecated (markup-name markup-id)
  "Markup the selected region, i.e. mark the region as a range
of `markup-name' with the id given in `markup-id'."
  ;; Deprecated! Ether a name or an id should be given! This makes
  ;; things consistent.
  (interactive
   (list (standoff-minibuffer-require-markup-name)
	 (standoff-minibuffer-require-id-or-key)))
  (message (format "Name was %s, ID was %s." markup-name markup-id))
  (let* ((buf (current-buffer)))
    (save-restriction
      (widen)
      (save-excursion
	(let* ((id-from-backend (funcall standoff-markup-write-range-function buf (region-beginning) (region-end) markup-name markup-id)))
	  (when id-from-backend
	    ;; run hook to notify success and highlight the new markup
	    (run-hook-with-args 'standoff-markup-post-functions buf (region-beginning) (region-end) markup-name id-from-backend)))))
    (deactivate-mark)))

(defun standoff-minibuffer-require-id-or-key ()
  (let ((enable-recursive-minibuffers t)
	 (mini-input (read-from-minibuffer "Id of markup element or <n> for a new one (automatic handling): ")))
    (if (string-match "^\\([0-9]+\\|n\\)$" mini-input)
	(match-string 0 mini-input)
      (standoff-minibuffer-require-id-or-key))))

(defcustom standoff-markup-require-name-require-match 'confirm
  "Defines how restrictive the markup name is handled. If set to
`t' then it must match a list element returned by
standoff-markup-names-function. If set to `nil', the user may
exit his input with any name. If set to `confirm' (symbol), the
user may exit with any name, but is asked to confirm his input."
  :group 'standoff)

(defun standoff-markup-names-from-overlays ()
  "Return the list of user defined markup elements. This would be
a good alternative for standoff-dummy-markup-element-names as the
standoff-markup-names-function. "
  (mapcar 'car standoff-markup-overlays))

(defun standoff-minibuffer-require-markup-name ()
  "Ask the user for the name of the  "
  (completing-read "Name of markup element: " 
		   (funcall standoff-markup-names-function) 
		   nil
		   standoff-markup-require-name-require-match))

(defun standoff-markup-notify (buf startchar endchar markup-name markup-id)
  "A handler function that can be hooked to the
standoff-markup-post-functions hook, which is called when ever an
markup is being done. This function does nothing but notify
the user with a message in the minibuffer (and in the *Messages*
buffer)."
  (message "Annotating from %i to %i as %s with id %i." 
	   startchar endchar markup-name markup-id))

(add-hook 'standoff-markup-post-functions 'standoff-markup-notify)

(defun standoff-markup-delete-range-at-point ()
  "Delete a string range of a markup element at point. The range
is identified by the overlays properties. So this works only if
there is an overlay."
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
  "A convience function to get the property of an overlay. The
value of property KEY of the overlay OVLY is returned as a
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

(defun standoff-highlight-markup-range (buf startchar endchar markup-name markup-id)
"Highlight a markup range. This is the workhorse of highlighning in standoff mode."
(save-restriction
 (widen)
 (let* ((ovly-props (or (cdr (assoc markup-name standoff-markup-overlays))
			standoff-markup-overlays-default))
	(front-props (or (cdr (assoc markup-name standoff-markup-overlays-front))
			 standoff-markup-overlays-front-default))
	(after-props (or (cdr (assoc markup-name standoff-markup-overlays-after))
			 standoff-markup-overlays-after-default))
	(front-str (format "[%i" markup-id))
	(after-str (format "%i]" markup-id))
	(hlp-echo (format "%s ID=%i" markup-name markup-id))
	(front-string (car (mapcar '(lambda (x) (propertize front-str (car(cdar x)) (cadr x))) front-props)))
	(after-string (car (mapcar '(lambda (x) (propertize after-str (car(cdar x)) (cadr x))) after-props)))
	;;(after (propertize after-str (quote face) (quote(:background "light grey"))))
	;; create the overlay
	(ovly (make-overlay startchar endchar buf)))
   ;;(mapcar '(lambda (x) (overlay-put ovly (car (cdar x)) (car (cdr x)))) ovly-props)
   ;;(message (format "%s" ovly-props))
   (mapcar '(lambda (x) (overlay-put ovly (car (cdar x)) (cadr x))) ovly-props)
   (overlay-put ovly 'help-echo hlp-echo)
   (overlay-put ovly 'before-string front-string)
   (overlay-put ovly 'after-string after-string)
   (standoff--overlay-property-set ovly "name" markup-name)
   (standoff--overlay-property-set ovly "id" (number-to-string markup-id))
   ;;(overlay-put ovly 'local-map standoff-markup-range-local-map)
   )))

(add-hook 'standoff-markup-post-functions 'standoff-highlight-markup-range)

(defun standoff-hide-markup-buffer (&optional markup-name)
  "Hide markup in the current buffer, i.e. remove all overlays."
  (interactive
   (list (completing-read "Name of markup element, <!> for all: "
			  (funcall standoff-markup-names-function)
			  nil
			  nil)))
  (save-excursion
    (cond
     ((equal markup-name "!") (remove-overlays))
     ((not markup-name) (remove-overlays))
     (t (remove-overlays (point-min) (point-max) 
			 (standoff--overlay-property-format-key "name")
			 (standoff--overlay-property-format-value "name" markup-name))))))

(defun standoff-hide-markup-region (area-start area-end &optional markup-name)
  "Hide markup in the region, i.e. remove overlays."
  (interactive "r")
  (save-excursion
    (overlay-recenter area-end)
    (mapc 'delete-overlay (overlays-in area-start area-end))))

(defun standoff-hide-markup-at-point ()
  "Hide markup at point, i.e. remove overlay(s)."
  (interactive)
  (save-excursion
    (overlay-recenter (point))
    (mapc 'delete-overlay (overlays-at (point)))))

(defun standoff-hide-markup-at-point-by-id (id)
  (interactive "NId of markup element to hide: ")
  (save-excursion
    (overlay-recenter (point))
    (let ((overlays (overlays-at (point))))
      (while overlays
	(let* ((overlay (car overlays))
	       (overlay-id (standoff--overlay-property-get overlay "id")))
	  (when (equal overlay-id (number-to-string id))
	    (delete-overlay overlay)))
	(setq overlays (cdr overlays))))))

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
    (define-key map "ħ" 'standoff-hide-markup-at-point-by-id)
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
    ["Hide markup with id at point" standoff-hide-markup-at-point-by-id]
    ["--" nil]
    ["Navigate to next highlightened element" standoff-navigate-next]
    ["Navigate to previous highlightened element" standoff-navigate-previous]
    ))

(defvar standoff-markup-range-local-map
  (let ((map (make-sparse-keymap)))
    (define-key map "d" 'standoff-markup-delete-range-at-point)
    ;; (define-key map "D" 'standoff-markup-delete-element-at-point)
    (define-key map "h" 'standoff-hide-markup-at-point)
    (define-key map "H" 'standoff-hide-markup-at-point-by-id)
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
