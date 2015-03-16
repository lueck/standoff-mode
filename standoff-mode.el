;; standoff.el -- An Emacs mode for stand-off markup

(defcustom standoff-markup-write-range-function 'standoff-dummy-write-range
  "The function that writes a range of a markup element to some
backend (which should be persistent).  The default value is
`standoff-dummy-write-range' which does not persist
anything. Use this variable to choose an elaborate backend.

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
"
  :group 'standoff
  :type 'function)

(defcustom standoff-markup-read-ranges-function 'standoff-dummy-read-ranges
  "The function that gets markup ranges from some backend--which
should be persistent.

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

The function is expected to return a list markup ranges which are
again represented as lists, as follows:

'('(STARTCHAR ENDCHAR MARKUP-NAME MARKUP-ID) (...))

So, if a markup element consists of more than one
range (discontinous markup), the same values for MARKUP-NAME and
MARKUP-ID must occur in more than one of those lists.

"
  :group 'standoff
  :type 'function)

(defcustom standoff-markup-names-function 'standoff-dummy-markup-element-names
  "A function that returns names of defined (or already used)
markup elements. The function ist expected to return a list of
strings."
  :group 'standoff
  :type 'function)

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

(defcustom standoff-markup-names-function 'standoff-markup-names
  "The name of the function that returns the list of names of
valid markup elements. This should be set according to the
choosen persistent layer."
  :group 'standoff
  :type 'function)

;;
;; Dummy backend
;;

(defun standoff-dummy-write-range (buf startchar endchar markup-name markup-id)
  "A dummy function that does nothing but fullfill the function
definition. The user should definitively replace it with a
function that persists the markup in some backend."
  (let ((markup-identifier (cond
			    ((equal markup-id "n") (+ (standoff-dummy-markup-element-last-id) 1))
			    (t (string-to-number markup-id)))))
    (setq standoff-dummy-backend
	  (cons (list startchar endchar markup-name markup-identifier) 
		standoff-dummy-backend))
    markup-identifier))

(defun standoff-dummy-read-ranges (buf &optional startchar endchar markup-name markup-id)
  "Return the dummy backend, but for this dummy thing we don't
filter for portion of buffer or for name or id of markup
element."
standoff-dummy-backend)

(defun standoff-dummy-markup-element-names ()
  "Return the names of markup elements stored in the dummy backend."
  ;; TODO: remove duplicates
  (mapcar '(lambda (x) (nth 2 x)) standoff-dummy-backend))

(defun standoff-dummy-markup-element-last-id ()
  "Return the id of the last markup element in the dummy backend,
i.e. the highest value."
  (if standoff-dummy-backend
      (apply 'max (mapcar '(lambda (x) (nth 3 x)) standoff-dummy-backend))
    0))

;; we want the dummy backend to be buffer local, so we set it up in a
;; mode hook
(defun standoff-dummy-backend-setup ()
  (setq standoff-dummy-backend nil))
(add-hook 'standoff-mode-hook 'standoff-dummy-backend-setup)

;;
;; creating and deleting stand-off markup
;;

(defun standoff-markup-region (markup-name markup-id)
  "Markup the selected region, i.e. mark the region as a range
of `markup-name' with the id given in `markup-id'."
  (interactive ;;"MMarkup as: \nnId of %s: ")
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
  (interactive)
  (save-restriction
    (widen)
    (overlay-recenter (point))
    (let* ((overlays (overlays-at (point)))
	   (ovly (car overlays))
	   (startchar)
	   (endchar)
	   (markup-name)
	   (markup-id))
      (if (> (length overlays) 1)
	  (error "There is more than one markup element at point. Not deleting anything")
	(setq startchar (overlay-start ovly))
	(setq endchar (overlay-end ovly))
	(setq markup-name (standoff--overlay-property-get ovly "name"))
	(setq markup-id (standoff--overlay-property-get ovly "id"))
	(message (format "%i %i %s %s" startchar endchar markup-name markup-id))))))

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
  (defvar standoff--overlay-property-obarray nil))
(add-hook 'standoff-mode-hook 'standoff--overlay-property-obarray-init)

(defvar standoff--overlay-property-value-format
  "standoff-markup-element-property-symbol-%s-%s")

(defun standoff--overlay-property-format-key (key)
  "Overlay properties are key value pairs where key and value are
symbols. This function returns the key as an interned
symbol. Interference with symbol names of other emacs packages
prevented if you use this function."
  (intern (format "%s" key) standoff--overlay-property-obarray))

(defun standoff--overlay-property-format-value (key value &optional setting)
  "Overlay properties are key value pairs where key and value are
symbols. This function returns the value as an interned symbol
whichs name is made from the key and the value. Interference with
symbol names of other emacs packages prevented if you use this
function."
  (let ((value-formatted (format standoff--overlay-property-value-format key value)))
    (if setting
	(intern value-formatted standoff--overlay-property-obarray)
      (intern-soft value-formatted standoff--overlay-property-obarray))))

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
    (if (not (and value-symbol (intern-soft value-symbol standoff--overlay-property-obarray)))
	nil
      ;; we use (format "%s" ...) to make a string from the symbol
      (substring (format "%s" value-symbol) value-front-length))))

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
   (overlay-put ovly 'local-map standoff-markup-range-local-map)
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

;;
;; Major mode
;;

;; Keymap and Menu

(defvar standoff-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "m" 'standoff-markup-region)
    (define-key map "d" 'standoff-markup-delete-range-at-point)
    (define-key map "D" 'standoff-markup-delete-element-at-point)
    (define-key map "h" 'standoff-hide-markup-at-point)
    (define-key map "ħ" 'standoff-hide-markup-at-point-by-id)
    (define-key map "H" 'standoff-hide-markup-buffer)
    (define-key map "l" 'standoff-highlight-markup-at-point)
    (define-key map "L" 'standoff-highlight-markup-buffer)
    (define-key map "r" 'standoff-relate-markup-element-at-point)
    (define-key map "s" 'standoff-store-markup-element-at-point)
    (define-key map "a" 'standoff-annotate-markup-element-at-point)
    map))

(easy-menu-define standoff-menu standoff-mode-map
  "Menu for standoff mode"
  '("Standoff"
    ["Markup region as ..." standoff-markup-region]
    ["--" nil]
    ["Delete markup range at point" standoff-markup-delete-range-at-point]
    ["Delete markup element at point" standoff-markup-delete-element-at-point]
    ["--" nil]
    ["Store markup element as relation object" standoff-store-markup-element-at-point]
    ["Relate markup element to stored object" standoff-relate-markup-at-point]
    ["--" nil]
    ["Annotate markup element" standoff-annotate-markup-element-at-point]
    ["--" nil]
    ["Highlight markup in buffer" standoff-highlight-markup-buffer]
    ["Highlight markup in region" standoff-highlight-markup-region]
    ["Hide markup in buffer" standoff-hide-markup-buffer]
    ["Hide markup in region" standoff-hide-markup-region]
    ["Hide markup at point" standoff-hide-markup-at-point]
    ["Hide markup with id at point" standoff-hide-markup-at-point-by-id]
    ["--" nil]
    ))

(defvar standoff-markup-range-local-map
  (let ((map (make-sparse-keymap)))
    (define-key map "d" 'standoff-markup-delete-range-at-point)
    (define-key map "D" 'standoff-markup-delete-element-at-point)
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



(provide 'standoff)
