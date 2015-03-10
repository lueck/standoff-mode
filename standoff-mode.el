;; standoff.el -- An Emacs mode for producing stand-off markup

(defcustom standoff-markup-function 'standoff-markup-function-dummy
  "The function that makes the markup persistent in some backend.
The default value is `standoff-markup-function-dummy' which does
not persist anything. Use this variable to choose an elaborate
backend.

The function must take the following arguments:

BUFFER STARTCHAR ENDCHAR MARKUP-NAME MARKUP-ID

where MARKUP-ID can ether be an integer value or a single
character key signalling how to handle the ID:

`i': auto-increment the id; the backend has to take control over
     the ID.

The function is expected to return an integer ID of the markup
element (not the range). In case storing to the backend was not
successfull it is expected to return `nil'.
"
  :group 'standoff-mode
  :type 'function)

(defcustom standoff-get-markup-function 'standoff-get-markup-function-dummy
  "The function that get the markup elements from some persistent backend.

The function must take the following arguments:

BUFFER &optional STARTCHAR ENDCHAR MARKUP-NAME MARKUP-ID

"
  :group 'standoff
  :type 'function)

(defcustom standoff-markup-post-functions nil
  "A hook for handlers called when a region was markupd und the
markup was successfully stored to the backend--i.e. none of
the functions hooked to `standoff-markup-functions' did
returned `nil'. This hook should be used for highlightning and
notifications.

This is a so called `abnormal' hook, i.e. the hooked
functions (aka handlers) must take the following arguments:

BUFFER STARTCHAR ENDCHAR MARKUP-NAME MARKUP-ID

"
  :group 'standoff-mode
  :type 'hook
  :options '(standoff-notify-markup))

(defcustom standoff-markup-names-function 'standoff-markup-names
  "The name of the function that returns the list of names of
valid markup elements. This should be set according to the
choosen persistent layer."
  :group 'standoff-mode
  :type 'function)

(defun standoff-markup-function-dummy (buf startchar endchar markup-name markup-type)
  "A dummy function that does nothing but fullfill the function
definition. The user should definitively replace it with a
function that persists the markup in some backend."
  (cond
   ((equal markup-id "i") -1);; silly, but visible 
   (t (string-to-number markup-id))))

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
	(let* ((id-from-backend (funcall standoff-markup-function buf (region-beginning) (region-end) markup-name markup-id)))
	  (when id-from-backend
	    ;; run hook to notify success and highlight the new markup
	    (run-hook-with-args 'standoff-markup-post-functions buf (region-beginning) (region-end) markup-name id-from-backend)))))
    (deactivate-mark)))

(defun standoff-minibuffer-require-id-or-key ()
  (let ((enable-recursive-minibuffers t)
	 (mini-input (read-from-minibuffer "Id of markup element or <i> for automatic incrementation: ")))
    (if (string-match "^\\([0-9]+\\|i\\)$" mini-input)
	(match-string 0 mini-input)
      (standoff-minibuffer-require-id-or-key))))

(defun standoff-markup-names ()
  "Return the list of user defined markup elements."
  (mapcar 'car standoff-markup-overlays))

(defcustom standoff-markup-require-name-require-match 'confirm
  "Defines how restrictive the markup name is handled. If set to
`t' then it must match a list element returned by
standoff-markup-names-function. If set to `nil', the user may
exit his input with any name. If set to `confirm' (symbol), the
user may exit with any name, but is asked to confirm his input."
  :group 'standoff-mode)

(defun standoff-minibuffer-require-markup-name ()
  "Ask the user for the name of the  "
  (completing-read "Name of markup element: " 
		   (funcall standoff-markup-names-function) 
		   nil
		   standoff-markup-require-name-require-match))

(defun standoff-markup-notify (buf startchar endchar markup-name markup-id)
  "A handler function that can be hooked to the
standoff-markup-functions hook, which is called when ever an
markup is being done. This function does nothing but notify
the user with a message in the minibuffer (and in the *Messages*
buffer)."
  (message "Annotating from %i to %i as %s with id %i." 
	   startchar endchar markup-name markup-id))

(add-hook 'standoff-markup-post-functions 'standoff-markup-notify)




(defun standoff-markup-remove-at-point ()
  "Delete the markup at the point."
  (interactive)
  
  ;; update hightlightning
  (standoff-remove-highlightning (current-buffer))
  (standoff-create-hightlightning (current-buffer)))


;; Obsolte

(defcustom standoff-text-element "text"
  "The name of the xml element, which holds the markup
layer. Without tags < >."
  :group 'standoff-mode
  :type 'string)

(defun standoff-text-offset ()
  "Get the offset, where the annotatable text starts. This parses
  for the element name defined in `standoff-text-element'."
  ;; TODO handle narrowing that starts after offset 
  (or (save-excursion
	(goto-char (point-min))
	(search-forward (format "<%s>" standoff-text-element) nil t))
      0))


(defun standoff-test ()
  "This is for testing non-interactive functions."
  (interactive)
  (message "%i" (standoff-text-offset)))


;; HIGHLIGHTNING
;; =============

(defcustom standoff-markup-overlays nil
  "This should be a alist defined by the user."
  :group 'standoff-mode
  :type 'alist)

(defcustom standoff-markup-overlays-front nil
  "This should be a alist defined by the user."
  :group 'standoff-mode
  :type 'alist)

(defcustom standoff-markup-overlays-after nil
  "This should be a alist defined by the user."
  :group 'standoff-mode
  :type 'alist)

(defcustom standoff-markup-overlays-default 
  '(('face (:background "light grey")))
  "Overlay properties for markup elements not defined in
`standoff-markup-overlays'."
  :group 'standoff-mode
  :type 'alist)

(defcustom standoff-markup-overlays-front-default 
  '(('face (:background "light grey" :foreground "dark grey")))
  "Text properties of the front string of markup
oferlays. This is used for markup elements not defined in
`standoff-markup-overlays-after'."
  :group 'standoff-mode
  :type 'alist)

(defcustom standoff-markup-overlays-after-default 
  '(('face (:background "light grey" :foreground "dark grey")))
  "Text properties of the after string which trails markup
overlays. This is used for markup elements not defined in
`standoff-markup-overlays-after'."
  :group 'standoff-mode
  :type 'alist)

(defun standoff-highlight-markup (buf startchar endchar markup-name markup-id)
"Highlight an markup given as an elisp list expression."
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
   (overlay-put ovly 'name markup-name)
   (overlay-put ovly 'id markup-id)
   (overlay-put ovly 'local-map standoff-markup-local-map)
   )))

(add-hook 'standoff-markup-post-functions 'standoff-highlight-markup)


(defun standoff-remove-highlightning-buffer (markup-name)
  "Remove all hightlightning in the current buffer."
  (interactive
   (list (completing-read "Name of markup element, <!> for all: "
			  (funcall standoff-markup-names-function)
			  nil
			  nil)))
  (save-excursion
    (cond
     ((equal markup-name "!") (remove-overlays))
     (t (remove-overlays (point-min) (point-max) 'name markup-name)))))

(defun standoff-remove-highlightning-region (area-start area-end)
  "Remove highlightning in the region."
  (interactive "r")
  (save-excursion
    (overlay-recenter area-end)
    (mapc 'delete-overlay (overlays-in area-start area-end))))

(defun standoff-remove-highlightning-at-point ()
  "Remove highlightning at the point."
  (interactive)
  (save-excursion
    (overlay-recenter (point))
    (mapc 'delete-overlay (overlays-at (point)))))

(defun standoff-show-markup-region (beg end markup-name)
  "Create overlays for all markup in the backend."
  (interactive 
   (list 
    (region-beginning)
    (region-end)
    (completing-read "Name of markup elements to show, <!> for all: "
			  (cons "!" (funcall standoff-markup-names-function))
			  nil t)))
  (let ((markup-name-or-t (cond ((equal markup-name t) t)
				(t markup-name)))
	(markup-elements (funcall standoff-get-markup-function (current-buffer) beg end markup-name)))
    ;; build a list from BUF STARTCHAR ENDCHAR MARKUP-NAME MARKUP-ID and apply it to ..
    (mapc '(lambda (x) apply 'standoff-highlight-markup (cons (current-buffer) x)))))


(defun standoff-show-markup-buffer (markup-name)
  "Create overlays for all markup in the backend."
  (interactive 
   (list (completing-read "Name of markup elements to show, <!> for all: "
			  (cons "!" (funcall standoff-markup-names-function))
			  nil t)))
  (standoff-show-markup-region (point-min) (point-max) markup-name))


;;
;; Keymap and Menu
;;

;; This should be a major mode and it should at first make the buffer read only!
;; Then we don't need any escape and prefix keys but only
;; m -- markup-region
;; h -- hide (global map: on buffer, ... on region, local-map: on overlay)
;; d -- delete
;; r -- relate 


(defvar standoff-markup-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-cm" 'standoff-markup-region)
    map))

(define-minor-mode standoff-markup-minor-mode
  "Toggle the minor mode to *produce* standoff markup."
  :init-value nil
  :lighter " standoff"
  :keymap standoff-markup-minor-mode-map
  :group 'standoff)

(easy-menu-define standoff-menu standoff-markup-minor-mode-map
  "Menu for standoff mode"
  '("Standoff"
    ["Markup region as ..." standoff-markup-region]
    ["--" nil]
    ["Hide markup in buffer" standoff-remove-highlightning-buffer]
    ["Hide markup in region" standoff-remove-highlightning-region]
    ["Hide markup at point" standoff-remove-highlightning-at-point]
    ["Show markup in buffer" standoff-show-markup-buffer]
    ["Show markup in region" standoff-show-markup-region]
    ))

;; (defvar
(setq standoff-markup-local-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-d" 'standoff-remove-markup-at-point)
    (define-key map "\C-c\C-r" 'standoff-relate-markup-at-point)
    (define-key map "\C-c\C-h" 'standoff-remove-highlightning-at-point)
    map))


;; (defvar standoff-markup-menu-bar-menu
;;   (let ((menu (make-sparse-keymap "Standoff")))
;;     (define-key menu [standoff-markup-region]
;;       '(menu-item "Markup selected region" standoff-markup-region
;; 		  :help ""
;; 		  :key-sequence "\C-cm"))
;;     (define-key menu [separator-standoff-create]
;;       '(menu-item "--"))
;;     menu))



(provide 'standoff)
