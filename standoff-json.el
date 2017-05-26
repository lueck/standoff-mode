;;; standoff-json.el --- JSON file backend for standoff-mode.

;;; Commentary:
;; Functions for storing and loading annotations in JSON.

;;; Code:

;;(require 'standoff-dummy)

(require 'cl-lib)
(require 'json)
(require 'standoff-dummy)

(defvar standoff-json/json-buffer-name nil
  "The name of the buffer with the external markup.
This is stored in a file local variable of the source buffer.")

(defvar standoff-json/file-positions nil
  "The position for inserting markup.
This is maintained for fast insertion.")

(defun standoff-json/file-reset-positions ()
  "Reset all managed positions in a json file backend."
  (setq-local standoff-json/file-positions
	      (make-hash-table :test 'equal)))

(defun standoff-json/file-add-position (key pos)
  "Add a new position to the list of managed positions.
The name of the position is given as KEY, the position as POS."
  (let ((sym (if (symbolp key) key (intern key))))
    (puthash sym pos standoff-json/file-positions)))

(defun standoff-json/file-adjust-positions (key new-pos)
  "Adjust managed position KEY to NEW-POS."
  (let*
      ((sym (if (symbolp key) key (intern key)))
       (old-pos (gethash sym standoff-json/file-positions))
       (delta 0)
       (new-positions (copy-hash-table standoff-json/file-positions)))
    (when old-pos
      (setq delta (- new-pos old-pos))
      (maphash
       (lambda
	 (ky pos)
	 (when (<= old-pos pos)
	   (puthash ky (+ pos delta) standoff-json/file-positions)))
       standoff-json/file-positions))))

(defun standoff-json/file-get-position (key)
  "Return the managed position for KEY.
Do not use this directly.  Use
`standoff-json/file-get-or-parse-position' instead which parses
the json file when it is not read only."
  (let ((sym (if (symbolp key) key (intern key))))
    (gethash sym standoff-json/file-positions nil)))

(defun standoff-json/file-parse-positions ()
  "Parse the managed positions in a json file backend."
  ;; FIXME: Make more use of json parsing functions
  (let
      (matched
       array-start-pos)
    (standoff-json/file-reset-positions)
    (save-excursion
      ;; parse for md5sum
      (goto-char (point-min))
      (when (re-search-forward
	     "^\"md5sum\":[[:space:]]*\"[[:alnum:]]\\{32\\}\""
	     nil t)
	(standoff-json/file-add-position "md5sum-start" (point)))
      ;; parse for bags
      (goto-char (point-min))
      (while (re-search-forward
	      "^\"\\(MarkupRanges\\|Relations\\|LiteralAttributes\\)\":[[:space:]]*\\["
	      nil t)
	(setq matched (match-string 1))
	;; move point before opening [
	(goto-char (- (point) 1))
	(standoff-json/file-add-position
	 (concat matched "-start")
	 (point))
	(setq array-start-pos (point))
	;; parse the json array. We don't need the result, but the
	;; point is moved.
	(json-read-array)
	;; move point before closing ]
	(unless (< array-start-pos (search-backward "]" nil t))
	  (error "Error parsing json file: no sequence closing \"]\" found for %s" matched))
	(standoff-json/file-add-position
	 (concat matched "-insert")
	 (point))))))

(defun standoff-json/file-get-position-outer-closing ()
  "Get the position of the last closing curly brace."
  (save-excursion
    (goto-char (point-max))
    (search-backward "}")
    (point)))

(defun standoff-json/file-get-or-parse-position (key)
  "Return the managed position of KEY.
If it is not present in the managed positions, the file is
parsed.  If the buffer is not read-only, then the buffer is
parsed first."
  (if buffer-read-only
      (or (standoff-json/file-get-position key)
	  (progn
	    (message "Parsing positions, because not having key '%s'." key)
	    (standoff-json/file-parse-positions)
	    (standoff-json/file-get-position key)))
    ;; if not read only: parse the buffer
    (message "Parsing positions, because json buffer is not read only.")
    (standoff-json/file-parse-positions)
    (standoff-json/file-get-position key)))

(defun standoff-json/file-empty (source-buffer json-buffer)
  "Create an empty json file for storing annotations.
The SOURCE-BUFFER and JSON-BUFFER must be given."
  (with-current-buffer json-buffer
    (save-excursion
      (let ((buffer-read-only nil))
	(erase-buffer)
	(standoff-json/file-reset-positions)
	(goto-char (point-min))
	(insert "{\n")
	(standoff-json/file-add-position "md5sum-start" (point))
	(insert "\"md5sum\": \"" (md5 source-buffer) "\"\n}\n")))))

(defun standoff-json/file-new (source-buffer)
  "Create a new json file backend for SOURCE-BUFFER."
  ;; FIXME: Choose a name if default file present?
  (save-excursion
    (let*
	((default-name (concat (buffer-file-name source-buffer) ".json"))
	 (json-buf-name nil))
      ;; Create a new json file.
      (with-current-buffer (find-file default-name)
	(setq json-buf-name (current-buffer))
	(read-only-mode 1)
	(standoff-json/file-empty source-buffer (current-buffer)))
      ;; set json buffer name in source file.
      (with-current-buffer source-buffer
	(setq-local standoff-json/json-buffer-name json-buf-name)))))
	 
(defun standoff-json/file-get-json-buffer (source-buffer)
  "Return the json buffer for SOURCE-BUFFER."
  (let
      ((json-buf nil)
       (json-buf-name nil))
    (with-current-buffer source-buffer
      (save-excursion
	(setq json-buf-name standoff-json/json-buffer-name)
	(unless json-buf-name
	  (standoff-json/file-new source-buffer)
	  (setq json-buf-name standoff-json/json-buffer-name))
	json-buf-name))))

(defun standoff-json/filter-markup (startchar endchar markup-type markup-inst-id ranges)
  "Filter markup given as list of plists.
Filter markup between STARTCHAR and ENDCHAR or nil.  Filter for
MARKUP-TYPE or nil.  Filter for MARKUP-INST-ID or nil.  RANGES
has to be a list of plists."
  (unless (or (and startchar endchar) (and (null startchar) (null endchar)))
    (error "Use both offsets or none"))
  (cl-remove-if-not			; filter
   #'(lambda (r)
       (let ((start (string-to-number (plist-get r :sourceStart)))
	     (end (string-to-number (plist-get r :sourceEnd)))
	     (elem-id (plist-get r :markupElementId))
	     (typ (plist-get r :qualifiedName)))
	 ;; condition
	 (and (or (and (not startchar) (not endchar))
		  (or (and (<= start startchar)
			   (>= end startchar))
		      (and (>= start startchar)
			   (<= end endchar))
		      (and (<= start endchar)
			   (>= end endchar))))
	      (or (not markup-type)
		  (equal typ markup-type))
	      (or (not markup-inst-id)
		  (equal elem-id markup-inst-id)))))
   ranges))

(defun standoff-json/plist-to-list (range)
  "Convert a plist RANGE to a list representing a markup range.
Return a the range as a list like described in api."
  ;; FIXME: add some more? add markupRangeId!!
  (list
   ;; see api for order
   (plist-get range :markupElementId)
   (plist-get range :qualifiedName)
   (string-to-number (plist-get range :sourceStart))
   (string-to-number (plist-get range :sourceEnd))
   ))

(defun standoff-json/range-to-json (elem-id range-id typ start end)
  "Format a markup range to json.
The range is given by ELEM-ID, RANGE-ID, TYP, START and END
offset."
  (concat
   "{\"tag\": \"MarkupRange\""
   ", \"markupElementId\": \"" elem-id "\""
   ", \"markupRangeId\": \"" range-id "\""
   ", \"qualifiedName\": \"" typ "\""
   ", \"sourceStart\": \"" (number-to-string start) "\""
   ", \"sourceEnd\": \"" (number-to-string end) "\""
   "}"))

(defun standoff-json/file-create-markup (source-buffer start end markup-type)
  "Create an external markup element referring SOURCE-BUFFER.
The range is defined by the character offsets START and END and the
MARKUP-TYPE."
  (let
      ((json-buf (standoff-json/file-get-json-buffer source-buffer)))
    (with-current-buffer json-buf
      (save-excursion
	(let ((insert-pos (standoff-json/file-get-or-parse-position "MarkupRanges-insert"))
	      (elem-id (standoff-util/create-uuid))
	      (range-id (standoff-util/create-uuid))
	      (buffer-read-only nil))
	  ;; insert a comma if this is not the first markup element or range.
	  (if insert-pos
	      ;; There are markup ranges already present.
	      (progn
		(goto-char insert-pos)
		(insert ","))
	    ;; No markup ranges present yet.
	    (goto-char (point-max))
	    (search-backward "}")
	    ;; FIXME: get back to non-whitespace
	    (insert ";\n\n\"MarkupRanges\": []\n")
	    (search-backward "]")
	    (standoff-json/file-add-position "MarkupRanges-insert" (point))
	    (search-backward "[")
	    (standoff-json/file-add-position "MarkupRanges-start" (point))
	    (goto-char (+ (point) 1)))
	  ;; insert a new line with a MarkupRange
	  (insert "\n" (standoff-json/range-to-json elem-id range-id markup-type start end))
	  (standoff-json/file-adjust-positions "MarkupRanges-insert" (point))
	  ;; return element id
	  elem-id)))))

(defun standoff-json/file-read-markup (buffer &optional startchar endchar markup-type markup-inst-id)
  "Read markup form the json file backend of the source BUFFER.
This returns a list of markup elements.  The optional parameters
STARTCHAR ENDCHAR, MARKUP-TYPE MARKUP-INST-ID can be used for
filtering."
  (let
      ((json-buf (standoff-json/file-get-json-buffer buffer))
       (json-array-type 'list)
       (json-object-type 'plist))
    (with-current-buffer json-buf
      (save-excursion
	(let
	    ((json-start (standoff-json/file-get-or-parse-position "MarkupRanges-start")))
	  (if (null json-start)
	      ;; return empty list if there are no markup ranges yet
	      '()
	    ;; else
	    ;; set point to beginning of MarkupRanges
	    (goto-char json-start)
	    (mapcar
	     #'standoff-json/plist-to-list	; make lists form plists
	     (standoff-json/filter-markup	; filter plists
	      startchar endchar markup-type markup-inst-id
	      (json-read-array))))))))) ; parse json array at point into plist

(defun standoff-json/file-add-range (source-buffer start end elem-id)
  "Add a markup range to external markup of SOURCE-BUFFER.
The markup range is given by START and END character offset and
the ELEM-ID of the markup element, that is to be continued."
  (let
      ((json-buf (standoff-json/file-get-json-buffer source-buffer)))
    (with-current-buffer json-buf
      (save-excursion
	(let
	    ((insert-pos (standoff-json/file-get-or-parse-position "MarkupRanges-insert"))
	     (range-id (standoff-util/create-uuid))
	     ;; get the element id by reading and filtering all ranges
	     (markup-type (nth standoff-pos-markup-type
			       (car (standoff-json/file-read-markup
				     source-buffer nil nil nil elem-id))))
	     ;; make json buffer writeable
	     (buffer-read-only nil))
	  (if (null insert-pos)
	      (error "Adding markup range failed: No markup elements yet")
	    (if (null markup-type)
		(error "Adding markup range failed: No markup ranges with element id=%s" elem-id)
	      (goto-char insert-pos)
	      ;; insert the new range
	      (insert ",\n"
		      (standoff-json/range-to-json elem-id range-id markup-type start end))
	      ;; adjust positions
	      (standoff-json/file-adjust-positions "MarkupRanges-insert" (point))
	      elem-id)))))))

(defun standoff-json/file-delete-range (source-buffer start end markup-type elem-id)
  "Delete a markup range or element from the json file backend.
The range refers SOURCE-BUFFER and is identified by START and END
character offsets, by its MARKUP-TYPE and by the ELEM-ID."
  (let
      (range)
    (standoff-json/file-delete-json-object
     source-buffer
     "MarkupRanges"
     #'(lambda
	 (json-plist)
	 (progn
	   (setq range (standoff-json/plist-to-list json-plist))
	   (and		; deletion condition
	    (equal (nth standoff-pos-markup-inst-id range) elem-id)
	    (equal (nth standoff-pos-markup-type range) markup-type)
	    (equal (nth standoff-pos-startchar range) start)
	    (equal (nth standoff-pos-endchar range) end)))))))

(defun standoff-json/file-delete-json-object (source-buffer position-key deletion-predicate)
  "Delete a json object from an array of these objects.
This it takes a SOURCE-BUFFER.  The class of the object to be
deleted is identified is given by POSITION-KEY as string, i.e.
\"MarkupRanges\".  The third parameter DELETION-PREDICATE must be
a function taking a plist parsed from json as single argument and
is expected to return t, if the object represented by the plist
is to be deleted."
  (let
      ((json-buf (standoff-json/file-get-json-buffer source-buffer))
       (pos-key-start (concat position-key "-start"))
       (pos-key-insert (concat position-key "-insert"))
       (deleted 0)
       delta
       new-json-end-pos)
    (with-current-buffer json-buf
      (save-excursion
	(let*
	    ((json-start (standoff-json/file-get-or-parse-position pos-key-start))
	     (json-end (standoff-json/file-get-or-parse-position pos-key-insert))
	     (array-end json-end)
	     object-start
	     first-object-start
	     object-end
	     (json-object-type 'plist)
	     (buffer-read-only nil))
	  (if (null json-start)
	      (error "No %s in json file backend" position-key)
	    (goto-char json-start)
	    ;; move point behind [ and save it to object-start
	    (setq first-object-start (search-forward "[" nil t)
		  object-start first-object-start)
	    (while (search-forward "}" array-end t)
	      ;; search-forward moved the point, go back to saved position
	      (goto-char object-start)
	      ;; move behind comma
	      (json-skip-whitespace)
	      (when (char-equal (json-peek) ?,)
		(json-advance))
	      ;(message "Next chars: %s" (buffer-substring (point) (+ (point) 5)))
	      ;; parse json object and pass it to predicate function
	      (setq range (json-read))
	      (when (funcall deletion-predicate range)
		;; when deleting the first range, the comma behind has to be deleted
		(when (= object-start first-object-start)
		  (json-skip-whitespace)
		  (when (char-equal (json-peek) ?,)
		    (json-advance)))
		(setq object-end (point))
		;; delete object between object-start and object-end
		;(message "Deleting: %s" (buffer-substring object-start object-end))
		(delete-region object-start object-end)
		(setq delta (- object-end object-start)
		      deleted (+ deleted delta)
		      array-end (- array-end delta))
		;; go back to object-start
		(goto-char object-start))
	      ;(message "Parsed range: %s" range)
	      ;; save current position
	      (setq object-start (point)))
	    (setq new-json-end-pos (- json-end deleted))))
	(when (not (= 0 deleted))
	  ;; parse positions after deletion
	  (standoff-json/file-adjust-positions pos-key-insert new-json-end-pos))
	(not (= 0 deleted))))))

(defun standoff-json/file-load-file (file-name)
  "Load the annotations from FILE-NAME into the current buffer."
  (interactive
   (list (read-file-name "File to be loaded: "
			 nil
			 nil
			 'confirm
			 (file-relative-name (concat (buffer-file-name) ".json")))))
  ;; 1) parse the managed positions
  (let (json-buf (find-file file-name))
  (with-current-buffer json-buf
    (save-excursion
      (standoff-json/file-parse-positions)))
  ;; 2) set the file as backend file
  (setq-local standoff-json/json-buffer-name (buffer-name json-buf))
  ;; 3) Highlight all markup
  (require 'standoff-mode)
  (standoff-highlight-markup)))


(defun standoff-json/file-backend-setup ()
  "Set up the json file backend.
This is to be registered as a mode hook."
  (let ((default-file (concat (buffer-file-name) ".json")))
    ;; If default json backend file present...
    (when (file-readable-p default-file)
      ;; ... use it as backend.
      (standoff-json/file-load-file default-file))))


(provide 'standoff-json)

;;; standoff-json ends here
