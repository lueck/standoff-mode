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
  (puthash key pos standoff-json/file-positions))

(defun standoff-json/file-adjust-positions (key new-pos)
  "Adjust managed position KEY to NEW-POS."
  (let
      ((old-pos (gethash key standoff-json/file-positions))
       (delta 0)
       (new-positions (copy-hash-table standoff-json/file-positions)))
    (when old-pos
      (setq delta (- new-pos old-pos))
      (maphash
       (lambda
	 (key pos)
	 (when (>= old-pos pos)
	   (puthash key (+ pos delta) standoff-json/file-positions)))
       standoff-json/file-positions))))

(defun standoff-json/file-get-position (key)
  "Return the managed position for KEY."
  ;; TODO: Handle manual modifications of the buffer.
  (gethash key standoff-json/file-positions nil))

(defun standoff-json/file-parse-positions ()
  "Parse the managed positions in a json file backend."
  (standoff-json/file-reset-positions)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
	    "^\"\\(MarkupRanges\\|Relations\\|LiteralAttributes\\)\":[[:space:]]*\\["
	    nil t)
      (standoff-json/file-add-position
       (concat (match-string 1) "-start")
       (point))
      (unless (search-forward "]" nil t)
	(error "Error parsing json file: no sequence closing \"]\" found for %s"
	       (match-string 1)))
      (standoff-json/file-add-position
       (concat (match-string 1) "-insert")
       (- (point) 1)))))

(defun standoff-json/file-get-position-outer-closing ()
  "Get the position of the last closing curly brace."
  (save-excursion
    (goto-char (point-max))
    (search-backward "}")
    (point)))

(defun standoff-json/file-get-or-parse-position (key)
  "Return the managed position of KEY.
If it is not present in the managed positions, the file is
parsed."
  (or (standoff-json/file-get-position key)
      (progn
	(standoff-json/file-parse-positions)
	(standoff-json/file-get-position key))))
	;; (or (standoff-json/file-get-position key)
	;;     (standoff-json/file-get-position-outer-closing)))))

(defun standoff-json/file-empty (source-buffer json-buffer)
  "Create an empty json file for storing annotations.
The SOURCE-BUFFER and JSON-BUFFER must be given."
  (with-current-buffer json-buffer
    (save-excursion
      (let ((buffer-read-only nil))
	(erase-buffer)
	(goto-char (point-min))
	(insert "{\n\"md5sum\": \"" (md5 source-buffer) "\"\n}\n")
	(standoff-json/file-reset-positions)))))

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
	(standoff-json/file-empty source-buffer (current-buffer)))
      ;; set json buffer name in source file.
      (with-current-buffer source-buffer
	(setq-local standoff-json/json-buffer-name json-buf-name)))))
	 
(defun standoff-json/file-get-json-buffer (source-buffer)
  "Return the json buffer for SOURCE-BUFFER."
  (let
      ((json-buf nil)
       (json-buf-name))
    (with-current-buffer source-buffer
      (save-excursion
	(setq json-buf-name standoff-json/json-buffer-name)
	(unless json-buf-name
	  (standoff-json/file-new source-buffer)
	  (setq json-buf-name standoff-json/json-buffer-name))))
    (set-buffer json-buf-name)))

(defun standoff-json/filter-markup (startchar endchar markup-type markup-inst-id ranges)
  "Filter markup given as list of plists.
Filter markup between STARTCHAR and ENDCHAR or nil.  Filter for
MARKUP-TYPE or nil.  Filter for MARKUP-INST-ID or nil.  RANGES
has to be a list of plists."
  (cl-remove-if-not			; filter
   #'(lambda (r)
       (let ((start (plist-get 'sourceStart r))
	     (end (plist-get 'sourceEnd r))
	     (elem-id (plist-get 'markupElementId r))
	     (typ (plist-get 'qualifiedName r)))
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
   (plist-get 'qualifiedName range)
   (plist-get 'sourceStart range)
   (plist-get 'sourceEnde range)
   (plist-get 'markupElementId range)))

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

(defun standoff-json/file-add-markup (source-buffer start end markup-type)
  "Add a markup range to the external markup of SOURCE-BUFFER.
The range is defined by the character offsets START and END and the
MARKUP-TYPE."
  (let
      ((json-buf (standoff-json/file-get-json-buffer source-buffer)))
    (with-current-buffer json-buf
      (save-excursion
	(let ((insert-pos (standoff-json/file-get-or-parse-position "MarkupRanges-insert"))
	      (elem-id (standoff-dummy-create-uuid))
	      (range-id (standoff-dummy-create-uuid))
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
	    (goto-char (+ (point) 1))
	    (standoff-json/file-add-position "MarkupRanges-start" (point)))
	  ;; insert a new line with a MarkupRange
	  (insert "\n" (standoff-json/range-to-json elem-id range-id markup-type start end))
	  (standoff-json/file-adjust-positions "MarkupRanges-insert" (point))
	  ;; return element id
	  elem-id)))))

(defun standoff-json/file-read-markup (buffer &optional startchar endchar markup-type markup-inst-id)
  "Read markup form the json file backend of the source BUFFER.
The optional parameters STARTCHAR ENDCHAR, MARKUP-TYPE
MARKUP-INST-ID can be used for filtering."
  (let
      ((json-buf (standoff-json/file-get-json-buffer buffer))
       (json-array-type 'list)
       (json-object-type 'plist))
    (with-current-buffer json-buf
      (save-excursion
	;; set point to beginning of MarkupRanges
	(goto-char (standoff-json/file-get-or-parse-position "MarkupRanges-start"))
	(mapcar
	 #'standoff-json/plist-to-list	; make lists form plists
	 (standoff-json/filter-markup	; filter plists
	  startchar endchar markup-type markup-inst-id
	  (json-read-array))))))) ; parse json array at point into plist

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
