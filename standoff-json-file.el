;;; standoff-json-file.el --- JSON file backend for standoff-mode.

;; Copyright (C) 2017, 2021 Christian Lück

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

;; Functions for storing to and loading annotations from a JSON file.

;; To use this, make it load after standoff-mode is loaded:

;; (eval-after-load 'standoff-mode (require 'standoff-json-file))

;; See standoff-json.el for customization options.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'standoff-api)
(require 'standoff-util)
(require 'standoff-json)
(require 'standoff-log)

;;;; silencing the compiler

(declare-function standoff-highlight-markup "standoff-mode.el" nil)

;;;; internal varibales

(defvar standoff-json-file/json-buffer-name nil
  "The name of the buffer with the external markup.
This is stored in a file local variable of the source buffer.")

;;;; Managing positions in the json file

(defvar standoff-json-file/positions nil
  "The position for inserting markup.
This is maintained for fast insertion.")

(defun standoff-json-file/reset-positions ()
  "Reset all managed positions in a json file backend."
  (setq-local standoff-json-file/positions
	      (make-hash-table :test 'equal)))

(defun standoff-json-file/add-position (key pos)
  "Add a new position to the list of managed positions.
The name of the position is given as KEY, the position as POS."
  (let ((sym (if (symbolp key) key (intern key))))
    (puthash sym pos standoff-json-file/positions)))

(defun standoff-json-file/adjust-positions (key new-pos)
  "Adjust managed position KEY to NEW-POS."
  (let*
      ((sym (if (symbolp key) key (intern key)))
       (old-pos (gethash sym standoff-json-file/positions))
       (delta 0)
       (new-positions (copy-hash-table standoff-json-file/positions)))
    (when old-pos
      (setq delta (- new-pos old-pos))
      (maphash
       (lambda
	 (ky pos)
	 (when (<= old-pos pos)
	   (puthash ky (+ pos delta) standoff-json-file/positions)))
       standoff-json-file/positions))))

(defun standoff-json-file/get-position (key)
  "Return the managed position for KEY.
Do not use this directly.  Use
`standoff-json-file/get-or-parse-position' instead which parses
the json file when it is not read only."
  (let ((sym (if (symbolp key) key (intern key))))
    (gethash sym standoff-json-file/positions nil)))

(defun standoff-json-file/parse-positions ()
  "Parse the managed positions in a json file backend."
  ;; FIXME: Make more use of json parsing functions
  (let
      (matched
       array-start-pos)
    (standoff-json-file/reset-positions)
    (save-excursion
      ;; parse for md5sum
      (goto-char (point-min))
      (when (re-search-forward
	     "^\"md5sum\":[[:space:]]*\"[[:alnum:]]\\{32\\}\""
	     nil t)
	(standoff-json-file/add-position "md5sum-start" (point)))
      ;; parse for bags
      (goto-char (point-min))
      (while (re-search-forward
	      "^\"\\(MarkupRanges\\|Relations\\|LiteralAttributes\\)\":[[:space:]]*\\["
	      nil t)
	(setq matched (match-string 1))
	;; move point before opening [
	(goto-char (- (point) 1))
	(standoff-json-file/add-position
	 (concat matched "-start")
	 (point))
	(setq array-start-pos (point))
	;; parse the json array. We don't need the result, but the
	;; point is moved.
	(json-read-array)
	;; move point before closing ]
	(unless (< array-start-pos (search-backward "]" nil t))
	  (error "Error parsing json file: no sequence closing \"]\" found for %s" matched))
	(standoff-json-file/add-position
	 (concat matched "-insert")
	 (point))))))

(defun standoff-json-file/get-position-outer-closing ()
  "Get the position of the last closing curly brace."
  (save-excursion
    (goto-char (point-max))
    (search-backward "}")
    (point)))

(defun standoff-json-file/get-or-parse-position (key)
  "Return the managed position of KEY.
If it is not present in the managed positions, the file is
parsed.  If the buffer is not read-only, then the buffer is
parsed first."
  (if buffer-read-only
      (or (standoff-json-file/get-position key)
	  (progn
	    (standoff-log "JSON file backend: Parsing positions, because not having key '%s'.\n"
			  key)
	    (standoff-json-file/parse-positions)
	    (standoff-json-file/get-position key)))
    ;; if not read only: parse the buffer
    (standoff-log "JSON file backend: Parsing positions, because json buffer is not read only.\n")
    (standoff-json-file/parse-positions)
    (standoff-json-file/get-position key)))

;;;; Getting the json file or buffer

(defun standoff-json-file/empty (source-buffer json-buffer)
  "Create an empty json file for storing annotations.
The SOURCE-BUFFER and JSON-BUFFER must be given."
  (with-current-buffer json-buffer
    (save-excursion
      (let ((buffer-read-only nil))
	(erase-buffer)
	(standoff-json-file/reset-positions)
	(goto-char (point-min))
	(insert "{\n")
	(standoff-json-file/add-position "md5sum-start" (point))
	(insert "\"md5sum\": \"" (md5 source-buffer) "\"\n}\n")))))

(defun standoff-json-file/new (source-buffer)
  "Create a new json file backend for SOURCE-BUFFER."
  ;; FIXME: Choose a name if default file present?
  (save-excursion
    (let*
	((default-name (concat (buffer-file-name source-buffer) ".json"))
	 (json-buf-name nil))
      ;; Create a new json file.
      (with-current-buffer (find-file-noselect default-name)
	(setq json-buf-name (current-buffer))
	(read-only-mode 1)
	(standoff-json-file/empty source-buffer (current-buffer)))
      ;; set json buffer name in source file.
      (with-current-buffer source-buffer
	(setq-local standoff-json-file/json-buffer-name json-buf-name)))))
	 
(defun standoff-json-file/get-json-buffer (source-buffer)
  "Return the json buffer for SOURCE-BUFFER."
  (let
      ((json-buf nil)
       (json-buf-name nil))
    (with-current-buffer source-buffer
      (save-excursion
	(setq json-buf-name standoff-json-file/json-buffer-name)
	(unless json-buf-name
	  (standoff-json-file/new source-buffer)
	  (setq json-buf-name standoff-json-file/json-buffer-name))
	json-buf-name))))

;;;; General functions for accessing the json file.

(defun standoff-json-file/create-object (source-buffer object-type serialized)
  "Write a new object to json file backend.
The source must be given in SOURCE-BUFFER.  The object, the type
of which must be given in OBJECT-TYPE, must be given as a
SERIALIZED json string."
  (let
      ((json-buf (standoff-json-file/get-json-buffer source-buffer)))
    (with-current-buffer json-buf
      (save-excursion
	(let*
	    ((pos-start-key (concat object-type "-start"))
	     (pos-insert-key (concat object-type "-insert"))
	     (insert-pos (standoff-json-file/get-or-parse-position pos-insert-key))
	     (buffer-read-only nil))
	  ;; insert a comma if this is not the first object of this type.
	  (if insert-pos
	      ;; There are objects of this type already present.
	      (progn
		(goto-char insert-pos)
		;; Test if there is already a object. See issue #3
		(re-search-backward "[^[:space:]]" nil t)
		(if (equal (match-string 0) "}")
		    (progn 
		      (goto-char insert-pos)
		      (insert ","))
		  (goto-char insert-pos)))
	    ;; No objects of this type present yet.
	    (goto-char (point-max))
	    (search-backward "}")
	    ;; FIXME: get back to non-whitespace
	    (insert ",\n\n\"" object-type "\": []\n")
	    (search-backward "]")
	    (standoff-json-file/add-position pos-insert-key (point))
	    (search-backward "[")
	    (standoff-json-file/add-position pos-start-key (point))
	    (goto-char (+ (point) 1)))
	  ;; insert a new line with the serialized object
	  (insert "\n" serialized)
	  (standoff-json-file/adjust-positions pos-insert-key (point)))))))

(defun standoff-json-file/read-objects (source-buffer object-type filter-fun from-plist-fun)
  "Read and filter annotations from json file backend.
The source must be given in SOURCE-BUFFER, the type of
annotations in OBJECT-TYPE, e.g. \"MarkupRanges\".  FILTER-FUN
must be a function for filtering the objects in the json array,
it must take a single argument, a json plist
object. FROM-PLIST-FUN must be a factory, that produces internal
data from json.  Returns a list of objects that passed the filters."
  (let
      ((json-buf (standoff-json-file/get-json-buffer source-buffer))
       (json-array-type 'list)
       (json-object-type 'plist))
    (with-current-buffer json-buf
      (save-excursion
	(let*
	    ((pos-start-key (concat object-type "-start"))
	     (json-start (standoff-json-file/get-or-parse-position pos-start-key)))
	  (if (null json-start)
	      ;; return empty list if there are no markup ranges yet
	      '()
	    ;; set point to beginning of MarkupRanges
	    (goto-char json-start)
	    (mapcar
	     from-plist-fun		; plist to internal objects
	     (funcall
	      filter-fun		; pass parsed to filter
	      (json-read-array)))))))))	; parse json array at point

(defun standoff-json-file/delete-json-object (source-buffer object-type deletion-predicate)
  "Delete a json object from an array of these objects.
This it takes a SOURCE-BUFFER.  The class of the object to be
deleted is identified is given by OBJECT-TYPE as string, i.e.
\"MarkupRanges\".  The third parameter DELETION-PREDICATE must be
a function taking a plist parsed from json as single argument and
is expected to return t, if the object represented by the plist
is to be deleted."
  (let
      ((json-buf (standoff-json-file/get-json-buffer source-buffer))
       (pos-key-start (concat object-type "-start"))
       (pos-key-insert (concat object-type "-insert"))
       (deleted 0)
       delta
       new-json-end-pos)
    (with-current-buffer json-buf
      (save-excursion
	(let*
	    ((json-start (standoff-json-file/get-or-parse-position pos-key-start))
	     (json-end (standoff-json-file/get-or-parse-position pos-key-insert))
	     (array-end json-end)
	     object-start
	     first-object-start
	     object-end
	     (json-object-type 'plist)
	     (buffer-read-only nil)
	     range)
	  (if (null json-start)
	      (error "No %s in json file backend" object-type)
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
		(standoff-log "JSON file backend: %s %s deleted.\n" object-type range)
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
	  (standoff-json-file/adjust-positions pos-key-insert new-json-end-pos))
	(not (= 0 deleted))))))

;;;; Markup

(defun standoff-json-file/create-markup (source-buffer start end markup-type)
  "Create an external markup element referring SOURCE-BUFFER.
The range is defined by the character offsets START and END and the
MARKUP-TYPE."
  (let*
      ((elem-id (standoff-util/create-uuid))
       (range-id (standoff-util/create-uuid))
       (json (standoff-json/range-to-json elem-id range-id markup-type start end)))
    (standoff-json-file/create-object source-buffer "MarkupRanges" json)
    (standoff-log "JSON file backend: Markup range %s created.\n" elem-id)
    ;; return element id
    elem-id))

(defun standoff-json-file/read-markup (source-buffer &optional startchar endchar markup-type markup-inst-id)
  "Read markup form the json file backend of the SOURCE-BUFFER.
This returns a list of markup elements.  The optional parameters
STARTCHAR ENDCHAR, MARKUP-TYPE MARKUP-INST-ID can be used for
filtering."
  (standoff-json-file/read-objects
   source-buffer
   "MarkupRanges"
   #'(lambda
       (ranges)
       (standoff-json/filter-markup
	ranges startchar endchar markup-type markup-inst-id))
   #'standoff-json/range-plist-to-internal))

(defun standoff-json-file/add-range (source-buffer start end elem-id)
  "Add a markup range to external markup of SOURCE-BUFFER.
The markup range is given by START and END character offset and
the ELEM-ID of the markup element, that is to be continued."
  (let
      ((json-buf (standoff-json-file/get-json-buffer source-buffer)))
    (with-current-buffer json-buf
      (save-excursion
	(let
	    ((insert-pos (standoff-json-file/get-or-parse-position "MarkupRanges-insert"))
	     (range-id (standoff-util/create-uuid))
	     ;; get the element id by reading and filtering all ranges
	     (markup-type (nth standoff-pos-markup-type
			       (car (standoff-json-file/read-markup
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
	      (standoff-json-file/adjust-positions "MarkupRanges-insert" (point))
	      (standoff-log "JSON file backend: Markup range added to %s.\n" elem-id)
	      elem-id)))))))

(defun standoff-json-file/delete-range (source-buffer start end markup-type elem-id)
  "Delete a markup range or element from the json file backend.
The range refers SOURCE-BUFFER and is identified by START and END
character offsets, by its MARKUP-TYPE and by the ELEM-ID."
  (let (range)
    (standoff-json-file/delete-json-object
     source-buffer
     "MarkupRanges"
     #'(lambda
	 (json-plist)
	 (progn
	   (setq range (standoff-json/range-plist-to-internal json-plist))
	   (and		; deletion condition
	    (equal (nth standoff-pos-markup-inst-id range) elem-id)
	    (equal (nth standoff-pos-markup-type range) markup-type)
	    (equal (nth standoff-pos-startchar range) start)
	    (equal (nth standoff-pos-endchar range) end)))))))

(defun standoff-json-file/markup-types (source-buffer)
  "Return a list of markup types used in SOURCE-BUFFER."
  (let
      ((json-buf (standoff-json-file/get-json-buffer source-buffer)))
    (with-current-buffer json-buf
      (save-excursion
	(let
	    ((typeshash (make-hash-table :test 'equal))
	     (types '())
	     (ranges (standoff-json-file/read-markup source-buffer)))
	  (mapc #'(lambda (range) (puthash (nth standoff-pos-markup-type range) 1 typeshash))
		ranges)
	  ;; get all the keys from the hash table
	  (maphash #'(lambda (k v) (setq types (cons k types))) typeshash)
	  ;; return types
	  types)))))

;;;; Relations

(defun standoff-json-file/create-relation (source-buffer subj pred obj)
  "Create a relation of type PRED between SUBJ and OBJ.
Must be given as first argument.  The id of the new relation is returned."
  (let*
      ((rel-id (standoff-util/create-uuid))
       (json (standoff-json/relation-to-json rel-id subj pred obj)))
    (standoff-json-file/create-object source-buffer "Relations" json)
    (standoff-log "JSON file backend: Relation %s created.\n" rel-id)
    ;; return relation id
    rel-id))

(defun standoff-json-file/read-relations (source-buffer &optional sub pred obj rel-id)
  "Read relations in SOURCE-BUFFER from json file backend.
The relations may be filtered by SUB, PRED, OBJ and REL-ID."
  (standoff-json-file/read-objects
   source-buffer
   "Relations"
   #'(lambda
       (rels)
       (standoff-json/filter-relations
	rels sub pred obj rel-id))
   #'standoff-json/relation-plist-to-internal))

(defun standoff-json-file/delete-relation (source-buffer subj pred obj &optional rel-id)
  "Delete all relations in SOURCE-BUFFER matching SUBJ, PRED, OBJ or REL-ID."
  (let (rel)
    (standoff-json-file/delete-json-object
     source-buffer
     "Relations"
     #'(lambda
	 (json-plist)
	 (progn
	   (setq rel (standoff-json/relation-plist-to-internal json-plist))
	   (or				; deletion condition
	    (and rel-id			; rel-id given: only check id
		 (equal (nth standoff-pos-relation-id rel) rel-id))
	    (and	       ; no rel-id given: check sub, pred, obj
	     (equal (nth standoff-pos-subject rel) subj)
	     (equal (nth standoff-pos-predicate rel) pred)
	     (equal (nth standoff-pos-object rel) obj))))))))

(defun standoff-json-file/used-predicates (source-buffer subject-id object-id)
  "Return the predicates used for the combination of subject and object types.
The existing relations in SOURCE-BUFFER are examined for similar
combinations of markup types.  The markup types are determined
using SUBJECT-ID and OBJECT-ID."
  ;; FIXME: do not hardcode json properties
  (let*
      ((subject-type (nth
		      standoff-pos-markup-type
		      (car (standoff-json-file/read-markup source-buffer nil nil nil subject-id))))
       (object-type (nth
		     standoff-pos-markup-type
		     (car (standoff-json-file/read-markup source-buffer nil nil nil object-id))))
       subj-type
       obj-type)
    ;;(message "Subject type: '%s' Object type: '%s'" subject-type object-type)
    (standoff-json-file/read-objects
     source-buffer
     "Relations"
     #'(lambda
	 (rels)
	 (cl-remove-if-not
	  #'(lambda
	      (rel)
	      (progn
		;;(message "Examining: %s" rel)
		(setq subj-type (nth standoff-pos-markup-type (car (standoff-json-file/read-markup source-buffer nil nil nil (plist-get rel :subjectId)))))
		(setq obj-type (nth standoff-pos-markup-type (car (standoff-json-file/read-markup source-buffer nil nil nil (plist-get rel :objectId)))))
		;; FIXME: Bug: the equality test for the object type does not work
		;;(message "types are: '%s' '%s'" subj-type obj-type)
		;;(message "equal sub: %s equal obj: %s" (equal subj-type subject-type) (equal obj-type object-type))
		;;(message "type-of: %s %s" (type-of subj-type) (type-of obj-type))
		(and ;(or (null subject-id)
			 (equal subj-type subject-type);)
		     ;(or (null object-id)
			 (equal obj-type object-type);)
		     t)
		))
	  rels))
     #'(lambda
	 (rel)
	 (plist-get rel :predicate)))))

;;;; Literals

(defun standoff-json-file/create-literal (source-buffer subj key value)
  "Create a literal attribute of type KEY with VALUE on markup element SUBJ.
Must be given as first argument.  The id of the new literal
attribute is returned."
  (let*
      ((lit-id (standoff-util/create-uuid))
       (json (standoff-json/literal-to-json lit-id subj key value)))
    (standoff-json-file/create-object source-buffer "Literals" json)
    ;; return literal id
    (standoff-log "JSON file backend: Created literal %s.\n" lit-id)
    lit-id))

(defun standoff-json-file/read-literals (source-buffer &optional sub key value value-regex lit-id)
  "Read literal attributes in SOURCE-BUFFER from json file backend.
The literal attributes may be filtered by SUB, KEY, VALUE and LIT-ID."
  (standoff-json-file/read-objects
   source-buffer
   "Literals"
   #'(lambda
       (lits)
       (standoff-json/filter-literals
	lits sub key value value-regex lit-id))
   #'standoff-json/literal-plist-to-internal))

(defun standoff-json-file/delete-literal (source-buffer literal-id)
  "Delete all literal attributes in SOURCE-BUFFER matching LITERAL-ID."
  (standoff-json-file/delete-json-object
   source-buffer
   "Literals"
   #'(lambda
       (json-plist)
       ;; deletion condition
       (equal
	literal-id
	(nth standoff-pos-literal-id (standoff-json/literal-plist-to-internal json-plist))))))

(defun standoff-json-file/used-literal-keys (source-buffer &optional subject-id)
  "Return the keys of literal attributes used for this type of markup.
The existing literal attributes in SOURCE-BUFFER are examined.
The markup type is determined using SUBJECT-ID."
  ;; FIXME: do not hardcode json properties
  (let*
      ((subject-type (nth
		      standoff-pos-markup-type
		      (car (standoff-json-file/read-markup source-buffer nil nil nil subject-id))))
       subj-type)
    (standoff-json-file/read-objects
     source-buffer
     "Literals"
     #'(lambda
	 (lits)
	 (cl-remove-if-not
	  #'(lambda
	      (lit)
	      (progn
		(setq subj-type (nth standoff-pos-markup-type (car (standoff-json-file/read-markup source-buffer nil nil nil (plist-get lit :subjectId)))))
		(and (or (null subject-id)
			 (equal subj-type subject-type)))
		))
	  lits))
     #'(lambda
	 (lit)
	 (plist-get lit :key)))))



;;;; Loading

(defun standoff-json-file/load-file (file-name)
  "Load the annotations from FILE-NAME into the current buffer."
  (interactive
   (list (read-file-name "File to be loaded: "
			 nil
			 nil
			 'confirm
			 (file-relative-name (concat (buffer-file-name) ".json")))))
  (let
      ;; 1) open the json file
      ((json-buf (find-file-noselect file-name)))
    (with-current-buffer json-buf
      ;; 2) make it read only
      (read-only-mode 1)
      ;; 3) parse managed positions
      (standoff-json-file/parse-positions))
    ;; 4) set the file as backend file
    (setq-local standoff-json-file/json-buffer-name (buffer-name json-buf))
    ;; 5) Highlight all markup
    (require 'standoff-mode)
    (standoff-highlight-markup)))

(defun standoff-json-file/backend-setup ()
  "Set up the json file backend.
This is to be registered as a mode hook."
  (let ((default-file (concat (buffer-file-name) ".json")))
    ;; If default json backend file present...
    (when (file-readable-p default-file)
      ;; ... use it as backend.
      (standoff-json-file/load-file default-file))))

(add-hook 'standoff-mode-hook 'standoff-json-file/backend-setup)

;;;; Registration

(defun standoff-json-file/register-backend ()
  "Register the json file backend.
This should be used as a mode hook to standoff-mode."
  (setq
   ;; markup
   standoff-markup-create-function 'standoff-json-file/create-markup
   standoff-markup-range-add-function 'standoff-json-file/add-range
   standoff-markup-read-function 'standoff-json-file/read-markup
   standoff-markup-delete-range-function 'standoff-json-file/delete-range
   standoff-markup-types-used-function 'standoff-json-file/markup-types
   ;; relations
   standoff-predicates-used-function 'standoff-json-file/used-predicates
   standoff-relation-create-function 'standoff-json-file/create-relation
   standoff-relations-read-function 'standoff-json-file/read-relations
   standoff-relations-delete-function 'standoff-json-file/delete-relation
   ;; literals
   standoff-literal-keys-used-function 'standoff-json-file/used-literal-keys
   standoff-literal-create-function 'standoff-json-file/create-literal
   standoff-literals-read-function 'standoff-json-file/read-literals
   standoff-literal-delete-function 'standoff-json-file/delete-literal
   ))

(add-hook 'standoff-mode-hook 'standoff-json-file/register-backend)

(provide 'standoff-json-file)

;;; standoff-json-file ends here
