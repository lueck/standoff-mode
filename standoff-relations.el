;;; standoff-relations.el --- part of standoff-mode, a mode for creating stand-off markup

;; Copyright (C) 2015-2017 Christian Lück

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

;; This file contains UI parts related to the buffer displaying
;; relations between markup elements.

;;; Code

(require 'standoff-api)
(require 'standoff-xml)
(require 'standoff-mark)
(require 'standoff-log)

(defcustom standoff-relations--markup-string-range-delimiter " … "
  "A string that delimits the content of two markup ranges."
  :group 'standoff
  :type 'string)

(defcustom standoff-relations-fields
  '((:subj-number . 3)
    (:subj-string . 25)
    (:predicate-string . 20)
    (:obj-number . 3)
    (:obj-string . 25))
  "A list of fields to be displayed in relation lists."
  :group 'standoff)

(defvar standoff-relations-tags-invisible nil
  "Whether or not tags are invisible.
Its value is to be set by the function which initializes the
buffer where relations are displayed and should be set to the
same value as `standoff-xml-tags-invisible' in the source buffer.")

;;;; helpers for formatting lines in the *relations* buffers

(defun standoff-relations-remove-newlines (str)
  "Replace newlines and carriage returns in STR with space character." 
  (let ((idx (string-match "\\s-" str)))
    (if idx
	(concat (substring str 0 idx)
		" "
		(standoff-relations-remove-newlines (substring str (match-end 0))))
      str)))

(defun standoff-relations-tags-invisible (str)
  "Make string STR readable by hiding tags and substituting char refs."
  (let ((tag-start (string-match standoff-xml-tag-name-re str))
	(tag-end))
    (if tag-start
	(progn
	  (setq tag-end (match-end 0))
	  (when (string-match ">" str)
	    (setq tag-end  (match-end 0)))
	  (concat (substring str 0 tag-start)
		  (standoff-relations-tags-invisible (substring str tag-end))))
      str)))

(defvar standoff-relations-glyph-display nil
  "Whether to display character references or to substitute them with glyphs.")

(defun standoff-relations-glyph-display (str)
  (let ((idx (string-match standoff-xml-char-ref-re str))
	(ref))
    (message "idx: %s str: %s" idx str)
    (if idx
	(progn
	(message "char-ref: %s" (match-string 0 str))
	(concat (substring str 0 idx)
		(cond ((match-string 1 str)
		       (char-to-string (string-to-number (match-string 1 str) 16)))
		      ((match-string 2 str)
		       (char-to-string (string-to-number (match-string 2 str))))
		      (t (match-string 0 str)))
		(standoff-relations-glyph-display (substring str (match-end 0)))))
      str)))

(defun standoff-relations--markup-string (source-buffer ranges)
  "Formats a string from markup in SOURCE-BUFFER given by RANGES."
  (let (r
	markup-string)
    (with-current-buffer source-buffer
      (while ranges
	(setq r (pop ranges))
	(when markup-string
	  (setq markup-string (concat markup-string
				      standoff-relations--markup-string-range-delimiter)))
	(setq markup-string (concat markup-string
				    (buffer-substring (nth standoff-pos-startchar r)
						      (nth standoff-pos-endchar r))))))
    (when standoff-relations-tags-invisible
      (setq markup-string (standoff-relations-tags-invisible markup-string)))
    (when standoff-relations-glyph-display
      (setq markup-string (standoff-relations-glyph-display markup-string)))
    (standoff-relations-remove-newlines markup-string)))

(defun standoff-relations--markup-type-label (typ)
  "Return the label for the markup type TYP."
  ;; silince the compiler with a boundp test
  (if (and (boundp 'standoff-show-labels) (boundp 'standoff-markup-labels))
      (if standoff-show-labels
	  (or (cdr (assoc typ standoff-markup-labels)) typ)
	typ)
    typ))

(defun standoff-relations--predicate-label (predicate)
  "Return the label for the predicate PREDICATE."
  ;; silince the compiler with a boundp test
  (if (and (boundp 'standoff-show-labels) (boundp 'standoff-predicate-labels))
      (if standoff-show-labels
	  (or (cdr (assoc predicate standoff-predicate-labels)) predicate)
	predicate)
    predicate))

(declare-function standoff-markup-get-number "standoff-mode.el" (buf markup-inst-id))
(declare-function standoff-markup-get-by-number "standoff-mode.el" (buf number))

(defun standoff-relations--relation-handler (rel-id subj-id p-id obj-id source-buf &optional invariant point)
  "Create a one line description of a relation for the relations list in the current buffer."
  (let* ((subjs (funcall standoff-markup-read-function source-buf nil nil nil subj-id))
	 (objs (funcall standoff-markup-read-function source-buf nil nil nil obj-id))
	 (line (format " %s " rel-id)))
    (put-text-property 1 37 'invisible t line) ; invisible relation id
    (when (and subjs objs) 
    (dolist (f-w standoff-relations-fields)
      (let ((field (car f-w))
	    (width (cdr f-w))
	    (str))
	;;(message "Field: %s, type: %s" field (type-of field))
	(setq str
	      (cond
	       ((eq field :subj-number)
		(number-to-string (standoff-markup-get-number source-buf subj-id)))
	       ((eq field :obj-number)
		(number-to-string (standoff-markup-get-number source-buf obj-id)))
	       ((eq field :subj-string)
		(standoff-relations--markup-string source-buf subjs))
	       ((eq field :obj-string)
		(standoff-relations--markup-string source-buf objs))
	       ((eq field :subj-type)
		(standoff-relations--markup-type-label (nth standoff-pos-markup-type (car subjs))))
	       ((eq field :obj-type)
		(standoff-relations--markup-type-label (nth standoff-pos-markup-type (car objs))))
	       ((eq field :predicate-string)
		(standoff-relations--predicate-label p-id))))
	(when str
	  (setq line (concat line
			     (if (not width)
				 str
			       (truncate-string-to-width str width 0 ?\s "…")) " ")))
	;; propertize it
	;; TODO: makes sense only if we can mark a relation for e.g. deletion
	)))
    (when point
      (goto-char point))
    (insert (concat line "\n"))
    ))

(defun standoff-relations-get-relation-id-of-line ()
  "Return the id of the relation in the current line."
  (let ((chars 0)
	start
	end)
    (beginning-of-line)
    (forward-char 1)
    (setq start (point))
    ;; FIXME: better use regexp
    (while (and (not (eolp)) (< chars 37))
      (forward-char 1)
      (setq chars (1+ chars)))
    (backward-char 1)
    ;;(message "uuid: %s chars: %s" (buffer-substring start (point)) chars)
    (if (= chars 37)
	(buffer-substring start (point))
      (error "No relation id found in this line"))))

(defvar standoff-relations--relations-buffer "*Relations*"
  "The name of the buffer in which relations are displayed.")

(defun standoff-relations-for-markup (markup-number)
  "Show the relations for MARKUP-NUMBER in a special buffer."
  (interactive "NNumber of markup element: ")
  (standoff-util/set-source-buffer (current-buffer))
  (let* ((markup-inst-id (standoff-markup-get-by-number (current-buffer) markup-number))
	 (relations)
	 (rel)
	 (tags-invisible standoff-xml-tags-invisible)
	 (glyph-display standoff-xml-char-ref-glyph-display)
	 (source-buffer (current-buffer))
	 (rel-buffer (set-buffer (get-buffer-create standoff-relations--relations-buffer)))
	 (buffer-read-only nil))
    ;; set tags-invisible etc. to the same value like in source buffer
    (setq-local standoff-relations-tags-invisible tags-invisible)
    (setq-local standoff-relations-glyph-display glyph-display)
    (erase-buffer)
    (setq relations (funcall standoff-relations-read-function source-buffer markup-inst-id nil nil))
    (while relations
      (setq rel (pop relations))
      (standoff-relations--relation-handler
       (nth standoff-pos-relation-id rel)
       (nth standoff-pos-subject rel)
       (nth standoff-pos-predicate rel)
       (nth standoff-pos-object rel)
       source-buffer
       'subject))
    (setq relations (funcall standoff-relations-read-function source-buffer nil nil markup-inst-id))
    (while relations
      (setq rel (pop relations))
      (standoff-relations--relation-handler
       (nth standoff-pos-relation-id rel)
       (nth standoff-pos-subject rel)
       (nth standoff-pos-predicate rel)
       (nth standoff-pos-object rel)
       source-buffer
       'object))
    (standoff-relations-mode)
    (goto-char (point-min))
    ;;(when glyph-display (standoff-xml-toggle-char-ref-glyph-substitute 1))
    ;;(when tags-invisible (standoff-xml-tags-invisible 1))
    (switch-to-buffer rel-buffer)))

(defun standoff-relations-do-delete (&optional arg)
  "Delete all marked (or next ARG) relations."
  (interactive "P")
  (standoff-mark/internal-do-deletions
   ;; this may move point if ARG is an integer
   (standoff-mark/map-over-marks
    #'(lambda () (cons (standoff-relations-get-relation-id-of-line) (point)))
    arg)
   arg
   #'(lambda (rel-id) (funcall standoff-relations-delete-function
			       (standoff-util/get-source-buffer)
			       nil nil nil rel-id))
   nil 					; delete, do not trash
   ))

(defun standoff-relations-do-flagged-delete (&optional nomessage)
  "In the *Relations* buffer, delete the relations flagged for deletion.
If NOMESSAGE is non-nil, we don't display any message
if there are no flagged relations."
  (interactive)
  (let* ((standoff-mark/marker-char standoff-mark/del-marker)
	 (regexp (standoff-mark/marker-regexp))
	 case-fold-search)
    (if (save-excursion (goto-char (point-min))
			(re-search-forward regexp nil t))
	(standoff-mark/internal-do-deletions
	 ;; this can't move point since ARG is nil
	 (standoff-mark/map-over-marks
	  #'(lambda () (cons (standoff-relations-get-relation-id-of-line) (point)))
	  nil)
	 nil
	 #'(lambda (rel-id) (funcall standoff-relations-delete-function
				     (standoff-util/get-source-buffer)
				     nil nil nil rel-id))
	 nil)				; delete, do not trash
      (or nomessage
	  (message "(No deletions requested)")))))

;;;; Literals/Attributes

(defun standoff-relations--literal-key-label (key)
  "Return the label for the attribut's key KEY."
  ;; silince the compiler with a boundp test
  (if (and (boundp 'standoff-show-labels) (boundp 'standoff-literal-key-labels))
      (if standoff-show-labels
	  (or (cdr (assoc key standoff-literal-key-labels)) key)
	key)
    key))

(defun standoff-relations--literal-handler (attr ranges source-buf &optional invariant point)
  "Create a one line description of an literal resp. attribute.
The Description is for the relations/attributes list in the
current buffer."
  (let* ((line ""))
    (dolist (f-w standoff-relations-fields)
      (let ((field (car f-w))
	    (width (cdr f-w))
	    (str))
	;;(message "Field: %s, type: %s" field (type-of field))
	(setq str
	      (cond
	       ((eq field :subj-number)
		(number-to-string (standoff-markup-get-number source-buf (nth standoff-pos-markup-inst-id (car ranges)))))
	       ((eq field :obj-number)
		(cond ((equal (nth standoff-pos-literal-type attr) 'string) "L")
		      (t "?")))
	       ((eq field :subj-string)
		(standoff-relations--markup-string source-buf ranges))
	       ((eq field :obj-string)
		(nth standoff-pos-literal-value attr))
	       ((eq field :subj-type)
		(standoff-relations--markup-type-label (nth standoff-pos-markup-type (car ranges))))
	       ((eq field :obj-type)
		"(Attribute)")
	       ((eq field :predicate-string)
		(standoff-relations--literal-key-label (nth standoff-pos-literal-key attr)))))
	(when str
	  (setq line (concat line
			     (if (not width)
				 str
			       (truncate-string-to-width str width 0 ?\s "…")) " ")))
	;; propertize it
	;; TODO: makes sense only if we can mark a relation for e.g. deletion
	))
    (when point
      (goto-char point))
    (insert (concat line "\n"))
    ))

(defun standoff-literals-for-markup (markup-number)
  (interactive "NNumber of markup element: ")
  (let* ((markup-inst-id (standoff-markup-get-by-number (current-buffer) markup-number))
	 (ranges (funcall standoff-markup-read-function (current-buffer) nil nil nil markup-inst-id))
	 (literals)
	 (lit)
	 (tags-invisible standoff-xml-tags-invisible)
	 (glyph-display standoff-xml-char-ref-glyph-display)
	 (source-buffer (current-buffer))
	 (rel-buffer (set-buffer (get-buffer-create standoff-relations--relations-buffer)))
	 (buffer-read-only nil))
    ;; set tags-invisible etc. to the same value like in source buffer
    (setq-local standoff-relations-tags-invisible tags-invisible)
    (setq-local standoff-relations-glyph-display glyph-display)
    (erase-buffer)
    (setq literals (funcall standoff-literals-read-function source-buffer markup-inst-id))
    (while literals
      (setq lit (pop literals))
      (standoff-relations--literal-handler
       lit
       ranges
       source-buffer
       'subject))
    (standoff-relations-mode)
    (goto-char (point-min))
    ;;(when glyph-display (standoff-xml-toggle-char-ref-glyph-substitute 1))
    ;;(when tags-invisible (standoff-xml-tags-invisible 1))
    (switch-to-buffer rel-buffer)))

;;;; Mode for the *Relations* Buffer

(defvar standoff-relations-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map (make-composed-keymap
			    special-mode-map
			    standoff-mark/mark-map))
    (define-key map "D" 'standoff-relations-do-delete)
    (define-key map "x" 'standoff-relations-do-flagged-delete)
    map))

(easy-menu-define standoff-relations-operate-menu standoff-relations-mode-map
  "Operations menu for *Relations* buffer."
  '("Operate"
    ["Delete" standoff-relations-do-delete]
    ["Delete flagged" standoff-relations-do-flagged-delete]
    ))

(define-derived-mode standoff-relations-mode special-mode "*Relations*"
  "A mode for managing relations in a special buffer of `standoff-mode'.

\\{standoff-relations-mode-map}
")

(provide 'standoff-relations)

;;; standoff-relations.el ends here
