;;; standoff-relations.el --- part of standoff-mode, a mode for creating stand-off markup

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

;; This file contains things related to the buffer displaying
;; relations between markup elements.

;;; Code

(require 'standoff-api)

(defcustom standoff-relations--markup-string-range-delimiter " … "
  "A string that delimits the content of two markup ranges."
  :group 'standoff
  :type 'string)

(defcustom standoff-relations-fields
  '((:subj-number . 3)
    (:subj-string . 20)
    (:predicate-string . 20)
    (:obj-number . 3)
    (:obj-string . 20))
  "A list of fields to be displayed in relation lists."
  :group 'standoff)

(defun standoff-relations--markup-string (ranges)
  "Formats a string from markup given by RANGES."
  (let ((markup-string (nth standoff-pos-markup-string (pop ranges))))
    (while ranges
      (setq markup-string (concat markup-string standoff-relations--markup-string-range-delimiter (nth standoff-pos-markup-string (pop ranges)))))
    markup-string))

(defun standoff-relations--markup-type-label (typ)
  "Return the label for the markup type TYP."
  (if standoff-show-labels
      (or (cdr (assoc predicate standoff-markup-labels)) typ)
    typ))

(defun standoff-relations--predicate-label (predicate)
  "Return the label for the predicate PREDICATE."
  (if standoff-show-labels
      (or (cdr (assoc predicate standoff-predicate-labels)) predicate)
    predicate))

(defun standoff-relations--relation-handler (rel-id subj-id p-id obj-id source-buf &optional invariant point)
  "Create a one line description of a relation for the relations list in the current buffer."
  (let* ((subjs (funcall standoff-markup-read-function source-buf nil nil nil subj-id))
	 (objs (funcall standoff-markup-read-function source-buf nil nil nil obj-id))
	 (line ""))
    (dolist (f-w standoff-relations-fields)
      (let ((field (car f-w))
	    (width (cdr f-w))
	    (str))
	(message "Field: %s, type: %s" field (type-of field))
	(setq str
	      (cond
	       ((eq field :subj-number)
		(number-to-string (standoff-markup-get-number source-buf subj-id)))
	       ((eq field :obj-number)
		(number-to-string (standoff-markup-get-number source-buf obj-id)))
	       ((eq field :subj-string)
		(standoff-relations--markup-string subjs))
	       ((eq field :obj-string)
		(standoff-relations--markup-string objs))
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
	))
    (when point
      (goto-char point))
    (insert (concat line "\n"))
    ))

(defvar standoff-relations--relations-buffer "*Relations*"
  "The name of the buffer in which relations are displayed.")

(defun standoff-relations-for-markup (markup-number)
  (interactive "NNumber of markup element: ")
  (let* ((markup-inst-id (standoff-markup-get-by-number (current-buffer) markup-number))
	 (relations)
	 (rel)
	 (source-buffer (current-buffer))
	 (rel-buffer (set-buffer (get-buffer-create standoff-relations--relations-buffer)))
	 (buffer-read-only nil))
    (setq relations (funcall standoff-relations-read-function source-buffer markup-inst-id nil nil))
    (while relations
      (setq rel (pop relations))
      (standoff-relations--relation-handler
       nil
       (nth standoff-pos-subject rel)
       (nth standoff-pos-predicate rel)
       (nth standoff-pos-object rel)
       source-buffer
       'subject))
    (setq relations (funcall standoff-relations-read-function source-buffer nil nil markup-inst-id))
    (while relations
      (setq rel (pop relations))
      (standoff-relations--relation-handler
       nil
       (nth standoff-pos-subject rel)
       (nth standoff-pos-predicate rel)
       (nth standoff-pos-object rel)
       source-buffer
       'object))
    (standoff-relations-mode)
    (goto-char (point-min))
    (switch-to-buffer rel-buffer)))

(define-derived-mode standoff-relations-mode special-mode "*Relations*"
  "A mode for managing relations of an markup element in a special buffer in `standoff-mode'.

\\{standoff-relations-map}
")

(provide 'standoff-relations)

;;; standoff-relations.el ends here.
