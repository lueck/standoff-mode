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

(defun standoff-relations-for-markup (markup-number)
  (interactive "NNumber of markup element: ")
  (let* ((markup-inst-id (standoff-markup-get-by-number (current-buffer) markup-number))
	 (subj-relations)
	 (obj-relations)
	 (source-buffer (current-buffer))
	 (rel-buffer (get-buffer-create "*Relations*"))
	 (relations '())
	 (rel))
    (setq subj-relations (funcall standoff-relations-read-function (current-buffer) markup-inst-id nil nil))
    (message "%s" subj-relations)
    (while subj-relations
      (message "Hier")
      (setq rel (pop subj-relations))
      (message "Hier2")
      (setq subj (car (funcall standoff-markup-read-function (current-buffer) nil nil nil (nth standoff-pos-subject rel))))
      (message "%s" subj)
      (setq obj (car (funcall standoff-markup-read-function (current-buffer) nil nil nil (nth standoff-pos-object rel))))
      (push (list ;;(nth standoff-pos-markup-type subj)
		  (nth standoff-pos-markup-string subj)
		  (nth standoff-pos-predicate rel)
		  ;;(nth standoff-pos-markup-type obj)
		  (nth standoff-pos-markup-string obj))
	    relations))
    (setq obj-relations (funcall standoff-relations-read-function (current-buffer) nil nil markup-inst-id))
    (while obj-relations
      (setq rel (pop obj-relations))
      (setq subj (car (funcall standoff-markup-read-function (current-buffer) nil nil nil (nth standoff-pos-subject rel))))
      (setq obj (car (funcall standoff-markup-read-function (current-buffer) nil nil nil (nth standoff-pos-object rel))))
      (push (list ;;(nth standoff-pos-markup-type subj)
		  (nth standoff-pos-markup-string subj)
		  (nth standoff-pos-predicate rel)
		  ;;(nth standoff-pos-markup-type obj)
		  (nth standoff-pos-markup-string obj))
	    relations))
    (set-buffer rel-buffer)
    (standoff-relations-mode)
    (let ((buffer-read-only nil))
      (erase-buffer)
      (dolist (r relations)
	(print r rel-buffer)))
    (goto-char (point-min))
    (switch-to-buffer rel-buffer)))

(define-derived-mode standoff-relations-mode special-mode "*Relations*"
  "A mode for managing relations of an markup element in a special buffer in `standoff-mode'.

\\{standoff-relations-map}
")

;;; standoff-relations.el ends here.
