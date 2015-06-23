;;; standoff-oa.el --- serialize to rdf following the open annotation ontology.

;; Copyright (C) 2015 Christian Lück

;; Author: Christian Lück <christian.lueck@ruhr-uni-bochum.de>
;; Homepage: https://github.com/lueck/standoff-mode

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

;; This file contains stuff for exporting to OpenAnnotation.

;;; Code

(require 'standoff-mode)

(defcustom standoff-oa-markup-iri-namespace nil
  "Namespace part of a markup iri."
  :group 'standoff-oa
  :type 'string)

(defcustom standoff-oa-unknown-user "unknown"
  "String that is shown if a user unknown for dc:creator."
  :group 'standoff-oa
  :type 'string)

(defconst standoff-oa-datetime-format "%Y-%m-%dT%T%Z"
  "Format string for dc:date in xsd:dateTime.")

(defun standoff-oa-source-uri (source-buf &optional relative)
  "Returns a uri for the source document in buffer SOURCE-BUF.
If RELATIVE is non-nil, only the name of the file visited in
source buffer is returned."
  (if relative
      (file-relative-name (buffer-file-name source-buf))
    (buffer-file-name source-buf)))

(defun standoff-oa-markup-iri (id source-uri)
  "Make an iri for the markup element given by ID.
This concats `standoff-oa-markup-iri-namespace' and id, if and
only if 'standoff-oa-markup-iri-namespace' is a string. Otherwise
SOURCE-URI and ID are concatenated."
  (if (stringp standoff-oa-markup-iri-namespace)
      (concat standoff-oa-markup-iri-namespace id)
    (concat source-uri "#" id)))

;;;; Turtle

(defconst standoff-oa-markup-format
  "%s a oa:Annotation ;
  oa:hasBody %s ;
  dc:creator \"%s\" ;
  dc:date \"%s\"^^xsd:dateTime ;
  oa:hasTarget [
    a oa:SpecificResource ;
    oa:hasSource %s %s
  ] .
"
"A turtle representation of a markup element takes the following
format.")

(defconst standoff-oa-turtle-selector-format
  ";
    oa:hasSelector [
      a oa:TextPositionSelector ;
      oa:start %i ;
      oa:end %i ;
      oa:exact %s
      ] "
"Format string for a oa:TextPositionSelector.")

(defun standoff-oa-turtle-outfile-default (buf)
  "Default turtle output file name for buffer BUF."
  (concat (buffer-file-name buf) ".ttl"))

(defun standoff-oa-turtle-serialize-selectors (ranges)
  "Returns a string of selectors concatenated for RANGES."
  (let ((selectors ""))
    (while ranges
      (setq range (pop ranges)
	    selectors (concat selectors
			      (format standoff-oa-turtle-selector-format
				      (nth standoff-pos-startchar range)
				      (nth standoff-pos-endchar range)
				      (prin1-to-string (nth standoff-pos-markup-string range))))))
    selectors))

(defun standoff-oa-turtle-serialize-markup (source-buf out-buf &optional relative)
  "Append markup elements to output buffer BUF.
If RELATIVE is non-nil, only the name of the file visited in
source buffer is returned."
  (let ((data (funcall standoff-markup-read-function source-buf))
	(ranges)
	(other-elements)
	(source-uri (standoff-oa-source-uri source-buf relative)))
    (with-current-buffer out-buf
      (while data
	(setq ranges (list (pop data))
	      id (nth standoff-pos-markup-inst-id (car ranges))
	      other-elements '())
	;; get other ranges with same markup-inst-id
	(dolist (el data)
	  (if (equal (nth standoff-pos-markup-inst-id el) id)
	      (push el ranges)
	    (push el other-elements)))
	(setq data other-elements)
	;; serialize
	(insert (format standoff-oa-markup-format
			(standoff-oa-markup-iri (nth standoff-pos-markup-inst-id (car ranges)) source-uri)
			(nth standoff-pos-markup-type (car ranges))
			(or (nth standoff-pos-markup-user (car ranges))
			    standoff-oa-unknown-user)
			(format-time-string standoff-oa-datetime-format
					    (or (nth standoff-pos-markup-datetime (car ranges))
						(current-time)))
			source-uri
			(standoff-oa-turtle-serialize-selectors ranges)))))))

(defun standoff-oa-turtle-serialize (source-buf out-file)
  "Serialize the markup in buffer SOURCE-BUF and write to OUT-FILE."
  ;;TODO
  (interactive
   (list (current-buffer)
	 (read-file-name "Output file: "
			 nil
			 nil
			 'confirm
			 (file-relative-name (standoff-oa-turtle-outfile-default (current-buffer))))))
  (let ((out-buf (find-file-noselect out-file)))
    (with-current-buffer out-buf
      (erase-buffer)
      (goto-char (point-min)))    
    (standoff-oa-turtle-serialize-markup source-buf out-buf t)
    (with-current-buffer out-buf
      (save-buffer))
    (kill-buffer out-buf)))


;;; standoff-oa.el ends here.
