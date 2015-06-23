;;; standoff-oa.el --- serialize to rdf following the open annotation ontology.


;;; Code

(require 'standoff-mode)

;;;; Turtle

(defconst standoff-oa-markup-format
  "%s a oa:Annotation ;
  oa:hasBody %s ;
  dc:creator \"%s\" ;
  dc:date \"%s\"^^xsd:dateTime ;
  oa:hasTarget [
    a oa:SpecificResource ;
    oa:hasSource %s ;
    oa:hasSelector [
      a oa:TextPositionSelector ;
      oa:start %i ;
      oa:end %i ;
      oa:exact %s
      ]
  ] .
"
"A turtle representation of a markup element takes the following
format.")

(defun standoff-oa-source-uri (source-buf &optional relative)
  "Returns a uri for the source document in buffer SOURCE-BUF.
If RELATIVE is non-nil, only the name of the file visited in
source buffer is returned."
  (if relative
      (file-relative-name (buffer-file-name source-buf))
    (buffer-file-name source-buf)))

(defun standoff-oa-escape-markup-string (str)
  str)

(defcustom standoff-oa-markup-iri-namespace nil
  "Namespace part of a markup iri."
  :group 'standoff-oa
  :type 'string)

(defun standoff-oa-markup-iri (id source-uri)
  "Make an iri for the markup element given by ID.
This concats `standoff-oa-markup-iri-namespace' and id, if and
only if 'standoff-oa-markup-iri-namespace' is a string. Otherwise
SOURCE-URI and ID are concatenated."
  (if (stringp standoff-oa-markup-iri-namespace)
      (concat standoff-oa-markup-iri-namespace id)
    (concat source-uri "#" id)))

(defcustom standoff-oa-unknown-user "unknown"
  "String that is shown if a user unknown for dc:creator."
  :group 'standoff-oa
  :type 'string)

(defconst standoff-oa-datetime-format "%Y-%m-%dT%T%Z"
  "Format string for dc:date in xsd:dateTime.")

(defun standoff-oa-turtle-serialize-markup (source-buf out-buf &optional relative)
  "Append markup elements to output buffer BUF.
If RELATIVE is non-nil, only the name of the file visited in
source buffer is returned."
  (let ((data (funcall standoff-markup-read-function source-buf))
	(element)
	(source-uri (standoff-oa-source-uri source-buf relative)))
    (with-current-buffer out-buf
      (while data
	(setq element (pop data))
	(insert (format standoff-oa-markup-format
			(standoff-oa-markup-iri (nth standoff-pos-markup-inst-id element) source-uri)
			(nth standoff-pos-markup-type element)
			(or (nth standoff-pos-markup-user element)
			    standoff-oa-unknown-user)
			(format-time-string standoff-oa-datetime-format
					    (or (nth standoff-pos-markup-datetime element)
						(current-time)))
			source-uri
			(nth standoff-pos-startchar element)
			(nth standoff-pos-endchar element)
			(prin1-to-string (nth standoff-pos-markup-string element))))))))

(defun standoff-oa-turtle-outfile-default (buf)
  "Default turtle output file name for buffer BUF."
  (concat (buffer-file-name buf) ".ttl"))

(defun standoff-oa-turtle-serialize (source-buf out-file)
  "Serialize the markup in buffer SOURCE-BUF and write to OUT-FILE."
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
