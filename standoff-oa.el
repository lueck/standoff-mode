;;; standoff-oa.el --- serialize to rdf following the open annotation ontology.


;;; Code

;;;; Turtle

(defconst standoff-oa-markup-format
  "%s a oa:Annotation ;
  oa:hasBody %s ;
  dc:creator %s ;
  dc:date \"%s\"^^xsd:date ;
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
format:

%markup-inst-id a oa:Annotation ;
     		oa:hasBody %markup-type ;
		dc:creator %user ;
		dc:date \"%date\"^^xsd:date ;
     		oa:hasTarget [
     		 	      a oa:SpecificResource ;
     		  	      oa:hasSource %file-name ;
		  	      oa:hasSelector [
		  		 	      a oa:TextPositionSelector ;
				 	      oa:start %start ;
     		  		 	      oa:end %end ;
				 	      oa:exact %string
				 	      ]
		  	      ] .
"
)

(declare-function standoff-source-uri "standoff-mode.el" (buf))

(defun standoff-oa-turtle-serialize-markup (source-buf out-buf)
  "Append markup elements to output buffer BUF."
  (let ((data (funcall standoff-markup-read-function source-buf))
	element)
    (with-current-buffer out-buf
      (while data
	(setq element (pop data))
	(print (format standoff-oa-markup-format
		       (nth standoff-pos-markup-inst-id element)
		       (nth standoff-pos-markup-type element)
		       (nth standoff-pos-markup-user element)
		       (nth standoff-pos-markup-datetime element)
		       (standoff-source-uri source-buf)
		       (nth standoff-pos-startchar element)
		       (nth standoff-pos-endchar element)
		       (standoff-oa-escape-markup-string (nth standoff-pos-markup-string)))
	       out-buf)))))


;;; standoff-oa.el ends here.
