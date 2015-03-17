
;; require Masahiro Hayashi's Emacs-esqlite 
;; from https://github.com/mhayashi1120/Emacs-esqlite
;; which again requires Masahiro's Emacs-pcsv from
;; https://github.com/mhayashi1120/Emacs-pcsv
(require 'esqlite)

(defcustom standoff-sqlite-database-path "~/markup.sqlite3"
  "The path to the sqlite database file."
  ;; During development you should use
  ;; $ fuser <path to sqlite database>
  ;; from the linux command line in order to see if connections are
  ;; left open.
  ;; TODO: Should we put in a place like
  ;; ~/.emacs.d/standoff/markup.sqlite3 ?
  :group 'standoff-sqlite
  :type 'file)

(defun standoff-sqlite-get-database-path ()
  "Return the path to the sqlite3 database as a path in a manner
that fits the conventions of the operating system."
  (convert-standard-filename standoff-sqlite-database-path))

;;
;; private
;;

(defun standoff-sqlite--assert-database (stream)
  "Assert that a database is present."
  ;; TODO: Get the path with something like 
  (let ((sql-file (concat (file-name-directory (symbol-file 'standoff-sqlite--assert-database)) 
			  "createDB-sqlite.sql")))
    (esqlite-stream-send-command stream ".read" sql-file)))

(defun standoff-sqlite--get-markupDefinitionID-by-name (stream markup-name &optional create)
  "Ask for the ID of the markup definition given by name."
  (let* ((sql-sel (format "SELECT markupDefinitionID FROM markupDefinition WHERE name='%s';" markup-name))
	(sql-ins (format "INSERT INTO markupDefinition (name, reading, uuid, version) VALUES ('%s', '%s', lower(hex(randomblob(16))), datetime('now'));" markup-name markup-name))
	(markupDef-id (esqlite-stream-read-atom stream sql-sel)))
    (if (and (not markupDef-id) create)
	(progn
	  (esqlite-stream-execute stream sql-ins)
	  (esqlite-stream-read-atom stream sql-sel))
      markupDef-id)))

(defun standoff-sqlite--get-documentID (stream buf &optional create)
  "Return the id (primary key) of the document visited in the
buffer given by BUF. If the document is not found in the
database (based on comparison of md5checksums) then ether nil is
returned or--if and only if the optional argument CREATE is
non-nil--the document visited in BUF is inserted into the
database. The argument STREAM is expected to be an esqlite stream
object."
  ;; TODO: save the buffer given in buf to the database, not only the
  ;; file path and the checksum. Find a fast way to do this. With
  ;; esqlite it's very slow--if not impossible.
  (let* ((checksum (md5 buf))
	 (sql-sel-doc-id (format "SELECT documentID FROM document WHERE md5checksum='%s';" checksum))
	 (doc-id (esqlite-stream-read-atom stream sql-sel-doc-id)))
    (if doc-id
	(string-to-number doc-id)
      (if (not create)
	  nil
	(let* (;; (buffer-contents-escaped 
	       ;; 	(esqlite-format-text (save-excursion
	       ;; 			       (set-buffer buf)
	       ;; 			       (save-restriction
	       ;; 				 (widen)
	       ;; 				 (buffer-string)))))
	       (sql-ins-doc (format "INSERT INTO document (uuid, md5checksum, localUri) VALUES (lower(hex(randomblob(16))), %s, %s);"
				    ;; buffer-contents-escaped
				    (esqlite-format-value checksum)
				    (esqlite-format-value (abbreviate-file-name (buffer-file-name buf))))))
	  (esqlite-stream-execute stream sql-ins-doc)
	  (string-to-number (esqlite-stream-read-atom stream sql-sel-doc-id)))))))

(defun standoff-sqlite--assert-markupInstance-markupDefinitionID (stream markupInstID markupDefID)
  "Assume IDs have string type."
  (esqlite-stream-read-atom stream (format 
				    "SELECT markupDefinitionID FROM markupInstance WHERE markupInstanceID='%s' AND markupDefinitionID='%s'"
				    markupInstID
				    markupDefID)))

(defun standoff-sqlite--markup-instance-has-markup-name (stream markup-id markup-name)
  "Returns the markupInstance id if and only if markupInstance
with id MARKUP-ID is of markup element MARKUP-NAME."
  (esqlite-stream-read-atom
   stream
   (format "SELECT markupInstance.markupInstanceID FROM markupInstance INNER JOIN markupDefinition WHERE markupInstance.markupDefinitionID=markupDefinition.markupDefinitionID AND markupDefinition.name=%s AND markupInstance.markupInstanceID=%s;"
	   (esqlite-format-value markup-name)
	   (esqlite-format-value markup-id))))

;;
;; public
;;

(defun standoff-sqlite-get-markupDefinition-names ()
  "Return the list of markup definitions."
  (let ((stream (esqlite-stream-open (standoff-sqlite-get-database-path))))
    (unwind-protect
	(progn
	  (standoff-sqlite--assert-database stream)
	  (esqlite-stream-read-list stream "SELECT name FROM markupDefinition;"))
      (esqlite-stream-close stream))))

(defun standoff-sqlite-write-range (buf startchar endchar markupInstance-name markupInstance-id)
  "Make a new markup range persistent in the backend."
  (let ((stream (esqlite-stream-open (standoff-sqlite-get-database-path)))
	(sql-sel-markupInstID "SELECT last_insert_rowid()"))
    (unwind-protect
	(progn
	  (esqlite-stream-execute stream "BEGIN")
	  (standoff-sqlite--assert-database stream)
	  (let* ((markupDefinition-id (standoff-sqlite--get-markupDefinitionID-by-name stream markupInstance-name t))
		 (document-id (standoff-sqlite--get-documentID stream buf t))
		 (sql-ins-markupInst (format "INSERT INTO markupInstance (documentID, uuid,  markupDefinitionID) VALUES ('%s', lower(hex(randomblob(16))), '%s');" document-id markupDefinition-id))
		 (markupInst-id (if (equal markupInstance-id "n")
				    (progn
				      (esqlite-stream-execute stream sql-ins-markupInst)
				      (esqlite-stream-read-atom stream sql-sel-markupInstID))
				  markupInstance-id))
		 (sql-ins-stringrange (format 
				       "INSERT INTO stringrange (documentID, markupInstanceID, uuid, startchar, endchar) VALUES (%s, %s, lower(hex(randomblob(16))), %s, %s);"
				       (esqlite-format-value document-id)
				       (esqlite-format-value markupInst-id)
				       (esqlite-format-value startchar)
				       (esqlite-format-value endchar))))
	    (if (standoff-sqlite--assert-markupInstance-markupDefinitionID stream markupInst-id markupDefinition-id)
		(progn
		  (message "Hi3")
		  (esqlite-stream-execute stream sql-ins-stringrange)
		  (esqlite-stream-execute stream "COMMIT")
		  ;; return the id of markupInstance as integer
		  (cond ((numberp markupInst-id) markupInst-id)
			(t (string-to-number markupInst-id))))
	      (esqlite-stream-execute stream "ROLLBACK")
	      (error "Markup element %s is not of type %s" markupInst-id markupInstance-name))))
      (esqlite-stream-close stream))))

(defun standoff-sqlite-read-ranges (buf &optional startchar endchar markup-name markup-id)
  "Get ranges of markup elements from the sqlite database and
  return the as a list."
  (let ((stream (esqlite-stream-open (standoff-sqlite-get-database-path)))
	(doc-id)
	(sql-sel-markupDef)
	(markupDef-id)
	(where-list nil)
	(where)
	(sql-sel-ranges))
    (unwind-protect
	(progn
	  (setq doc-id (standoff-sqlite--get-documentID stream buf t))
	  (setq where (concat where (format " AND stringrange.documentID=%s"
					    (esqlite-format-value doc-id))))
	  (when (and startchar endchar)
	    (setq where (concat where (format " AND stringrange.endchar>=%s AND stringrange.startchar<=%s"
					      (esqlite-format-value startchar)
					      (esqlite-format-value endchar)))))
	  (when markup-id
	    (setq where (concat where (format " AND stringrange.markupInstanceID=%s"
					      (esqlite-format-value markup-id)))))
	  (when markup-name
	    (setq where (concat where (format " AND markupDefinition.name=%s"
					      (esqlite-format-value markup-name)))))
	  (setq sql-sel-ranges (format "SELECT stringrange.startchar, stringrange.endchar, markupDefinition.name, markupInstance.markupInstanceID FROM stringrange INNER JOIN markupInstance INNER JOIN markupDefinition WHERE stringrange.markupInstanceID=markupInstance.markupInstanceID and markupInstance.markupDefinitionID=markupDefinition.markupDefinitionID %s;" where))
	  ;;(message sql-sel-ranges)
	  (esqlite-stream-read stream sql-sel-ranges))
      (esqlite-stream-close stream))))

(defun standoff-sqlite-delete-range (buf startchar endchar markup-name markup-id)
  "Delete a string range from the sqlite database."
  (let ((stream (esqlite-stream-open (standoff-sqlite-get-database-path)))
	(doc-id)
	(sql-del-range (format "DELETE FROM stringrange WHERE startchar=%s AND endchar=%s AND markupInstanceID=%s;"
			       (esqlite-format-value startchar)
			       (esqlite-format-value endchar)
			       (esqlite-format-value markup-id)))
	(sql-sel-other-ranges (format "SELECT stringrangeID FROM stringrange WHERE markupInstanceID=%s;"
				      (esqlite-format-value markup-id)))
	(sql-del-markup-inst (format "DELETE FROM markupInstance WHERE markupInstanceID=%s;"
				     (esqlite-format-value markup-id))))
    (unwind-protect
	(progn
	  (setq doc-id (standoff-sqlite--get-documentID stream buf nil))
	  (if (not (standoff-sqlite--markup-instance-has-markup-name stream markup-id markup-name))
	      (error "Markup element %s is not of type %s" markup-id markup-name)
	    (esqlite-stream-execute stream "BEGIN;")
	    (esqlite-stream-execute stream sql-del-range)
	    (when (not (esqlite-stream-read stream sql-sel-other-ranges))
	      (esqlite-stream-execute stream sql-del-markup-inst))
	    (esqlite-stream-execute stream "COMMIT;")
	    t))
      (esqlite-stream-close stream))))


;;
;; Setup
;;

(defun standoff-sqlite-register-backend ()
  "Register sqlite as backend, i.e. setting the
standoff-markup-*-function variables point to sqlite backup functions."
  (setq standoff-markup-write-range-function 'standoff-sqlite-write-range
	standoff-markup-read-ranges-function 'standoff-sqlite-read-ranges
	standoff-markup-delete-range-function 'standoff-sqlite-delete-range
	standoff-markup-names-function 'standoff-sqlite-get-markupDefinition-names))


;;
;; Testing interactively while developping
;;

(defun standoff-sqlite-test ()
  (interactive)
  (message "%s" (standoff-sqlite-get-markupDefinition-names)))

(defun standoff-sqlite-test-with-stream ()
  (interactive)
  (let ((stream (esqlite-stream-open (standoff-sqlite-get-database-path))))
    (unwind-protect
	(progn
	  (standoff-sqlite--assert-database stream)
	  (let* ((test "tulpe"))
	    (message "%s"
		     (standoff-sqlite--get-markupDefinitionID-by-name stream "blub" t)
		     )))
      (esqlite-stream-close stream))))


(provide 'standoff-sqlite)
