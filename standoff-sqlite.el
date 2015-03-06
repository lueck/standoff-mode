;; INSERT INTO markupdefinition (uuid, name, version) VALUES ("1", "beispiel", "1"); 

;; require Masahiro Hayashi's Emacs-esqlite 
;; from https://github.com/mhayashi1120/Emacs-esqlite
;; which again requires Masahiro's Emacs-pcsv from
;; https://github.com/mhayashi1120/Emacs-pcsv
(require 'esqlite)

(defcustom standoff-sqlite-database-path "~/markup.sqlite3"
  "The path to the sqlite database file."
  ;; TODO: should be better default value
  :group 'standoff-sqlite
  :type 'file)

(defun standoff-sqlite--assert-database (stream)
  "Assert that a database is present."
  ;; TODO: Get the path with something like 
  ;; (symbol-file 'standoff-sqlite-assert-database)
  (esqlite-stream-send-command stream ".read" "createDB-sqlite.sql"))

(defun standoff-sqlite-get-markupdefinition-names ()
  "Return the list of markup definitions."
  (let ((stream (esqlite-stream-open standoff-sqlite-database-path)))
    (unwind-protect
	(progn
	  (standoff-sqlite--assert-database stream)
	  (esqlite-stream-read-list stream "SELECT name FROM markupDefinition;"))
      (esqlite-stream-close stream))))

(defun standoff-sqlite-test ()
  (interactive)
  (message "%s" (standoff-sqlite-get-markupDefinition-names)))

(defun standoff-sqlite-test-with-stream ()
  (interactive)
  (let ((stream (esqlite-stream-open standoff-sqlite-database-path)))
    (unwind-protect
	(progn
	  (standoff-sqlite--assert-database stream)
	  (let* ((test "tulpe"))
	    (message "%s"
		     (standoff-sqlite--get-markupDefinitionID-by-name stream "blub" t)
		     )))
      (esqlite-stream-close stream))))


(defun standoff-sqlite--get-markupDefinitionID-by-name (stream markup-name &optional create)
  "Ask for the ID of the markup defintion given by name."
  (let* ((sql-sel (format "SELECT markupDefinitionID FROM markupDefinition WHERE name='%s';" markup-name))
	(sql-ins (format "INSERT INTO markupDefinition (name, reading, uuid, version) VALUES ('%s', '%s', lower(hex(randomblob(16))), datetime('now'));" markup-name markup-name))
	(markupDef-id (esqlite-stream-read-atom stream sql-sel)))
    (if (and (not markupDef-id) create)
	(progn
	  (esqlite-stream-execute stream sql-ins)
	  (esqlite-stream-read-atom stream sql-sel))
      markupDef-id)))

(defun standoff-sqlite--get-documentID (stream buf)
  1)

(defun standoff-sqlite--assert-markupInstance-markupDefinitionID (stream markupInstID markupDefID)
  "Assume IDs have string type."
  (esqlite-stream-read-atom stream (format 
				    "SELECT markupDefinitionID FROM markupInstance WHERE markupInstanceID='%s' AND markupDefinitionID='%s'"
				    markupInstID
				    markupDefID)))

(defun standoff-sqlite-markup-range (buf startchar endchar markupInstance-name markupInstance-id)
  "Make a new markup range persistent in the backend."
  (let ((stream (esqlite-stream-open standoff-sqlite-database-path))
	(sql-sel-markupInstID "SELECT last_insert_rowid()"))
    (unwind-protect
	(progn
	  (esqlite-stream-execute stream "BEGIN")
	  (standoff-sqlite--assert-database stream)
	  (let* ((markupDefinition-id (standoff-sqlite--get-markupDefinitionID-by-name stream markupInstance-name t))
		 (document-id (standoff-sqlite--get-documentID stream buf))
		 (sql-ins-markupInst (format "INSERT INTO markupInstance (documentID, uuid,  markupDefinitionID) VALUES ('%s', lower(hex(randomblob(16))), '%s');" document-id markupDefinition-id))
		 (markupInst-id (if (equal markupInstance-id "i")
				     (progn
				       (esqlite-stream-execute stream sql-ins-markupInst)
				       (esqlite-stream-read-atom stream sql-sel-markupInstID))
				   markupInstance-id))
		 (sql-ins-stringrange (format 
				       "INSERT INTO stringrange (documentID, markupInstanceID, uuid, startchar, endchar) VALUES ('%s', '%s', lower(hex(randomblob(16))), '%i', '%i');"
				       document-id
				       markupInst-id
				       startchar
				       endchar)))
	    (if (standoff-sqlite--assert-markupInstance-markupDefinitionID stream markupInst-id markupDefinition-id)
		(progn
		  (esqlite-stream-execute stream sql-ins-stringrange)
		  (esqlite-stream-execute stream "COMMIT")
		  ;; return the id of markupInstance as integer
		  (string-to-number markupInst-id))
	      (esqlite-stream-execute stream "ROLLBACK")
	      (error "Markup element %s is not of type %s" markupInst-id markupInstance-name))))
      (esqlite-stream-close stream))))

(setq standoff-markup-function 'standoff-sqlite-markup-range)
