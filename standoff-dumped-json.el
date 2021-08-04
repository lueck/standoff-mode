;;; standoff-dumped-json.el --- load dumped annotations and writem them to json.

;; Copyright (C) 2021 Christian Lück

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

;; Functions and an interactive command for converting annotation data
;; dumped with the dummy backend to JSON.  It requires, that
;; standoff-json-file is configured as backend.  (Do not use the dummy
;; backend!)  Use M-x standoff-dumped-json/dumped-to-json RET to
;; convert.  You will be prompted for a dump file.  The command might
;; take a while to finish.

;; To use this, make it load after standoff-mode is loaded:

;; (eval-after-load 'standoff-mode (require 'standoff-dumped-json))


;;; Code:

(require 'standoff-util)
(require 'standoff-api)
(require 'standoff-json-file)

(declare-function standoff-dump-filename-default "standoff-mode.el" nil)

;; These variables are set when evaluating the dump file. In order to
;; silence the compiler we just define, but do not initialize them.
(defvar standoff-api-description-dumped)
(defvar standoff-source-md5-dumped)
(defvar standoff-markup-read-function-dumped)
(defvar standoff-relations-read-function-dumped)
(defvar standoff-literals-read-function-dumped)


(defun standoff-dumped-json/range-to-json (markup-inst-id typ start end markup-string &optional timestamp creator)
  "Serialize a markup range to json.
The range is given by MARKUP-INST-ID, TYP, START and END offset,
MARKUP-STRING, TIMESTAMP and CREATOR.  The UUID of the range is
newly created."
  (let
      ((range-id (standoff-util/create-uuid))
       (timestampp (or timestamp '(0 0 0 0)))
       (creatorr (or creator "unknown")))
    (concat
     "{\"tag\": \"MarkupRange\""
     ", \"markupElementId\": \"" markup-inst-id "\""
     ", \"markupRangeId\": \"" range-id "\""
     ", \"qualifiedName\": \"" typ "\""
     ", \"sourceStart\": \"" (number-to-string start) "\""
     ", \"sourceEnd\": \"" (number-to-string end) "\""
     ", \"createdAt\": \"" (format-time-string "%FT%TZ" timestampp) "\""
     ", \"createdBy\": \"" creatorr "\""
     "}")))

(defun standoff-dumped-json/relation-to-json (relation-id subject-id predicate object-id &optional timestamp creator)
  "Serialize a relation to json."
  (let
      ((timestampp (or timestamp '(0 0 0 0)))
       (creatorr (or creator "unknown")))
    (concat
     "{\"tag\": \"Relation\""
     ", \"relationId\": \"" relation-id "\""
     ", \"subjectId\": \"" subject-id "\""
     ", \"predicate\": \"" predicate "\""
     ", \"objectId\": \"" object-id "\""
     ", \"createdAt\": \"" (format-time-string "%FT%TZ" timestampp) "\""
     ", \"createdBy\": \"" creatorr "\""
     "}")))

(defun standoff-dumped-json/literal-to-json (literal-id subject-id key value typ other-typ &optional timestamp creator)
  "Serialize a literal to json."
  (let
      ((timestampp (or timestamp '(0 0 0 0)))
       (creatorr (or creator "unknown")))
    (concat
     "{\"tag\": \"Literal\""
     ", \"literalId\": \"" literal-id "\""
     ", \"subjectId\": \"" subject-id "\""
     ", \"key\": \"" key "\""
     ", \"value\": \"" value "\""
     ", \"createdAt\": \"" (format-time-string "%FT%TZ" timestampp) "\""
     ", \"createdBy\": \"" creatorr "\""
     "}")))

(defun standoff-dumped-json/dumped-to-json (fname)
  "Load dumped annotations and serialize them to json.
The file name FNAME of the dump is asked for interactively or it
can be passed in as an argument.  The file name of the json file
is as usually in the json-file backend."
  (interactive
   (list (read-file-name "File to be loaded: "
			 nil
			 nil
			 'confirm
			 (file-relative-name (standoff-dump-filename-default)))))
  (let
      ((srcbuf (current-buffer))
       (jsonbuf (standoff-json-file/get-json-buffer (current-buffer)))
       (dumped-api))
    (require 'standoff-mode)
    ;; initialize dumped vars, because old values may be left over
    ;; after loading, if dump-file is incomplete
    ;; TODO: use standoff-dump-vars
    (setq standoff-api-description-dumped nil
	  standoff-source-md5-dumped nil
	  standoff-markup-read-function-dumped nil
	  standoff-relations-read-function-dumped nil
	  standoff-literals-read-function-dumped nil)
    (load fname)
    ;; checksum should not differ
    (unless (equal (md5 srcbuf) standoff-source-md5-dumped)
      (error "Error loading dump file: Did you edit the file? Checksum does not match.  Not loading"))
    (let*
	(;; get dumped api
	 (dumped-api (or standoff-api-description-dumped
			 (cdr (assoc "first" standoff-api-generations))))
	 (ranges (standoff-api-evolve srcbuf :markup standoff-markup-read-function-dumped dumped-api))
	 (relations (standoff-api-evolve srcbuf :relations standoff-relations-read-function-dumped dumped-api))
	 (literals (standoff-api-evolve srcbuf :literals standoff-literals-read-function-dumped dumped-api)))
      (message "File %s successfully loaded." fname)
      (message "Annotations loaded: Markup: %i; Relations: %i; Literals: %i. It may take some time to convert..." (length ranges) (length relations) (length literals))
      (with-current-buffer srcbuf
	(save-excursion
	  (mapc #'(lambda (obj) (standoff-json-file/create-object srcbuf "MarkupRanges" (apply 'standoff-dumped-json/range-to-json obj))) ranges)
	  (mapc #'(lambda (obj) (standoff-json-file/create-object srcbuf "Relations" (apply 'standoff-dumped-json/relation-to-json obj))) relations)
	  (mapc #'(lambda (obj) (standoff-json-file/create-object srcbuf "Literals" (apply 'standoff-dumped-json/literal-to-json obj))) literals)
	  (message "... done converting to json.")
	  )))))


(provide 'standoff-dumped-json)

;;; standoff-dumped-json.el ends here
