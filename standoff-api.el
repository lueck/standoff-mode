;; standoff-api.el --- API for back-ends in standoff-mode

;; Copyright (C) 2015 Christian Lueck

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

;; See standoff-dummy.el for a reference implementation

;;
;; Structure of lists returned by a backend
;;

;; Structure of markup lists 

(defconst standoff-pos-markup-inst-id 0
  "Position of a markup instance's ID.
I.e. the position of the ID in a list representing a markup
element (or a range of a markup element in the case of
discontinous markup).

Example of a list representing a markup element: 

'(1 \"example\" 23 42 &rest)

This represents the markup element with the ID 1, its type is
\"example\" and it's ranging from character 23 to 42.")

(defconst standoff-pos-markup-type 1
  "Position of a markup element's type.
See `standoff-pos-markup-inst-id' for an example.")

(defconst standoff-pos-startchar 2
  "Position of a markup element's starting character's offset.
See `standoff-pos-markup-inst-id' for an example.")

(defconst standoff-pos-endchar 3
  "Position of a markup element's end character's offset.
See `standoff-pos-markup-inst-id' for an example.")

(defconst standoff-pos-markup-string 4
  "Position of the string that was annotated.
See `standoff-pos-markup-inst-id' for an example.")

;; Structure of relation lists

(defconst standoff-pos-subject 0
  "The position of a relation's subject.
I.e. the position in a list representing a relation between
markup elements.")

(defconst standoff-pos-predicate 1
  "The position of a relation's predicate.")

(defconst standoff-pos-object 2
  "The position of a relations object.")

;;
;; Functions to be implemented
;;

(defvar standoff-markup-create-function 'standoff-dummy-create-markup
  "The function that writes a markup element to some backend.
This variable must be set to the function's symbol (name). The
function must take the following arguments:

BUFFER STARTCHAR ENDCHAR MARKUP-NAME

The function is expected to return the ID of the markup
element. When storing to the backend was not successfull, it
should return nil.")

(defvar standoff-markup-range-add-function 'standoff-dummy-add-range
  "The function that adds a range to a markup element in some backend.
This variable must be set to the function's symbol (name). The
function must take the following arguments:

BUFFER STARTCHAR ENDCHAR MARKUP-INST-ID

The function is expected to return the ID of the markup
element. When storing to the backend was not successfull, it
should return nil.")

(defvar standoff-markup-read-function 'standoff-dummy-read-markup
  "The function that gets the markup from some backend.
This variable must be set to the function's symbol (name). The
function must take the following arguments:

BUFFER &optional STARTCHAR ENDCHAR MARKUP-NAME MARKUP-INST-ID

The optional parameters should be used for filtering the returned
markup. If STARTCHAR *and* ENDCHAR are numerical values, the
function should return only markup elements that overlap this
portion of BUFFER. If MARKUP-NAME is given, only markup elements
of this type should be returned. If MARKUP-INST-ID is given, only
the markup with this id should be returned. So, if none of
STARTCHAR, ENDCHAR, MARKUP-NAME and MARKUP-ID is given (or all of
them are nil), the function should return all markup element
ranges for the buffer.

The function is expected to return a list of markup ranges which
are again represented as true lists, as follows:

(markup markup markup ...)

where markup takes the form of a true list as

(MARKUP-INST-ID MARKUP-TYPE STARTCHAR ENDCHAR &rest)

If a markup element consists of more than one
range--i.e. discontinous markup--the same values for MARKUP-NAME
and MARKUP-INST-ID must occur in more than one of those markup
lists.")

(defvar standoff-markup-delete-range-function 'standoff-dummy-delete-range
  "The function that deletes (a range of a) markup from some backend.
This variable must be set to the function's symbol (name). The
function should delete a markup element or only a range of this
markup element in case of discountinous markup. The element or
range respectively is given by the following parameters all of
which the function must take:

BUFFER STARTCHAR ENDCHAR MARKUP-NAME MARKUP-INST-ID

The function should return nil or throw an error if the range
could not be deleted and t on successfull deletion. It's up to
the backend to control deletion preconditions which might be:

- any relation to other markup elements. If the markup element
  would consist of only one range then the relation would get
  invalid after deletion. The backend may interact with the user
  in this case.

")

(defvar standoff-markup-types-used-function 'standoff-dummy-markup-types
  "The function that returns names the types of markup in use.
This variable must be set to the function's symbol (name). The
function must take the following arguments:

BUFFER

The function ist expected to return a true list of markup
types.")

(defvar standoff-markup-changed-functions nil
  "A hook for handlers that need to be called if the markup on a
buffer was changed. This can be used for evalution, updating the
highlightning etc. etc.

In terms of elisp this is a so called abnormal hook, i.e. the
hooked functions must take arguments. Arguments are:

BUFFER

BUFFER: the buffer the markup relates to, aka the source document

The return value of the hooked functions is not evaluated at all.")

(defvar standoff-predicates-used-function 'standoff-dummy-used-predicates
  "The function which returns a list of used relation predicates from some backend. 
This variable must be set to the function's symbol (name). The
function must take the following arguments:

BUFFER SUBJECT-ID OBJECT-ID 

The returned list of predicate names must be valid predicate
names for the combination of subject and object. Should return
nil or an empty true list if there are no valid predicates for
this combination.")

(defvar standoff-relation-create-function 'standoff-dummy-create-relation
  "The function which writes a new relation to some backend.
This variable must be set to the function's symbol (name). The
function must take the following arguments:

BUFFER SUBJECT-ID PREDICATE-NAME OBJECT-ID

The function is expected to return a non-nil value, if writing
the relation to the backend was successful, nil in case of
failure.")

(defvar standoff-relations-read-function 'standoff-dummy-read-relations
  "The function which reads relations from some backend. 
This variable must be set to the function's symbol (name). The
function must take the following arguments:

BUFFER &optional SUBJ-ID PREDICATE OBJ-ID

The optional arguments should be interpreted as filter parameters
and a value of nil in either of them should be interpreted as a
wildcard.")

(defvar standoff-relations-delete-function 'standoff-dummy-delete-relation
  "The function that deletes a relation from some backend.
This variable must be set to the function's symbol (name). The
function must take the following arguments:

BUFFER SUBJECT-ID PREDICATE OBJECT-ID

The relation that is to be deleted is given by the three last
arguments. All duplicates of the relation should be removed.")

(provide 'standoff-api)
