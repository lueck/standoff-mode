;;; standoff-xml.el --- Hide tags and character codes in XML documents.

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

;; Hide XML-tags and XML character references in standoff-mode.

;; Code is inspired by sgml-mode.el written James clark and licensed
;; under the GNU General Public License, and from nxml-mode.el, also
;; written by James Clark and licensed under the GNU General Public
;; License v3. The code was adapted to the needs of standoff-mode and
;; to the standoff-xml namespace.

;;; Code

;; TODO: Should we make a minor mode from this?

;;;; Show/hide tags

(defconst standoff-xml-namespace-re "[_[:alpha:]][-_.[:alnum:]]*")
(defconst standoff-xml-name-re "[_:[:alpha:]][-_.:[:alnum:]]*")
(defconst standoff-xml-tag-name-re (concat "<\\([!/?]?" standoff-xml-name-re "\\)"))
(defconst standoff-xml-attrs-re "\\(?:[^\"'/><]\\|\"[^\"]*\"\\|'[^']*'\\)*")
(defconst standoff-xml-start-tag-regex (concat "<" standoff-xml-name-re standoff-xml-attrs-re)
  "Regular expression that matches a non-empty start tag.
Any terminating `>' or `/' is not matched.")

(defvar standoff-xml-display-text ()
  "Tag names as lowercase symbols, and display string when invisible.")

;; internal
(defvar standoff-xml-tags-invisible nil)

(defvar standoff-xml-show-hidden-tags nil
  "Whether or not to show hidden tags per `message' when moving point over it.")

(defun standoff-xml-point-entered (x y)
  ;; Show preceding or following hidden tag, depending of cursor direction.
  (when standoff-xml-show-hidden-tags
    (let ((inhibit-point-motion-hooks t))
      (save-excursion
	(condition-case nil
	    (message "Invisible tag: %s"
		     ;; Strip properties, otherwise, the text is invisible.
		     (buffer-substring-no-properties
		      (point)
		      (if (or (and (> x y)
				   (not (eq (following-char) ?<)))
			      (and (< x y)
				   (eq (preceding-char) ?>)))
			  (backward-list)
			(forward-list))))
	  (error nil))))))

(or (get 'standoff-xml-tag 'invisible)
    (setplist 'standoff-xml-tag
	      (append '(invisible t
			point-entered standoff-xml-point-entered
			rear-nonsticky t
			read-only t)
		      (symbol-plist 'standoff-xml-tag))))

(defun standoff-xml-tags-invisible (arg)
  "Toggle visibility of existing tags."
  (interactive "P")
  (let ((modified (buffer-modified-p))
	(inhibit-read-only t)
	(inhibit-modification-hooks t)
	;; Avoid spurious the `file-locked' checks.
	(buffer-file-name nil)
	;; This is needed in case font lock gets called,
	;; since it moves point and might call standoff-xml-point-entered.
	;; How could it get called?  -stef
	(inhibit-point-motion-hooks t)
	string)
    (unwind-protect
	(save-excursion
	  (goto-char (point-min))
	  ;; standoff-xml-tags-invisible als buffer-locale Variable setzen.  t
	  ;; wenn arg >= 0 (sollte > 0) sonst das inverse des
	  ;; aktuellen Wertes von standoff-xml-tags-invisible (toggle)
	  (if (setq-local standoff-xml-tags-invisible
			  (if arg
			      (>= (prefix-numeric-value arg) 0)
			    (not standoff-xml-tags-invisible)))
	      ;; mache unsichtbar
	      ;; 1) suche nach Tags
	      (while (re-search-forward standoff-xml-tag-name-re nil t)
		;; 2) speichere den Text des Tags
		(setq string
		      (cdr (assq (intern-soft (downcase (match-string 1)))
				 standoff-xml-display-text)))
		;; 
		(goto-char (match-beginning 0))
		;; wenn string ein string und keine overlays at point vorhanden
		;; dann mache erzeuge über einen overlay über dem Tag
		;; speichere string als before-string und
		;; setze die Eigenschaft standoff-xml-tag auf t
		(and (stringp string)
		     (not (overlays-at (point)))
		     (let ((ol (make-overlay (point) (match-beginning 1))))
		       ;;(overlay-put ol 'before-string string)
		       (overlay-put ol 'standoff-xml-tag t)))
		;; setze von point bis zum nächsten klammer-ähnlichen Ausdruck
		;; die Textproperty Kategorie standoff-xml-tag  
		(put-text-property (point)
				   ;;(progn (forward-list) (point))
				   (progn (search-forward ">") (point))
				   'category 'standoff-xml-tag))
	    ;; mache sichtbar
	    (let ((pos (point-min)))
	      (while (< (setq pos (next-overlay-change pos)) (point-max))
		(dolist (ol (overlays-at pos))
		  (if (overlay-get ol 'standoff-xml-tag)
		      (delete-overlay ol)))))
	    (remove-text-properties (point-min) (point-max) '(category nil))))
      (restore-buffer-modified-p modified))
    (run-hooks 'standoff-xml-tags-invisible-hook)
    (message "")))

;;;; Show/hide character references

(defvar standoff-xml-char-ref-re "&#x\\([[:xdigit:]]\\{2,4\\}\\);\\|&#\\([[:digit:]]+\\);")

(defvar standoff-xml-char-ref-glyph-display nil
  "Non-nil means that character references are substituted by glyphs.")
(make-variable-buffer-local 'standoff-xml-char-ref-glyph-display)

(defface standoff-xml-glyph
  '((((type x))
     (:family
      "misc-fixed"
      :background
      "gainsboro"
      :foreground
      "black"
      :weight
      normal
      :slant
      normal))
    (t
     (:background
      "gainsboro"
      :foreground
      "black"
      :weight
      normal
      :slant
      normal)))
  "Face used for glyph for char references."
  :group 'standoff-faces)

(defface standoff-xml-ref
  '((t (:inherit font-lock-constant-face)))
  "Face used to highlight character and entity references.
This is not used directly, but only via inheritance by other faces."
  :group 'standoff-faces)

(defun standoff-xml-toggle-char-ref-glyph-substitute (arg)
  "Toggle the displaying substitution of character references by glyphs."
  (interactive "P")
  (let ((new (if (null arg)
		 (not standoff-xml-char-ref-glyph-display)
	       (> (prefix-numeric-value arg) 0)))
	(char-ref))
    (when (not (eq new standoff-xml-char-ref-glyph-display))
      (setq standoff-xml-char-ref-glyph-display new)
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward standoff-xml-char-ref-re nil t)
	  (if standoff-xml-char-ref-glyph-display
	      (progn
		(setq char-ref (cond ((match-string 1)
				      (string-to-number (match-string 1) 16))
				     ((match-string 2)
				      (string-to-number (match-string 2)))
				     (t nil)))
		(when char-ref
		  (standoff-xml-char-ref-display-glyph (- (point) (length (match-string 0))) (point) char-ref)))
	    (standoff-xml-clear-char-ref-glyph-display (- (point) (length (match-string 0))) (point))))))))

(put 'standoff-xml-char-ref 'evaporate t)

(defun standoff-xml-char-ref-display-glyph (start end n)
  "Display glyph instead of char reference.
Puts an overlay from START to END, makes the passage invisible
and displays a character given by numerical value N instead."
  (let ((glyph (char-to-string n))
	ov)
    (setq ov (make-overlay start end nil t))
    (overlay-put ov 'category 'standoff-xml-char-ref)
    (overlay-put ov 'help-echo glyph)
    (overlay-put ov
		 'after-string
		 (propertize glyph 'face 'standoff-xml-glyph))
    (overlay-put ov 'invisible t)))

(defun standoff-xml-clear-char-ref-glyph-display (start end)
  "Unhide a character reference hidden by an overlay."
  (let ((ov (overlays-in start end)))
    (while ov
      (when (eq (overlay-get (car ov) 'category) 'standoff-xml-char-ref)
	(delete-overlay (car ov)))
      (setq ov (cdr ov)))
    ;;(put-text-property start end 'face 'standoff-xml-ref)
    ))


(provide 'standoff-xml)

;;; standoff-xml.el ends here.
