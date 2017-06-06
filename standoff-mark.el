;;; standoff-mark.el --- Marking and operating on items.

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

;; This file defines some commands for marking and operating on marks
;; that can be reused in different buffers listing annotations one by
;; line like files in a Dired buffer.

;; Adapted from dired.el from GNU Emacs.

;; While adapting from dired.el, the word 'files' was subsituted with
;; 'items', which was the most general term. We could also have said
;; 'annotations'. We always say *items* buffer in doc strings, but
;; there will be a *Relations* buffer, a *Literals* buffer etc.

;;; Code:

(require 'standoff-util)
(require 'standoff-log)

;;;; Marking

(defcustom standoff-mark/no-confirm nil
  "A list of symbols for commands operate should not confirm, or t.
Command symbols are `delete'.
If t, confirmation is never needed."
  :group 'standoff
  :type '(choice (const :tag "Confirmation never needed" t)
		 (set (const delete))))

;; These regexps must be tested at beginning-of-line, but are also
;; used to search for next matches, so neither omitting "^" nor
;; replacing "^" by "\n" (to make it slightly faster) will work.

(defvar standoff-mark/re-mark "^[^ \n]")
;; "Regexp matching a marked line.
;; Important: the match ends just after the marker."
(defvar standoff-mark/re-maybe-mark "^. ")

(defvar standoff-mark/marker-char ?*
  "In the *items* buffer, the current mark character.
This is what the do-commands look for, and what the mark-commands store.")

(defvar standoff-mark/del-marker ?D
  "Character used to flag items for deletion.")

(defun standoff-mark/marker-regexp ()
  "Return a regexp, matching the marker character."
  (concat "^" (regexp-quote (char-to-string standoff-mark/marker-char))))

;;;; Commands for marking, moving etc.

(defun standoff-mark (arg &optional interactive)
  "Mark the item at point in the *items* buffer.
If the region is active, mark all items in the region.
Otherwise, with a prefix arg, mark items on the next ARG lines.
INTERACTIVE should be t, if called interactively.

Use \\[standoff-unmark-all] to remove all marks and
\\[standoff-unmark] to remove a single mark."
  (interactive (list current-prefix-arg t))
  (if (and interactive (use-region-p))
      ;; Mark items in the active region.
      (save-excursion
	(let ((beg (region-beginning))
	      (end (region-end)))
	  (standoff-mark/mark-items-in-region
	   (progn (goto-char beg) (line-beginning-position))
	   (progn (goto-char end) (line-beginning-position)))))
    ;; Mark the current (or next ARG) items.
    (let ((inhibit-read-only t))
      (standoff-mark/repeat-over-lines
       (prefix-numeric-value arg)
       #'(lambda ()
	   (delete-char 1)
	   (insert standoff-mark/marker-char))))))

(defun standoff-unmark (arg &optional interactive)
  "Unmark the item at point in the *items* buffer.
If the region is active, unmark all items in the region.
Otherwise, with a prefix arg, unmark items on the next ARG
lines.  INTERACTIVE should be t, if called interactively."
  (interactive (list current-prefix-arg t))
  (let ((standoff-mark/marker-char ?\040))
    (standoff-mark arg interactive)))

(defun standoff-unmark-all-marks ()
  "Remove all marks from all items in the *items* buffer."
  (interactive)
  (standoff-unmark-all-items ?\r))

(defun standoff-unmark-all-items (mark &optional arg)
  "Remove a specific MARK (or any mark) from every item.
After this command, type the mark character to remove,
or type RET to remove all marks.
With prefix ARG, query for each marked item.
Type \\[help-command] at that time for help."
  (interactive "cRemove marks (RET means all): \nP")
  (save-excursion
    (let* ((count 0)
	   (inhibit-read-only t) case-fold-search
	   (string (format "\n%c" mark))
	   (help-form "\
Type SPC or `y' to unmark one item, DEL or `n' to skip to next,
`!' to unmark all remaining items with no more questions."))
      (goto-char (point-min))
      (while (if (eq mark ?\r)
		 (re-search-forward standoff-mark/re-mark nil t)
	       (search-forward string nil t))
	(if (or (not arg)
		(let ((item (standoff-mark/get-item-id)))
		  (and item
		       (standoff-mark/query 'query "Unmark `%s'? "
				    item))))
	    (progn (subst-char-in-region (1- (point)) (point)
					 (preceding-char) ?\s)
		   (setq count (1+ count)))))
      (message (if (= count 1) "1 mark removed"
		 "%d marks removed")
	       count))))

(defun standoff-toggle-marks ()
  "Toggle marks: marked items become unmarked, and vice versa.
Items marked with other flags (such as `D') are not affected.
As always, hidden items are not affected."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((inhibit-read-only t))
      (while (not (eobp))
	;; use subst instead of insdel because it does not move
	;; the gap and thus should be faster and because
	;; other characters are left alone automatically
	(apply 'subst-char-in-region
	       (point) (1+ (point))
	       (if (eq ?\040 (following-char)) ; SPC
		   (list ?\040 standoff-mark/marker-char)
		 (list standoff-mark/marker-char ?\040)))
	(forward-line 1)))))

(defun standoff-flag-delete (arg &optional interactive)
  "In the *items* buffer, flag the current line's item for deletion.
If the region is active, flag all items in the region.
Otherwise, with a prefix arg, flag items on the next ARG
lines.  INTERACTIVE should be t, if called interactively."
  (interactive (list current-prefix-arg t))
  (let ((standoff-mark/marker-char standoff-mark/del-marker))
    (standoff-mark arg interactive)))

(defun standoff-next-item (arg)
  "Move down lines then position at item.
Optional prefix ARG says how many lines to move; default is one line."
  (interactive "p")
  (let ((line-move-visual)
	(goal-column))
    (line-move arg t))
  ;; We never want to move point into an invisible line.
  (while (and (invisible-p (point))
	      (not (if (and arg (< arg 0)) (bobp) (eobp))))
    (forward-char (if (and arg (< arg 0)) -1 1)))
  (standoff-mark/move-to-item))

(defun standoff-previous-item (arg)
  "Move up lines then position at item.
Optional prefix ARG says how many lines to move; default is one line."
  (interactive "p")
  (standoff-next-item (- (or arg 1))))

;;;; helper function

(defun standoff-mark/mark-items-in-region (start end)
  "Mark items in region given by START and END."
  (let ((inhibit-read-only t))
    (if (> start end)
	(error "start > end"))
    (goto-char start)			; assumed at beginning of line
    (while (< (point) end)
      (delete-char 1)
      (insert standoff-mark/marker-char)
      (forward-line 1))))

(defun standoff-mark/repeat-over-lines (arg function)
  "If ARG is numerical, FUNCTION is called on ARG lines."
  (let ((pos (make-marker)))
    (beginning-of-line)
    (while (and (> arg 0) (not (eobp)))
      (setq arg (1- arg))
      (beginning-of-line)
      (save-excursion
	(forward-line 1)
	(move-marker pos (1+ (point))))
      (save-excursion (funcall function))
      ;; Advance to the next line--actually, to the line that *was* next.
      ;; (If FUNCTION inserted some new lines in between, skip them.)
      (goto-char pos))
    (while (and (< arg 0) (not (bobp)))
      (setq arg (1+ arg))
      (forward-line -1)
      (beginning-of-line)
      (save-excursion (funcall function)))
    (move-marker pos nil)
    (standoff-mark/move-to-item)))

(defun standoff-mark/move-to-item ()
  "Move to the item in the current line."
  ;; TODO
  (beginning-of-line))

;;;; Operate

(defun standoff-mark/get-item-id ()
  "Return the id of the item in this line."
  (save-excursion
    (let ((limit (end-of-line))
	  rel-id)
      (beginning-of-line)
      (if (re-search-forward standoff-util/re-uuid limit t)
	  (setq rel-id (match-string 0))	; returns item id
	(error "No Id found in this line")))))

(defun standoff-mark/map-over-marks (body arg &optional show-progress
					       distinguish-one-marked)
  "Call function BODY with point on each marked line.
Return a list of BODY's results.  If no marked item could be
found, execute BODY on the current line.  ARG, if non-nil,
specifies the items to use instead of the marked items.

If ARG is an integer, use the next ARG (or previous -ARG, if
ARG<0) items.  In that case, point is dragged along.  This is
so that commands on the next ARG (instead of the marked) items
can be chained easily.  For any other non-nil value of ARG, use
the current item.

If optional third arg SHOW-PROGRESS evaluates to non-nil,
redisplay the *items* buffer after each item is
processed.

No guarantee is made about the position on the marked line.
BODY must ensure this itself if it depends on this.

Search starts at the beginning of the buffer, thus the car of the
list corresponds to the line nearest to the buffer's bottom.
This is also true for (positive and negative) integer values of
ARG.

BODY should not be too long as it is called four times.

If DISTINGUISH-ONE-MARKED is non-nil, then if we find just one
marked item, return (t ITEM-ID) instead of (ITEM-ID)."
  (prog1
      (let ((inhibit-read-only t)
	    case-fold-search
	    found
	    results)
	(if arg
	    (if (integerp arg)
		(progn	;; no save-excursion, want to move point.
		  (standoff-mark/repeat-over-lines
		   arg
		   (function (lambda ()
			       (if show-progress (sit-for 0))
			       (setq results (cons (funcall body) results)))))
		  (if (< arg 0)
		      (nreverse results)
		    results))
	      ;; non-nil, non-integer ARG means use current item:
	      (list (funcall body)))
	  ;; arg is nil
	  (let ((regexp (standoff-mark/marker-regexp))
		next-position)
	    (save-excursion
	      (goto-char (point-min))
	      ;; remember position of next marked item before BODY
	      ;; can insert lines before the just found item,
	      ;; confusing us by finding the same marked item again
	      ;; and again and...
	      (setq next-position (and (re-search-forward regexp nil t)
				       (point-marker))
		    found (not (null next-position)))
	      (while next-position
		(goto-char next-position)
		(if show-progress (sit-for 0))
		(setq results (cons (funcall body) results))
		;; move after last match
		(goto-char next-position)
		(forward-line 1)
		(set-marker next-position nil)
		(setq next-position (and (re-search-forward regexp nil t)
					 (point-marker)))))
	    (if (and distinguish-one-marked (= (length results) 1))
		(setq results (cons t results)))
	    (if found
		results
	      (list (funcall body))))))
     ;; save-excursion loses, again
    (standoff-mark/move-to-item)))

(defun standoff-mark/map-over-marks-off (body arg &optional show-progress
					      distinguish-one-marked)
  ;; This version does not map over marks
  (prog1
      (let ((inhibit-read-only t)
	    case-fold-search
	    found
	    results)
	(if arg
	    (if (integerp arg)
		(progn	;; no save-excursion, want to move point.
		  (standoff-mark/repeat-over-lines
		   arg
		   (function (lambda ()
			       (if show-progress (sit-for 0))
			       (setq results (cons body results)))))
		  (if (< arg 0)
		      (nreverse results)
		    results))
	      ;; non-nil, non-integer ARG means use current item:
	      (list body))
	  (let ((regexp (standoff-mark/marker-regexp))
		next-position)
	    (save-excursion
	      (goto-char (point-min))
	      ;; remember position of next marked item before BODY
	      ;; can insert lines before the just found item,
	      ;; confusing us by finding the same marked item again
	      ;; and again and...
	      (setq next-position (and (re-search-forward regexp nil t)
				       (point-marker))
		    found (not (null next-position)))
	      (while next-position
		(goto-char next-position)
		(if show-progress (sit-for 0))
		(setq results (cons body results))
		;; move after last match
		(goto-char next-position)
		(forward-line 1)
		(set-marker next-position nil)
		(setq next-position (and (re-search-forward regexp nil t)
					 (point-marker)))))
	    (if (and distinguish-one-marked (= (length results) 1))
		(setq results (cons t results)))
	    (if found
		results
	      (list body)))))
     ;; save-excursion loses, again
    (standoff-mark/move-to-item)))

(defun standoff-mark/map-over-marks-quoted (body arg &optional show-progress
						  distinguish-one-marked)
  "Original implementation adapted from dired."
  `(prog1
       (let ((inhibit-read-only t)
	     case-fold-search
	     found
	     results)
	 (if ,arg
	     (if (integerp ,arg)
		 (progn	;; no save-excursion, want to move point.
		   (standoff-mark/repeat-over-lines
		    ,arg
		    (function (lambda ()
				(if ,show-progress (sit-for 0))
				(setq results (cons ,body results)))))
		   (if (< ,arg 0)
		       (nreverse results)
		     results))
	       ;; non-nil, non-integer ARG means use current item:
	       (list ,body))
	   (let ((regexp (standoff-mark/marker-regexp))
		 next-position)
	     (save-excursion
	       (goto-char (point-min))
	       ;; remember position of next marked item before BODY
	       ;; can insert lines before the just found item,
	       ;; confusing us by finding the same marked item again
	       ;; and again and...
	       (setq next-position (and (re-search-forward regexp nil t)
					(point-marker))
		     found (not (null next-position)))
	       (while next-position
		 (goto-char next-position)
		 (if ,show-progress (sit-for 0))
		 (setq results (cons ,body results))
		 ;; move after last match
		 (goto-char next-position)
		 (forward-line 1)
		 (set-marker next-position nil)
		 (setq next-position (and (re-search-forward regexp nil t)
					  (point-marker)))))
	     (if (and ,distinguish-one-marked (= (length results) 1))
		 (setq results (cons t results)))
	     (if found
		 results
	       (list ,body)))))
     ;; save-excursion loses, again
     (standoff-mark/move-to-item)))

(defvar standoff-mark/deletion-confirmer 'yes-or-no-p) ; or y-or-n-p?

(defun standoff-mark/query (symbol query-string &rest subst)
  "Query the user QUERY-STRING formatted with SUBST.
SYMBOL is ignored."
  ;; FIXME: Adapt from dired.
  (y-or-n-p (format query-string subst)))

(defun standoff-mark/internal-do-deletions (l arg delete-function &optional trash)
  "L is an alist of items to delete, with their buffer positions.
ARG is the prefix arg.  Itemnames are absolute.  (car L) *must*
be the *last* (bottommost) item in the *items* buffer.  That way
as changes are made in the buffer they do not shift the lines
still to be changed, so the (point) values in L stay valid.  The
DELETE-FUNCTION is called with the id of the item to be deleted
as argument.  If TRASH is non-nil, the deleted item will be moved
to trash."
  (let* ((items (mapcar (function car) l))
	 (count (length l))
	 (succ 0)
	 (trashing (and trash delete-by-moving-to-trash)))
    ;; canonicalize items list for pop up
    (if (standoff-mark/mark-pop-up
	 " *Deletions*" 'delete items standoff-mark/deletion-confirmer
	 (format "%s %s "
		 (if trashing "Trash" "Delete")
		 (standoff-mark/mark-prompt arg items)))
	(save-excursion
	  (let ((progress-reporter
		 (make-progress-reporter
		  (if trashing "Trashing..." "Deleting...")
		  succ count))
		failures) ;; items better be in reverse order for this loop!
	    (while l
	      (goto-char (cdr (car l)))
	      (let ((inhibit-read-only t))
		(condition-case err
		    (let ((rel-id (car (car l)))
			  (del-start (progn (beginning-of-line) (point)))
			  (del-end (progn (forward-line 1) (point))))
		      (goto-char del-start)
		      (funcall delete-function rel-id)
		      ;; if we get here, removing worked
		      (setq succ (1+ succ))
		      (progress-reporter-update progress-reporter succ)
		      (delete-region del-start del-end))
		  (error ;; catch errors from failed deletions
		   (standoff-log "%s\n" err)
		   (setq failures (cons (car (car l)) failures)))))
	      (setq l (cdr l)))
	    (if (not failures)
		(progress-reporter-done progress-reporter)
	      (standoff-log-summary
	       (format "%d of %d deletion%s failed"
		       (length failures) count
		       (standoff-util/plural-s count))
	       failures))))
      (message "(No deletions performed)")))
  (standoff-mark/move-to-item))

(defun standoff-mark/mark-prompt (arg items)
  "Return a string suitable for use in a operation prompt.
ARG is normally the prefix argument for the calling command.
ITEMS should be a list of ITEM-IDs.

The return value has a form like \"<UUID>\", \"[next 3 items]\",
or \"* [3 items]\"."
  ;; distinguish-one-marked can cause the first element to be just t.
  (if (eq (car items) t) (setq items (cdr items)))
  (let ((count (length items)))
    (if (= count 1)
	(car items)
      ;; more than 1 item:
      (if (integerp arg)
	  ;; abs(arg) = count
	  ;; Perhaps this is nicer, but it also takes more screen space:
	  ;;(format "[%s %d items]" (if (> arg 0) "next" "previous")
	  ;;                        count)
	  (format "[next %d items]" arg)
	(format "%c [%d items]" standoff-mark/marker-char count)))))

(defun standoff-mark/mark-pop-up (buffer-or-name op-symbol items function &rest args)
  "Return FUNCTION's result on ARGS after showing which items are marked.
Displays the items in a window showing a buffer named
BUFFER-OR-NAME; the default name being \" *Marked Items*\".  The
window is not shown if there is just one item,
`standoff-mark/no-confirm' is t, or OP-SYMBOL is a member of
the list in `standoff-mark/no-confirm'.

By default, the display buffer is shrinked to fit the marked
items.  To disable this, use the Customization interface to
add a new rule to `display-buffer-alist' where condition regexp
is \"^ \\*Marked Items\\*$\", action argument symbol is
`window-height' and its value is nil.

ITEMS is the list of marked items.  It can also be (t ITEM)
in the case of one marked item, to distinguish that from using
just the current item.

FUNCTION should not manipulate items, just read input (an
argument or confirmation)."
  (if (or (eq standoff-mark/no-confirm t)
	  (memq op-symbol standoff-mark/no-confirm)
	  ;; If ITEMS defaulted to the current line's item.
	  (= (length items) 1))
      (apply function args)
    (let ((buffer (get-buffer-create (or buffer-or-name " *Marked Items*")))
	  ;; Mark *Marked Items* window as softly-dedicated, to prevent
	  ;; other buffers e.g. *Completions* from reusing it (bug#17554).
	  (display-buffer-mark-dedicated 'soft))
      (with-current-buffer buffer
	(with-current-buffer-window
	 buffer
	 (cons 'display-buffer-below-selected
	       '((window-height . fit-window-to-buffer)))
	 #'(lambda (window _value)
	     (with-selected-window window
	       (unwind-protect
		   (apply function args)
		 (when (window-live-p window)
		   (quit-restore-window window 'kill)))))
	 ;; Handle (t ITEM) just like (ITEM), here.  That value is
	 ;; used (only in some cases), to mean just one item that was
	 ;; marked, rather than the current line item.
	 (standoff-mark/format-columns-of-items
	  (if (eq (car items) t) (cdr items) items))
	 (remove-text-properties (point-min) (point-max)
				 '(mouse-face nil help-echo nil)))))))

;;;; formatting

(defun standoff-mark/format-columns-of-items (items)
  "Do nothing at the moment with ITEMS."
  ;; FIXME
  )

;;;; mode maps

(defvar standoff-mark/mark-map
  (let ((map (make-sparse-keymap)))
    ;; Commands for marking and unmarking
    (define-key map "*" nil)
    (define-key map "*m" 'standoff-mark)
    (define-key map "*?" 'standoff-unmark-all-items)
    (define-key map "*!" 'standoff-unmark-all-marks)
    (define-key map "U" 'standoff-unmark-all-marks)
    (define-key map "*t" 'standoff-toggle-marks)
    ;; Lower keys for commands not operating on all the marked files
    (define-key map "d" 'standoff-flag-delete)
    (define-key map "m" 'standoff-mark)
    (define-key map "n" 'standoff-next-item)
    (define-key map "p" 'standoff-previous-item)
    (define-key map "t" 'standoff-toggle-marks)
    (define-key map "u" 'standoff-unmark)
    ;; moving
    (define-key map " " 'standoff-next-item)
    (define-key map [remap next-line] 'standoff-next-item)
    (define-key map [remap previous-line] 'standoff-previous-item)
    map))

(easy-menu-define standoff-mark/mark-menu standoff-mark/mark-map
  "Menu for marking items in standoff-mode."
  '("Mark"
    ["Toggle marks" standoff-toggle-marks]
    ["Mark" standoff-mark]
    ["Unmark" standoff-unmark]
    ["Unmark all" standoff-unmark-all-marks]
    ["Unmark query all" standoff-unmark-all-items]
    ["Flag" standoff-flag-delete]
    ))

(provide 'standoff-mark)

;;; standoff-mark.el ends here
