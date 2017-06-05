;;; standoff-log.el --- Logging for standoff mode.

;;; Commentary:

;; Logging failures on operations, and showing the results.

;; This was adapted from dired.el

;;; Code:

(defvar standoff-log-buffer "*Stand-Off log*")

(defun standoff-why ()
  "Pop up a buffer with error log output from standoff.
A group of errors from a single command ends with a formfeed.
Thus, use \\[backward-page] to find the beginning of a group of errors."
  (interactive)
  (if (get-buffer standoff-log-buffer)
      (let ((owindow (selected-window))
	    (window (display-buffer (get-buffer standoff-log-buffer))))
	(unwind-protect
	    (progn
	      (select-window window)
	      (goto-char (point-max))
	      (forward-line -1)
	      (backward-page 1)
	      (recenter 0))
	  (select-window owindow)))))

(defun standoff-log (log &rest args)
"Log a message or the contents of a buffer.
If LOG is a string and there are more args, it is formatted with
those ARGS.  Usually the LOG string ends with a \n.
End each bunch of errors with (standoff-log t):
this inserts the current time and buffer at the start of the page,
and \f (formfeed) at the end."
  (let ((obuf (current-buffer)))
    (with-current-buffer (get-buffer-create standoff-log-buffer)
      (goto-char (point-max))
      (let ((inhibit-read-only t))
	(cond ((stringp log)
	       (insert (if args
			   (apply (function format) log args)
			 log)))
	      ((bufferp log)
	       (insert-buffer-substring log))
	      ((eq t log)
	       (backward-page 1)
	       (unless (bolp)
		 (insert "\n"))
	       (insert (current-time-string)
		       "\tBuffer `" (buffer-name obuf) "'\n")
	       (goto-char (point-max))
	       (insert "\f\n")))))))

(defun standoff-log-summary (string failures)
  "State a summary of a command's failures, in echo area and log buffer.
STRING is an overall summary of the failures.  FAILURES is a list
of annotations that we failed to operate on, or nil if
annotations are not applicable."
  (if (= (length failures) 1)
      (message "%s"
	       (with-current-buffer standoff-log-buffer
		 (goto-char (point-max))
		 (backward-page 1)
		 (if (eolp) (forward-line 1))
		 (buffer-substring (point) (point-max))))
    (message (if failures "%s--type ? for details (%s)"
	       "%s--type ? for details")
	     string failures))
  ;; Log a summary describing a bunch of errors.
  (standoff-log (concat "\n" string "\n"))
  (standoff-log t))

(provide 'standoff-log)

;;; standoff-log.el ends here
