;;; standoff-util.el --- utilities for standoff mode.

;;; Commentary:

;;; Code:

;;;; getting and setting the source buffer

(defvar standoff-current-source-buffer nil
  "Current source buffer.")

(defun standoff-util/get-source-buffer ()
  "Return the current source buffer."
  (get-buffer standoff-current-source-buffer))

(defun standoff-util/set-source-buffer (buffer)
  "Set the current source buffer name BUFFER."
  (if (bufferp buffer)
      (setq standoff-current-source-buffer (buffer-name buffer))
    (setq standoff-current-source-buffer buffer)))

;;;; formatting UI output

(defun standoff-util/plural-s (count)
  "Return a plural s if COUNT does not equal 1."
  (if (= 1 count) "" "s"))

;;;; UUIDs

(defvar standoff-util/re-uuid
  "[[:xdigit:]]\\{8\\}-[[:xdigit:]]\\{4\\}-[[:xdigit:]]\\{4\\}-[[:xdigit:]]\\{4\\}-[[:xdigit:]]\\{12\\}"
  "Regular expression for uuid.
Matches xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")

(defun standoff-util/create-uuid (&optional data pos)
  "Create a UUID, using a simple hashing of variable data.
Example of a UUID: 1df63142-a513-c850-31a3-535fc3520c3dq.  The
parameters DATA and POS are optional, but have no effect.  They
are only there to make this function compatible to funcalls of
the function symbol stored in
`standoff-dummy-create-id-function'.

Note: This code uses https://en.wikipedia.org/wiki/Md5 , which is
not cryptographically safe.  Written by Christopher Wellons, 2011,
edited by Xah Lee and other, taken from URL
`http://ergoemacs.org/emacs/elisp_generate_uuid.html'."
  (let ((myStr (md5 (format "%s%s%s%s%s%s%s%s%s%s"
                            (user-uid)
                            (emacs-pid)
                            (system-name)
                            (user-full-name)
                            (current-time)
                            (emacs-uptime)
                            (garbage-collect)
                            (buffer-string)
                            (random)
                            (recent-keys)))))
    (format "%s-%s-4%s-%s%s-%s"
                    (substring myStr 0 8)
                    (substring myStr 8 12)
                    (substring myStr 13 16)
                    (format "%x" (+ 8 (random 4)))
                    (substring myStr 17 20)
                    (substring myStr 20 32))))

(provide 'standoff-util)

;;; standoff-util.el ends here
