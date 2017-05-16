;;; standoff-util.el --- utilities for standoff mode.

;;; Commentary:

;;; Code:

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
