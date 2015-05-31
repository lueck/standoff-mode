;; -*- coding: utf-8 -*-

(require 'standoff-dummy)
;;(require 'standoff-mode)

;; Monkey patching in order to make tests non-interactive

(when noninteractive

  (setq standoff-y-or-n-p nil)
  (defun y-or-n-p (prompt)
    standoff-y-or-n-p)

  (setq standoff-yes-or-no-p nil)
  (defun yes-or-no-p (prompt)
    standoff-yes-or-no-p)
)
  
;; Setup utility functions

(defun standoff-test-utils-save-old-config ()
  "Save the configuration so it can be restored after running a test."
  ;; TODO
  )

(defun standoff-test-utils-restore-old-config ()
  "Restore old configuration from before test setup."
  ;; TODO
  )

(defun standoff-test-utils-setup-source-buffer ()
  "Save the current configuration and setup testing environment.
Returns a test buffer with some content, sets up the dummy
backend."
  (let ((test-buffer (generate-new-buffer "dummy-test")))
    (standoff-test-utils-save-old-config)
    ;; create and fill test buffer
    (set-buffer test-buffer)
    (insert "Was kann das Licht in unſer Auge mahlen?
Was ſich mahlen laͤßt, Bilder. Wie auf der
weißen Wand der dunklen Kammer, ſo faͤllt auf
die Netzhaut des Auges ein Stralenpinſel von
allem, was vor ihm ſtehet, und kann nichts, als
was […] was da ſteht, eine Flaͤche, ein Nebeneinander
aller und der verſchiedenſten ſichtbaren Gegenſtaͤnde
zeichnen. Dinge hinter einander, oder ſolide,
maſſive Dinge als ſolche dem Auge zu geben, iſt
ſo unmoͤglich, als den Liebhaber hinter der dicken
Tapete, den Bauer innerhalb der Windmuͤhle
ſingend zu mahlen.")
    ;; register und reset dummy backend
    (standoff-dummy-register-backend)
    (setq standoff-dummy-create-id-function 'standoff-dummy-create-uuid)
    (standoff-dummy--backend-reset);; Needed?
    ;; Return test buffer
    test-buffer))

(defun standoff-test-utils-setup-overlays ()
  "Set up overlays for testing environment.
Make shure that the old config is saved before and restored after
using this setup function."
  (setq standoff-markup-overlays
	'(("beispiel" 
	   ('priority 10)
	   ('face (:foreground "red")))
	  ("marker"
	   ('priority 20)
	   ('face (:background "light blue")))
	  ("konzept"
	   ('priority 5)
	   ('face (:foreground "green"))))))

(defun standoff-test-utils-return-relations-allowed ()
  "Set up `standoff-relations-allowed'."
  '((("marker") "markiert" ("beispiel"))
    (("beispiel") "illustriert" ("konzept" "kontext"))
    (("beispiel") "belegt" ())
    (("beispiel" "beruf" "stand" "person") "stehtFür" nil)))

;; Tear down utility functions

(defun standoff-test-utils-teardown-source-buffer (buf)
  "Tear down the test setup and restore the old configuration."
  (kill-buffer buf)
  (standoff-test-utils-restore-old-config))

(provide 'standoff-test-utils)
