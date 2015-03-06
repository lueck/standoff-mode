;;(setq standoff-markup-require-name-require-match t)

(setq standoff-markup-overlays
      '(("beispiel" 
	 ('priority 10)
	 ('face (:foreground "red"))
	 )
	("marker"
	 ('priority 20)
	 ('face (:background "light blue"))
	 )
	("konzept"
	 ('priority 5)
	 ('face (:foreground "green"))
	 )
	))

(setq standoff-markup-overlays-front
      '(("beispiel" 
	 ('face (:foreground "red")))
	("marker"
	 ('face (:background "light blue")))
	("konzept"
	 ('face (:foreground "green")))
	))

(setq standoff-markup-overlays-after
      '(("beispiel" 
	 ('face (:foreground "red")))
	("marker"
	 ('face (:background "light blue")))
	("konzept"
	 ('face (:foreground "green")))
	))



;; (setplist 'beispiel-tag 
;; 	  '(face (foreground-color . "red")))
;; (setplist 'marker-tag 
;; 	  '(face (background-color . "light blue")))
;; (setplist 'konzept-tag 
;; 	  '(face (foreground-color . "red")))



