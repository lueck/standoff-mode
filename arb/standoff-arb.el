;;; standoff-arb.el --- config file for Archiv des Beispiels research project.

;;; Code

;;(require 'standoff-mode)

(add-to-list 'auto-mode-alist '("\\.TEI-P5.xml$" . standoff-mode))

(eval-after-load "standoff-mode"
  '(progn

     (message "evaluating configuration for Archiv des Beispiels...")

    ;; Sprache des Benutzerhandbuchs
    (setq standoff-info-language "de")

    ;; Label statt fqd-Namen für Markup-Elemente
    (setq standoff-show-labels t)

    ;; load annotation schema, no error if file not present
    (load "standoff-arb-schema.el" t)

    ;; set how restrictive markup types and predicates are handled
    ;; depending on presence of annotation schema
    (if (featurep 'standoff-arb-schema)
	(setq standoff-markup-type-require-match t
	      standoff-predicate-require-match t)
      (setq standoff-markup-type-require-match 'confirm
	    standoff-predicate-require-match 'confirm))

    (defface standoff-arb-beispiel
      '((((type x))
	 (:family
	  "misc-fixed"
	  :foreground
	  "dark red"
	  :weight
	  normal
	  :slant
	  normal))
	(t
	 (:foreground
	  "dark red"
	  :weight
	  normal
	  :slant
	  normal)))
      "Face used \"beispiel\" markup type."
      :group 'standoff-faces)

    (defface standoff-arb-konzept
      '((((type x))
	 (:family
	  "misc-fixed"
	  :foreground
	  "forest green"
	  :weight
	  normal
      :slant
      normal))
	(t
	 (:foreground
	  "forest green"
	  :weight
	  normal
	  :slant
	  normal)))
      "Face used \"konzept\" markup type."
      :group 'standoff-faces)

    (defface standoff-arb-marker
      '((((type x))
	 (:family
	  "misc-fixed"
	  :foreground
	  "dark orange"
	  :weight
	  normal
	  :slant
	  normal))
	(t
	 (:foreground
	  "dark orange"
	  :weight
	  normal
	  :slant
	  normal)))
      "Face used \"marker\" markup type."
      :group 'standoff-faces)

    (defface standoff-arb-kontext
      '((((type x))
	 (:family
	  "misc-fixed"
	  :foreground
	  "dark blue"
	  :weight
	  normal
	  :slant
	  normal))
	(t
	 (:foreground
	  "dark blue"
	  :weight
	  normal
	  :slant
	  normal)))
      "Face used \"kontext\" markup type."
      :group 'standoff-faces)

    ))

;;; standoff-arb.el ends here.
