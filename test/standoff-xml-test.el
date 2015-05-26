:; exec emacs -Q --script "$0" -- "$@"

;; WARNING: Close your production files before running tests! Restart
;; Emacs before going from testing to production again, because it is
;; not shure, that all configuration is restored correctly. You might
;; loose data! -- OR BETTER: Run this test file in batch mode.

;;; Code

(when noninteractive
  ;; set load path to . and ..
  (setq standoff-lib-dir (concat (file-name-directory load-file-name) "/.."))
  (push standoff-lib-dir load-path)
  (push (file-name-directory load-file-name) load-path)
  ;; pop "--" from argv
  (setq argv (cdr argv))
  )

(require 'ert)
(require 'standoff-test-utils)
(require 'standoff-xml)

;;; Test setup and teardown

(defun standoff-xml-test-setup-source-buffer ()
  "Save the current configuration and setup testing environment.
Returns a test buffer with some content, sets up the dummy
backend."
  (let ((test-buffer (generate-new-buffer "standoff-xml-test")))
    (standoff-test-utils-save-old-config)
    ;; create and fill test buffer
    (set-buffer test-buffer)
    (insert "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<TEI xmlns=\"http://www.tei-c.org/ns/1.0\">
  <teiHeader/>
  <text>\n")
    (insert "<p>Was kann das Licht in un&#x017F;er Auge mahlen?<lb/>
Was &#x017F;ich mahlen la&#x0364;ßt, <hi rendition=\"#fr\">Bilder</hi>. Wie auf der<lb/>
weißen Wand der dunklen Kammer, &#x017F;o fa&#x0364;llt auf<lb/>
die Netzhaut des Auges ein Stralenpin&#x017F;el von<lb/>
allem, was vor ihm &#x017F;tehet, und kann nichts, als<lb/>
<fw type=\"sig\" place=\"bottom\">A 5</fw><fw type=\"catch\" place=\"bottom\">was</fw><lb/>
<pb n=\"10\" facs=\"#f0013\"/>
was da &#x017F;teht, eine Fla&#x0364;che, ein <hi rendition=\"#fr\">Nebeneinander</hi><lb/>
aller und der ver&#x017F;chieden&#x017F;ten &#x017F;ichtbaren Gegen&#x017F;ta&#x0364;nde<lb/>
zeichnen. Dinge <hi rendition=\"#fr\">hinter</hi> einander, oder &#x017F;olide,<lb/>
ma&#x017F;&#x017F;ive Dinge als &#x017F;olche dem Auge zu geben, i&#x017F;t<lb/>
&#x017F;o unmo&#x0364;glich, als den Liebhaber hinter der dicken<lb/>
Tapete, den Bauer innerhalb der Windmu&#x0364;hle<lb/>
&#x017F;ingend zu mahlen.</p><lb/>")
    (insert "\n  </text>\n</TEI>")
    ;; Return test buffer
    test-buffer))

(ert-deftest standoff-xml-toggle-char-ref-glyph-substitute-test ()
  "Testing the substitution of character references with glyphs."
  (let ((test-buffer (standoff-xml-test-setup-source-buffer)))
    (set-buffer test-buffer)
    (goto-char (point-min))
    (search-forward "&#" nil t)
    ;; should create an overlay at char ref at point
    (standoff-xml-toggle-char-ref-glyph-substitute 1)
    (should (= (length (overlays-at (point))) 1))
    ;; should remove this overlay again
    (standoff-xml-toggle-char-ref-glyph-substitute 0)
    (should (= (length (overlays-at (point))) 0))
    (standoff-test-utils-teardown-source-buffer test-buffer)
    ))

(ert-deftest standoff-xml-tags-invisible-test ()
  "Testing hide/show of tags."
  ;; Fixme: Write better testing conditions.
  :expected-result :failed
  (let ((test-buffer (standoff-xml-test-setup-source-buffer)))
    (set-buffer test-buffer)
    ;; should create an overlay for the tag at point
    (standoff-xml-tags-invisible 1)
    (should (> (length (overlays-in (point-min) (point-max))) 0))
    ;; should remove the overlay again
    (standoff-xml-tags-invisible 0)
    (should (= (length (overlays-in (point-min) (point-max))) 0))
    ;;(standoff-test-utils-teardown-source-buffer test-buffer)
    ))

(ert-deftest standoff-xml-tags-invisible-without-exception-test ()
  "Testing if `standoff-xml-tags-invisible' runs without exception."
  (let ((test-buffer (standoff-xml-test-setup-source-buffer)))
    (set-buffer test-buffer)
    ;; should create an overlay for the tag at point
    (standoff-xml-tags-invisible 1)
    (standoff-xml-tags-invisible 0)
    ))

;; run tests and exit
(when noninteractive
  (ert-run-tests-batch-and-exit (car argv)))

;;; standoff-xml-test.el ends here.
