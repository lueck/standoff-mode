:; exec emacs -Q --script "$0" -- "$@"

;;; Commentary:
;; WARNING: Close your production files before running tests! Restart
;; Emacs before going from testing to production again, because it is
;; not shure, that all configuration is restored correctly. You might
;; loose data! -- OR BETTER: Run this test file in batch mode.

;;; Code:

(defvar standoff-json-test-debug nil
  "Set this to non-nil, if you would like to get various messages.")

(defvar standoff-json-test-kill-buffers t
  "Kill buffers on tear down.")

(when noninteractive
  ;; add . and .. to load path
  (push (concat (file-name-directory load-file-name) "/..") load-path)
  (push (file-name-directory load-file-name) load-path)
  ;; pop "--" from argv
  (setq argv (cdr argv))
  )

(require 'ert)
(require 'standoff-test-utils)
(require 'standoff-json-file)

;;; Functions for set up and tear down

(defun standoff-json-test-setup-source-buffer ()
  "Save the current configuration and setup testing environment.
Returns a test buffer with some content."
  (let ((test-buffer (generate-new-buffer "json-test")))
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
    ;; Return test buffer
    test-buffer))

(defun standoff-json-test-teardown (buf)
  "Tear down the test setup for dummy testing."
  (let ((json-buf (standoff-json-file/get-json-buffer buf)))
    (when standoff-json-test-kill-buffers (kill-buffer json-buf))
    (kill-buffer buf)
    (standoff-test-utils-restore-old-config)))

(defun standoff-json-test-positions (buf)
  (when standoff-json-test-debug
    (with-current-buffer buf
      (message "JSON Position:")
      (message "MD5 sum: %s"
	       (standoff-json-file/get-or-parse-position "md5sum-start"))
      (message "Markup: %s %s"
	       (standoff-json-file/get-or-parse-position "MarkupRanges-start")
	       (standoff-json-file/get-or-parse-position "MarkupRanges-insert"))
      )))

(defun standoff-json-test-json-buffer-print (buf)
  (when standoff-json-test-debug
    (with-current-buffer buf
      (message "JSON buffer:\n%s" (buffer-substring (point-min) (point-max))))))

;;; Tests

(ert-deftest standoff-json-test-get-json-buffer ()
  (let ((source-buffer (standoff-json-test-setup-source-buffer))
	(json-buffer nil)
	(markup-id nil))
    (set-buffer source-buffer)
    (setq json-buffer (standoff-json-file/get-json-buffer source-buffer))
    ;; The buffer is not nil
    (should json-buffer)
    ;; still the same buffer
    (should (equal json-buffer (standoff-json-file/get-json-buffer source-buffer)))
    (standoff-json-test-teardown source-buffer)))

(ert-deftest standoff-json-test-create-read-markup ()
  (let ((source-buffer (standoff-json-test-setup-source-buffer))
	json-buffer
	json-buffer-size1
	json-buffer-size2
	markup-id1
	markup-id2
	markup-id3
	markup-id4)
    (set-buffer source-buffer)
    (setq json-buffer (standoff-json-file/get-json-buffer source-buffer))
    (standoff-json-test-positions json-buffer)
    (setq json-buffer-size1 (buffer-size json-buffer))
    ;; reading markup before any was created
    (should (equal '() (standoff-json-file/read-markup source-buffer)))
    ;; create markup elements
    (setq markup-id1 (standoff-json-file/create-markup source-buffer 23 42 "example"))
    (standoff-json-test-positions json-buffer)
    (should (> (buffer-size json-buffer) json-buffer-size1))
    (setq markup-id2 (standoff-json-file/create-markup source-buffer 52 64 "marker"))
    (standoff-json-test-positions json-buffer)
    ;; Read
    (setq ranges (standoff-json-file/read-markup source-buffer))
    (should (= 2 (length ranges)))
    (should (= 2 (length (standoff-json-file/read-markup source-buffer nil nil nil nil))))
    (should (= 2 (length (standoff-json-file/read-markup source-buffer nil nil nil))))
    (should (= 2 (length (standoff-json-file/read-markup source-buffer nil nil))))
    (setq ranges (standoff-json-file/read-markup source-buffer nil nil "example"))
    (should (= 1 (length ranges)))
    (setq ranges (standoff-json-file/read-markup source-buffer nil nil nil markup-id2))
    (should (= 1 (length ranges)))
    (setq ranges (standoff-json-file/read-markup source-buffer 51 65))
    (should (= 1 (length ranges)))
    (setq ranges (standoff-json-file/read-markup source-buffer 1 19 "example"))
    (should (= 0 (length ranges)))
    (should-error (standoff-json-file/read-markup source-buffer 1))
    (should-error (standoff-json-file/read-markup source-buffer nil 2))
    ;; Add range (discontinous markup)
    (setq markup-id3 (standoff-json-file/add-range source-buffer 67 68 markup-id2))
    (should (equal markup-id2 markup-id3))
    ;; Read again
    (should (= 3 (length (standoff-json-file/read-markup source-buffer))))
    (should (= 2 (length (standoff-json-file/read-markup source-buffer nil nil nil markup-id2))))
    ;; Try adding range with unknow element id
    (setq markup-id4 (standoff-util/create-uuid))
    (should-error (standoff-json-file/add-range source-buffer 67 68 markup-id4))
    ;; delete last one
    (should (equal t (standoff-json-file/delete-range source-buffer 67 68 "marker" markup-id2)))
    (standoff-json-test-json-buffer-print json-buffer)
    ;; Read again
    (should (= 2 (length (standoff-json-file/read-markup source-buffer))))
    (standoff-json-test-positions json-buffer)
    ;; add range again
    (setq markup-id3 (standoff-json-file/add-range source-buffer 107 109 markup-id1))
    ;; read again
    (should (= 3 (length (standoff-json-file/read-markup source-buffer))))
    (standoff-json-test-json-buffer-print json-buffer)
    ;; delete the first one
    (should (equal t (standoff-json-file/delete-range source-buffer 23 42 "example" markup-id1)))
    (standoff-json-test-json-buffer-print json-buffer)
    ;; read again
    (should (= 2 (length (standoff-json-file/read-markup source-buffer))))
    ;; get types
    (should (= 2 (length (standoff-json-file/markup-types source-buffer))))
    (should (member "example" (standoff-json-file/markup-types source-buffer)))
    (should (member "marker" (standoff-json-file/markup-types source-buffer)))
    ;; tear down
    (standoff-json-test-teardown source-buffer)))

(ert-deftest standoff-json-test-create-read-relation ()
  (let ((source-buffer (standoff-json-test-setup-source-buffer))
	json-buffer
	json-buffer-size1
	json-buffer-size2
	markup-id1
	markup-id2
	markup-id3
	markup-id4
	relation-id1
	relation-id2)
    (set-buffer source-buffer)
    (setq json-buffer (standoff-json-file/get-json-buffer source-buffer))
    ;; read relation before there are any
    (should (= 0 (length (standoff-json-file/read-relations source-buffer))))
    ;; create markup elements
    (setq markup-id1 (standoff-json-file/create-markup source-buffer 23 42 "example"))
    (setq markup-id2 (standoff-json-file/create-markup source-buffer 52 64 "marker"))
    (setq markup-id3 (standoff-json-file/create-markup source-buffer 45 50 "example"))
    ;; create relations
    (setq relation-id1 (standoff-json-file/create-relation source-buffer markup-id1 "hatMarker" markup-id2))
    (should (= 1 (length (standoff-json-file/read-relations source-buffer))))
    (setq relation-id2 (standoff-json-file/create-relation source-buffer markup-id2 "markiert" markup-id1))
    (should (= 2 (length (standoff-json-file/read-relations source-buffer))))
    (should (= 1 (length (standoff-json-file/read-relations source-buffer markup-id1))))
    (should (= 1 (length (standoff-json-file/read-relations source-buffer markup-id2))))
    (should (= 1 (length (standoff-json-file/read-relations source-buffer nil nil markup-id2))))
    (should (= 1 (length (standoff-json-file/read-relations source-buffer nil "markiert"))))
    ;; As long as used-predicates fails, it is manteled in its own test
    ;; (should (= 1 (length (standoff-json-file/used-predicates source-buffer markup-id1 markup-id2))))
    
    ;; tear down
    (standoff-json-test-teardown source-buffer)))

(ert-deftest standoff-json-test-used-predicates ()
  :expected-result :failed
  (let ((source-buffer (standoff-json-test-setup-source-buffer))
	json-buffer
	json-buffer-size1
	json-buffer-size2
	markup-id1
	markup-id2
	markup-id3
	markup-id4
	relation-id1
	relation-id2)
    (set-buffer source-buffer)
    (setq json-buffer (standoff-json-file/get-json-buffer source-buffer))
    (setq markup-id1 (standoff-json-file/create-markup source-buffer 23 42 "example"))
    (setq markup-id2 (standoff-json-file/create-markup source-buffer 52 64 "marker"))
    (setq markup-id3 (standoff-json-file/create-markup source-buffer 45 50 "example"))
    (setq relation-id1 (standoff-json-file/create-relation source-buffer markup-id1 "hatMarker" markup-id2))
    (setq relation-id2 (standoff-json-file/create-relation source-buffer markup-id2 "markiert" markup-id1))
    ;; FIXME!
    (should (= 1 (length (standoff-json-file/used-predicates source-buffer markup-id1 markup-id2))))
    ;;(message "%s" (standoff-json-file/used-predicates source-buffer markup-id2 markup-id1))
    ;; tear down
    (standoff-json-test-teardown source-buffer)))
    


(ert-deftest standoff-json-test-create-read-literal ()
  (let ((source-buffer (standoff-json-test-setup-source-buffer))
	json-buffer
	json-buffer-size1
	json-buffer-size2
	markup-id1
	markup-id2
	markup-id3
	markup-id4
	literal-id1
	literal-id2)
    (set-buffer source-buffer)
    (setq json-buffer (standoff-json-file/get-json-buffer source-buffer))
    ;; read literal before there are any
    (should (= 0 (length (standoff-json-file/read-literals source-buffer))))
    ;; create markup elements
    (setq markup-id1 (standoff-json-file/create-markup source-buffer 23 42 "example"))
    (setq markup-id2 (standoff-json-file/create-markup source-buffer 52 64 "marker"))
    (setq markup-id3 (standoff-json-file/create-markup source-buffer 45 50 "example"))
    ;; create literals
    (setq literal-id1 (standoff-json-file/create-literal source-buffer markup-id1 "Paraphrase" "id"))
    (should (= 1 (length (standoff-json-file/read-literals source-buffer))))
    (setq literal-id2 (standoff-json-file/create-literal source-buffer markup-id2 "Name" "Dido"))
    (should (= 2 (length (standoff-json-file/read-literals source-buffer))))
    (should (= 1 (length (standoff-json-file/read-literals source-buffer markup-id1))))
    (should (= 1 (length (standoff-json-file/read-literals source-buffer markup-id2))))
    (should (= 1 (length (standoff-json-file/read-literals source-buffer nil "Paraphrase"))))
    (should (= 2 (length (standoff-json-file/read-literals source-buffer nil nil nil "id"))))
    (should (= 1 (length (standoff-json-file/read-literals source-buffer nil nil nil "Di"))))
    ;; tear down
    (standoff-json-test-teardown source-buffer)))
    


;; run tests and exit
(when noninteractive
  (ert-run-tests-batch-and-exit (car argv)))

;;; standoff-json-file-test.el ends here
