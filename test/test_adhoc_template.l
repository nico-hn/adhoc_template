(require "adhoc-template")
(use-package '("user" "editor" "si" "misc-utils"))

(defun test-adhoc-template-convert()
  (let ((configs (misc-utils:file-to-string "../test_data/adhoc_template/config.txt"))
	(template (misc-utils:file-to-string "../test_data/adhoc_template/template.txt"))
	(expected-result (misc-utils:file-to-string "../test_data/adhoc_template/expected_result.txt")))
    (unless (string= expected-result (adhoc-template:convert template configs))
      (message-box (concat "NOT EXPECTED RESULT: " (adhoc-template:convert template configs))))))

(defun test-misc-utils-tsv-to-alists ()
  (let* ((tsv "first\tsecond\tthird
1\t2\t3
un\tdeux\ttrois
one\ttwo\tthree
")
	 (expected '((("first" . "1") ("second" . "2") ("third" . "3"))
		     (("first" . "un") ("second" . "deux") ("third" . "trois"))
		     (("first" . "one") ("second" . "two") ("third" . "three"))))
	 (alists (tsv-to-alists tsv)))
    (unless (equalp alists expected)
      (message-box (concat "test for tsv-to-alists failed:\n" alists)))))


(test-adhoc-template-convert)
(test-misc-utils-tsv-to-alists)