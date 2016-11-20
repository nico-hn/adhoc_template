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

(defun test-misc-utils-read-lines-from ()
  (let ((lines (read-lines-from "first\tsecond\tthird
1\t2\t3
un\tdeux\ttrois
one\ttwo\tthree
"))
	(expected '("first\tsecond\tthird" "1\t2\t3" "un\tdeux\ttrois" "one\ttwo\tthree")))
    (unless (equalp lines expected)
      (message-box (concat "test for read-lines-from failed:\n" lines)))))

(defun test-adhoc-template-convert-tsv ()
  (let ((tsv (misc-utils:file-to-string "../test_data/adhoc_template/tsv.txt"))
	(template (misc-utils:file-to-string "../test_data/adhoc_template/template_for_tsv.txt"))
	(expected-result (misc-utils:file-to-string "../test_data/adhoc_template/expected_result_for_tsv.txt")))
    (unless (string= expected-result (adhoc-template:convert-tsv template tsv))
      (message-box (concat "NOT EXPECTED RESULT: " (adhoc-template:convert-tsv template tsv))))))

(test-adhoc-template-convert)
(test-misc-utils-tsv-to-alists)
(test-misc-utils-read-lines-from)
(test-adhoc-template-convert-tsv)
