(require "adhoc-template")
(use-package '("user" "editor" "si" "misc-utils"))

(defun test-adhoc-template-convert()
  (let ((configs (misc-utils:file-to-string "../test_data/adhoc_template/config.txt"))
	(template (misc-utils:file-to-string "../test_data/adhoc_template/template.txt"))
	(expected-result (misc-utils:file-to-string "../test_data/adhoc_template/expected_result.txt")))
    (unless (string= expected-result (adhoc-template:convert template configs))
      (message-box (concat "NOT EXPECTED RESULT: " (adhoc-template:convert template configs))))))

(test-adhoc-template-convert)

