(require "adhoc-template")
(use-package '("user" "editor" "si" "misc-utils"))

(defun test-adhoc-template-convert()
  (let ((configs (misc-utils:file-to-string "../test_data/adhoc_template/config.txt"))
	(template (misc-utils:file-to-string "../test_data/adhoc_template/template.txt")))
    (with-output-to-temp-buffer ("*tmp*")
      (princ (adhoc-template:convert template configs)))))

(test-adhoc-template-convert)

