(require "misc-utils")

(defpackage "greps")
(in-package "greps")
(use-package '("user" "editor" "si" "misc-utils"))

(export '(*greps-config-file*
	  greps-with-config))

(defvar *greps-config-file* "greps-config.txt")

(defun read-regexps-config (config-file)
  (with-open-file (fp config-file)
    (let ((*standard-input* fp)
	  (configs '()))
      (each-line (line)
	(setq configs (cons (re-split-string line ":\t" nil :number-of-fields 2)
			    configs)))
      (reverse configs))))

;; (read-regexps-config "~/greps-config.txt")
;; "href=\"\\([^\"]+\\)\""

(defun greps-with-config (&optional (config-file *greps-config-file*))
  (let ((lines (buffer-substring (point-min) (point-max)))
	(configs (read-regexps-config config-file))
	(grep-target-name (buffer-name (selected-buffer))))
    (labels ((grep-with-config (config)
	       (let ((label (first config))
		     (regexp (second config)))
		 (insert label "\n ----------\n")
		 (each-line (line lines)
		   (when (string-match regexp line)
		     (insert line "\n"))))))
      (with-output-to-temp-buffer ("*greps-result*")
	(insert "Result of greps for " grep-target-name ":\n")
	(mapc #'grep-with-config configs)))))
