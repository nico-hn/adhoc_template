(defpackage "adhoc-template")
(in-package "adhoc-template")
(use-package '("user" "editor" "misc-utils"))

(defun read-config-block (block-head in)
  (labels ((collect-lines (in)
	     (let ((lines '()))
	       (catch 'block-end
		 (each-line (line in)
		   (if (string-looking-at "//@" line)
		       (throw 'block-end (list lines line)))
		   (push line lines))
		 (list lines nil)))))
    (let ((config (list (substring block-head 3)))
	  (lines-and-next-head (collect-lines in)))
      (setf (cdr config) (join-string (reverse (car lines-and-next-head)) "\n"))
      (values config (cadr lines-and-next-head)))))

(defun read-configs (in)
  (labels ((line-to-config (line)
	     (let ((lst (re-split-string line ":[ \t]?" t :number-of-fields 2)))
	       (cons (car lst) (cadr lst)))))
    (let ((configs '())
	  config
	  block-head)
      (each-line (line in)
	(cond ((emptyp line)
	       nil)
	      ((string-looking-at "//@" line)
	       (setq block-head line)
	       (while block-head
		 (multiple-value-setq (config block-head)
		   (read-config-block block-head in))
		 (push config configs)))
	      (t
	       (push (line-to-config line) configs))))
      configs)))


