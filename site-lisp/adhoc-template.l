(require "misc-utils")

(defpackage "adhoc-template")
(in-package "adhoc-template")
(use-package '("user" "editor" "misc-utils"))

(export '(read-configs
	  parse
	  format-tree
	  convert
	  convert-tsv
	  eval-current-buffer-as-config
	  *adhoc-template-formatters*))

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
	       (cons (car lst) (cadr lst))))
	   (prepare-config-stream (config-data)
	     (if (stringp config-data)
		 (make-string-input-stream config-data)
	       config-data)))
    (let ((configs '())
	  (in (prepare-config-stream in))
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

(defun split-into-tokens (in)
  (let* ((tag-sets '((#\< #\%)
		     (#\% #\>)))
	 (tag-heads (mapcar #'car tag-sets))
	 (last-index (1- (length in))))
    (labels ((tagp (in tag-head-index)
	       (and tag-head-index
		    (< tag-head-index last-index)
		    (char= (elt in (1+ tag-head-index)) (cadr (assoc (elt in tag-head-index) tag-sets)))))
	     (tag-head-pos (in head-index)
	       (position-if #'(lambda (chr)
				(member chr tag-heads)) in :start head-index))
	     (tag-pos (in head-index)
	       (do ((tag-head-index (tag-head-pos in head-index) (tag-head-pos in (1+ tag-head-index))))
		   ((or (and tag-head-index (tagp in tag-head-index)) (null tag-head-index))
		    tag-head-index)))
	     (read-token (in head-index tokens)
	       (let* ((tag-head-index (tag-pos in head-index))
		      (tag (and (tagp in tag-head-index) (subseq in tag-head-index (+ tag-head-index 2)))))
		 (if tag
		     (read-token in (+ tag-head-index 2)
				 (cons tag (cons (substring in head-index tag-head-index) tokens)))
		   (cons (substring in head-index) tokens)))))
      (nreverse (read-token in 0 '())))))

;;
;;another version of split-into-tokens using regular expression
;;
;;(defun split-into-tokens (in)
;;  (let ((tag-pat "\\(<%\\|%>\\)"))
;;    (do* ((tag-head-index (string-match tag-pat in) (string-match tag-pat in tag-tail-index))
;;	  (last-tag-tail-index 0 tag-tail-index)
;;	  (tag-tail-index (match-end 0) (match-end 0))
;;	  (tokens (if tag-head-index
;;		      (list (substring in tag-head-index tag-tail-index)
;;			    (substring in 0 tag-head-index))
;;		    (list (substring in 0 tag-head-index)))
;;		  (if tag-head-index
;;		      (cons (substring in tag-head-index tag-tail-index)
;;			    (cons (substring in last-tag-tail-index tag-head-index) tokens))
;;		    (cons (substring in last-tag-tail-index) tokens))))
;;	((null tag-head-index) (nreverse tokens)))))

(defun parse (in)
  (let ((begin-tag "<%")
	(end-tag "%>"))
    (labels ((read-string-part (parsed-list tokens)
	       (if (string= (car tokens) begin-tag)
		   (read-embedded-part parsed-list (cdr tokens))
		 (do ((token (car tokens) (car rest-tokens))
		      (rest-tokens (cdr tokens) (cdr rest-tokens))
		      (lst parsed-list (cons token lst)))
		     ((or (null rest-tokens) (string= token begin-tag))
		      (if (null rest-tokens)
			  (cons token lst)
			(read-embedded-part lst rest-tokens))))))
	     (read-embedded-part (parsed-list tokens)
	       (do ((token (car tokens) (car rest-tokens))
		    (rest-tokens (cdr tokens) (cdr rest-tokens))
		    (embedded-part '() (cons token embedded-part)))
		   ((or (null rest-tokens) (string= token end-tag))
		    (if (null rest-tokens)
			(cons embedded-part parsed-list)
		      (read-string-part (cons embedded-part parsed-list) rest-tokens))))))
      (read-string-part '() (split-into-tokens in)))))

(defun default-eval-variable (var configs)
  (let ((val (assoc var configs :test 'equal)))
    (or (and val (cdr val)) var)))

(defun eval-variable-with-html-encode (var configs)
  (let ((val (assoc var configs :test 'equal)))
    (or (and val (html-encode (cdr val))) var)))

(defun eval-partial-template (var configs)
  (labels ((find-value (name)
	     (let ((config (assoc name configs :test 'equal)))
	       (and config (cdr config))))
	   (read-content (content-name)
	     (and content-name (read-into-from-buffer-or-file content-name)))
	   (choose-converter (configs-name)
	     (if (and configs-name (string-match "\.\\(tsv\\|TSV\\)$" configs-name))
		 #'convert-tsv
	       #'convert)))
    (let ((template-name (find-value var))
	  (configs-name (find-value (concat var ">"))))
      (if template-name
	  (funcall (choose-converter configs-name)
		   (or (read-content template-name) template-name)
		   (or (read-content configs-name) configs))
	var))))

(defvar *adhoc-template-formatters* '(("=" . default-eval-variable)
				      ("h" . eval-variable-with-html-encode)
				      (">" . eval-partial-template)))

(defun choose-formatter (function-id)
  (let ((fn-pair (assoc function-id *adhoc-template-formatters* :test 'string=)))
    (if fn-pair
	(cdr fn-pair)
      'default-eval-variable)))

(defun function-id-and-var (tag)
  (values-list (re-split-string (car tag) "[ \t]+")))

(defun eval-tag (tag configs)
  (multiple-value-bind (function-id var)
      (function-id-and-var tag)
    (funcall (choose-formatter function-id) var configs)))

(defun format-tree (tree configs &optional cur-str)
  (let ((cur-str (or cur-str "")))
    (cond ((null tree)
	   cur-str)
	  ((consp (car tree))
	   (format-tree (cdr tree) configs
			(concat (eval-tag (car tree) configs) cur-str)))
	  ((stringp (car tree))
	   (format-tree (cdr tree) configs
			(concat (car tree) cur-str))))))

(defun convert (template-data config-data)
  (let ((configs (read-configs config-data)))
      (format-tree (parse template-data) configs)))

(defun convert-tsv (template-data tsv-data)
  (let ((tsv-records (tsv-to-alists tsv-data))
	(template (parse template-data)))
    (join-string (mapcar #'(lambda (tsv-record)
			     (format-tree template tsv-record))
			 tsv-records))))

(defun eval-current-buffer-as-config ()
  (interactive)
  (labels ((choose-template ()
	     (if (y-or-n-p "Read template from buffer? ")
		 (buffer-name (read-buffer-name "Template buffer name: "))
	       (read-file-name "Template file name: ")))
	   (tsv-data? ()
	     (string-match "\.\\(tsv\\|TSV\\)$" (buffer-name (selected-buffer)))))
    (let ((config-data (buffer-substring (point-min) (point-max)))
	  (template-data (read-into-from-buffer-or-file (choose-template)))
	  (converter (or (and (tsv-data?) #'convert-tsv) #'convert)))
      (with-output-to-temp-buffer ("*adhoc-template: result*")
	(insert (funcall converter template-data config-data))))))
