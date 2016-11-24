(defpackage "misc-utils")
(in-package "misc-utils")
(use-package '("user" "editor" "si"))

(export '(emptyp
	  make-string
	  each-line
	  join-string
	  re-split-string
	  integer-to-string
	  file-to-string
	  with-input-from
	  read-lines-from
	  tsv-to-alists
	  html-encode
	  read-content-of-files-in-directory))

(defun emptyp (arg)
  (and (or (stringp arg) (listp arg)) (zerop (length arg))))

(defun make-string (string-length)
  (make-vector string-length :element-type 'character :fill-pointer 0))

(defmacro each-line ((line &optional in) &rest body)
  (let ((lines (gensym))
	(input (gensym)))
    `(let ((,input ,in))
       (cond ((null ,input)
	      (setq ,lines *standard-input*))
	     ((stringp ,input)
	      (setq ,lines (make-string-input-stream ,input)))
	     (t
	      (setq ,lines ,input)))
       (do ((,line (read-line ,lines nil) (read-line ,lines nil)))
	   ((null ,line) nil)
	 ,@body))))

(setf (get 'each-line 'lisp-indent-hook) 'defun)

(defmacro with-input-from ((lines &optional in) &rest body)
  (let ((input (gensym)))
    `(let ((,input ,in))
      (cond ((null ,input)
	     (with-input-from-buffer ((selected-buffer))
	       (let ((,lines *standard-input*))
		 ,@body)))
	    ((stringp ,input)
	     (with-input-from-string (,lines ,input)
	       ,@body))
	    ((bufferp ,input)
	     (with-input-from-buffer (,input)
	       (let ((,lines *standard-input*))
		 ,@body)))))))

(setf (get 'with-input-from 'lisp-indent-hook) 'defun)

(defun join-string (strings &optional (separator ""))
  (let ((strings-with-separator (reduce #'(lambda (m str)
					   (cons str (cons separator m)))
				       (cdr strings)
				       :initial-value (list (car strings)))))
    (apply #'concat (reverse strings-with-separator))))

(defun re-split-string (str separator &optional (allow-empty nil) &key (number-of-fields nil))
  (do* ((result '() (cons (substring str head-index tail-index) result))
	(head-index 0 (+ tail-index (length (match-string 0))))
	(tail-index (string-match separator str head-index)
		    (string-match separator str head-index)))
      ((or (null tail-index)
	   (and number-of-fields (= (length result) (1- number-of-fields))))
       (if (string-not-equal (substring str head-index) "")
	   (push (substring str head-index) result))
       (if (= head-index (length str))
	   (push "" result))
       (unless allow-empty
	 (setq result (remove-if #'(lambda (field)
				     (string= field "")) result)))
       (reverse result))))

(defun integer-to-string (int)
  (format nil "~D" int))

(defun file-to-string (file-path)
  (with-open-file (stream file-path)
    (let ((file-content (make-string (file-length stream))))
      (read-into file-content stream)
      (convert-encoding-to-internal
       (or (detect-char-encoding file-content) *encoding-utf8n*)
				    file-content))))

(defun read-lines-from (input)
  (labels ((read-lines (in)
	     (do ((line (read-line in nil) (read-line in nil))
		  (lines '() (cons line lines)))
		 ((null line) (reverse lines)))))
    (with-input-from (lines input)
      (read-lines lines))))

(defun tsv-to-alists (tsv)
  (let* ((lines (read-lines-from tsv))
	 (field-names (split-string (car lines) #\tab t)))
    (mapcar #'(lambda (line)
		(mapcar #'cons field-names (split-string line #\tab t)))
	    (cdr lines))))

(defun html-encode (str)
  (let ((entities '(("&" . "&amp;")
		    (">" . "&gt;")
		    ("<" . "&lt;")
		    ("\"" . "&quot;")
		    ("'" . "&apos;")))
	(entity-re "\\([&<>\"']\\)"))
    (labels ((char-to-entity (chr)
	       (cdr (assoc chr entities :test 'equal))))
      (do ((result "" (concat result
			      (substring str prev-pos (match-beginning 0))
			      (char-to-entity (match-string 0))))
	   (prev-pos 0 (match-end 0)))
	  ((null (string-match entity-re str prev-pos))
	   (concat result (substring str prev-pos)))))))

(defun read-content-of-files-in-directory (directory-path &optional (file-pat nil))
  (labels ((keep-matching-files (files)
	     (if file-pat
		 (delete-if-not #'(lambda (file)
				    (string-match file-pat file))
				files)
	       files))
	   (file-content (file)
	     (file-to-string (merge-pathnames file directory-path))))
    (let ((files (keep-matching-files (directory directory-path))))
      (reduce #'(lambda (contents file)
		  (acons file (file-content file) contents))
	      files
	      :initial-value '()))))
