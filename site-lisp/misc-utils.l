(defpackage "misc-utils")
(in-package "misc-utils")
(use-package '("user" "editor" "si"))

(export '(emptyp
	  make-string
	  each-line
	  join-string
	  re-split-string
	  integer-to-string
	  file-to-string))

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
    (let ((file-content (make-vector (file-length stream) :element-type 'character :fill-pointer 0)))
      (read-into file-content stream))))
