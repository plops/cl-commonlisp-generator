(ql:quickload "optima")
(ql:quickload "alexandria")
					;
;; (ql:quickload "cl-commonlisp-generator")
(in-package :cl-commonlisp-generator)
(setf (readtable-case *readtable*) :invert)

(defparameter *file-hashes* (make-hash-table))

(defun write-source (name code &optional (dir (user-homedir-pathname))
				 ignore-hash)
  (let* ((fn (merge-pathnames (format nil "~a.lisp" name)
			      dir))
	(code-str (emit-cl
		   :clear-env t
		   :code code))
	(fn-hash (sxhash fn))
	 (code-hash (sxhash code-str)))
    (multiple-value-bind (old-code-hash exists) (gethash fn-hash *file-hashes*)
     (when (or (not exists) ignore-hash (/= code-hash old-code-hash))
       ;; store the sxhash of the c source in the hash table
       ;; *file-hashes* with the key formed by the sxhash of the full
       ;; pathname
       (setf (gethash fn-hash *file-hashes*) code-hash)
       (with-open-file (s fn
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create)
	 (write-sequence code-str s))
       

       (sb-ext:run-program "/usr/local/bin/lisp-format" (list "-i" (namestring fn)))
       ))))

(defun print-sufficient-digits-f64 (f)
  "print a double floating point number as a string with a given nr. of                                                                                                                                             
  digits. parse it again and increase nr. of digits until the same bit                                                                                                                                              
  pattern."

  (let* ((a f)
         (digits 1)
         (b (- a 1)))
    (unless (= a 0)
      (loop while (< 1d-12
		     (/ (abs (- a b))
		       (abs a))
		    ) do
          (setf b (read-from-string (format nil "~,vG" digits a)))
           (incf digits)
	   ))
    (substitute #\e #\d (format nil "~,vG" digits a))))


;(print-sufficient-digits-f64 1d0)


(defparameter *env-functions* nil)
(defparameter *env-macros* nil)

(defun emit-cl (&key code (str nil) (clear-env nil) (level 0))
  ;(format t "emit ~a ~a~%" level code)
  (when clear-env
    (setf *env-functions* nil
	  *env-macros* nil))
  (flet ((emit (code &optional (dl 0))
	   (emit-cl :code code :clear-env nil :level (+ dl level))))
    (if code
	(if (listp code)
	    (case (car code)
	      (indent (format nil "~{~a~}~a"
			      (loop for i below level collect "    ")
			      (emit (cadr code))))
	      (comment (format nil "# ~a~%" (cadr code)))
	      (comments (let ((args (cdr code)))
			  (format nil "~{;;q ~a~%~}" args)))
	      (string (format nil "\"~a\"" (cadr code)))
	      (toplevel (let ((args (cdr code)))
			  (format nil "~{~a~%~}" (mapcar #'emit args))))
	      (do0
	       (let ((args (cdr code)))
		 args))

	      (setf (let ((args (cdr code)))
		      (format nil "(setf ~{~a~^~%~})"
			      #+nil (mapcar #'emit args)
			      
			      (loop for i below (length args) by 2 collect
								   (let ((a (elt args i))
									 (b (elt args (+ 1 i))))
								     (format nil "~a ~a" (emit a)
									     (emit b)))))))

	      (let (destructuring-bind (decls &rest body) (cdr code)
		     (with-output-to-string (s)
		       (format s "(let (")
		       (when decls
			 (format s "~{~a~^~%~})" (mapcar #'emit decls)))
		       (format s "~{~a~^~%~}" (mapcar #'emit body)))))
	      (let* (destructuring-bind (decls &rest body) (cdr code)
		     (with-output-to-string (s)
		       (format s "(let* (")
		       (when decls
			 (format s "~{~a~^~%~})" (mapcar #'emit decls)))
		       (format s "~{~a~^~%~}" (mapcar #'emit body)))))
	      (defun (destructuring-bind (name lambda-list &rest body) (cdr code)
		       (multiple-value-bind (req-param opt-param res-param
					     key-param other-key-p aux-param key-exist-p)
			   (parse-ordinary-lambda-list lambda-list)
			 (declare (ignorable req-param opt-param res-param
					     key-param other-key-p aux-param key-exist-p))
			 (with-output-to-string (s)
			  (format s "(defun ~a"
				  name
				  (append (mapcar #'emit req-param)
					  (loop for e in key-param
						collect 
						(destructuring-bind ((keyword-name name) init suppliedp)
						    e
						  (declare (ignorable keyword-name suppliedp))
						  (if init
						      `(= ,name ,init)
						      `(,name nil)))))
				  body)
			  (format s " (")
			  (format s "~{~a~^ ~}"
				  
				  (mapcar #'emit req-param)
				 
				  )
			  (when opt-param
			    (format s " &optional ~{~a~^ ~}"
				  
				 
				   (loop for e in opt-param
					 collect 
					 (destructuring-bind (name init suppliedp)
					     e
					   (declare (ignorable suppliedp))
					   (if init
					       `(,name ,init)
					       name)))
				   ))
			  (when key-param
			   (format s " &key ~{~a~^ ~}"
				  
				 
				   (loop for e in key-param
					 collect 
					 (destructuring-bind ((keyword-name name) init suppliedp)
					     e
					   (declare (ignorable keyword-name suppliedp))
					   (if init
					       `(,name ,init)
					       name)))
				   ))
			  (format s ")~%")
			  (format s "~{~a~^~%~})"
				  
				  body))
			 )))
	      
	      
	      (t (destructuring-bind (name &rest args) code
		   
		   (if (listp name)
		       ;; lambda call and similar complex constructs
		       (format nil "(~a)(~a)" (emit name) (if args
							      (emit `(paren ,@args))
							      ""))
		       #+nil(if (eq 'lambda (car name))
				(format nil "(~a)(~a)" (emit name) (emit `(paren ,@args)))
				(break "error: unknown call"))
		       ;; function call
		       (progn #+nil (member (first code) `(
					    + - * / mod rem incf decf
					    = /= < <= max min
					    and or not
					    logand logior logxor lognor logeqv
					    cond if when case
					    loop dotimes dolist ; do
					    block progn prog1
					    defun
					    atom equal eq eql evenp oddp
					    zerop null listp greaterp lessp
					    numberp symbolp integerp rationalp floatp
					    realp complexp characterp stringp
					    arrayp packagep
					    declare declaim
					    in-package
					    use-package
					    ))
			   
			   (progn ;; not common lisp
			     (let* ((positional (loop for i below (length args)
						      until
						      (keywordp (elt args i)) collect
						      (elt args i)))
				   (plist (subseq args (length positional)))
				    (props (loop for e in plist by #'cddr
						 collect e)))
			       (with-output-to-string (s)
				(format s "(~a ~{~a~^ ~}"
					name
					positional
					)
				(when props
				 (format s " ~{~a~^ ~}"
					 (loop for e in props appending
					       `( ,(format nil ":~a" e) ,(getf plist e)))))
				(format s ")"
					)))))))))
	    (cond
	      ((symbolp code) ;; print variable
	       (format nil "~a" code))
	      ((stringp code)
	       (format nil "~a" code)
	       #+nil
		(substitute #\: #\- (format nil "~a" code)))
	      ((numberp code) ;; print constants
	       (cond ((integerp code) (format str "~a" code))
		     ((floatp code)
		      (format str "(~a)" (print-sufficient-digits-f64 code)))
		     ((complexp code)
		      (format str "((~a) + 1j * (~a))"
			      (print-sufficient-digits-f64 (realpart code))
			      (print-sufficient-digits-f64 (imagpart code))))))))
	"")))

