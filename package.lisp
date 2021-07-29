;(ql:quickload "optima")
;(ql:quickload "alexandria")
(defpackage :cl-commonlisp-generator
  (:use :cl
	;:optima
	:alexandria)
  (:export
   #:write-source
  ))
