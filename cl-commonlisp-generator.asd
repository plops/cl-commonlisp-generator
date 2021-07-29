(asdf:defsystem cl-py-generator
    :version "0"
    :description "Emit Common Lisp code"
    :maintainer " <kielhorn.martin@gmail.com>"
    :author " <kielhorn.martin@gmail.com>"
    :licence "GPL"
    :depends-on ("alexandria")
    :serial t
    :components ((:file "package")
		 (:file "cl")) )
