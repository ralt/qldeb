(asdf:defsystem #:qldeb
  :description "Transforms all of quicklisp in debian packages."
  :author "Florian Margaine <florian@margaine.com>"
  :license "MIT License"
  :serial t
  :depends-on (:lparallel :drakma :quicklisp :uiop :sb-posix :ironclad)
  :components ((:file "package")
               (:file "qldeb")))
