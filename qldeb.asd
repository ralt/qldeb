(asdf:defsystem #:qldeb
  :description "Transforms all of quicklisp in debian packages."
  :author "Florian Margaine <florian@margaine.com>"
  :license "MIT License"
  :serial t
  :depends-on (:lparallel
               :drakma
               :quicklisp
               :uiop
               :ironclad
               :local-time
               :cl-ppcre
               :archive
               :deb-packager)
  :components ((:file "package")
               (:file "deb-packager")
               (:file "debian-package")
               (:file "release")
               (:file "qldeb")))
