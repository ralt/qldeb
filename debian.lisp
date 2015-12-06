(in-package #:qldeb)

(defun build-debian-package (env system)
  (debianize-system env system)
  (build-package env system)
  (copy-package env system))

(defun debianize-system (env system))

(defun build-package (env system))

(defun copy-package (env system)
  (let ((package (package-filename system)))
    (rename-file (merge-pathnames package env) package)))

(defun package-filename (system)
  (uiop:strcat (ql-dist:project-name system)
               "-" (ql-dist:version (ql-dist:dist system))
               "_amd64.deb"))
