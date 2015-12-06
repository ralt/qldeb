(in-package #:qldeb)

(defun build-debian-package (env system)
  (debianize-system env system)
  (build-package env system)
  (copy-package env system))

(defun debianize-system (env system))

(defun build-package (env system)
  (uiop:chdir (merge-pathnames
               (uiop:strcat (ql-dist:prefix system) "/")
               (merge-pathnames #p"root/common-lisp" env)))
  (uiop:run-program "dpkg-buildpackage -us -uc"))

(defun copy-package (env system)
  (dolist (file (package-files system))
    (rename-file (merge-pathnames file
                                  (merge-pathnames #p"root/common-lisp/" env))
                 file)))

(defun package-files (system)
  (list
   ;; .deb
   (uiop:strcat (ql-dist:project-name system)
                "_" (ql-dist:version (ql-dist:dist system))
                "_amd64.deb")
   ;;; .changes
   (uiop:strcat (ql-dist:project-name system)
                "_" (ql-dist:version (ql-dist:dist system))
                "_amd64.changes")
   ;;; .dsc
   (uiop:strcat (ql-dist:project-name system)
                "_" (ql-dist:version (ql-dist:dist system))
                ".dsc")
   ;;; .tar.xz
   (uiop:strcat (ql-dist:project-name system)
                "_" (ql-dist:version (ql-dist:dist system))
                ".tar.xz")))
