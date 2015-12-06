(in-package #:qldeb)

(defun build-debian-package (env system)
  (debianize-system env system)
  (build-package env system)
  (copy-package env system))

(defun debianize-system (env system)
  "Debianizing a system is just creating the debian/ folder and
put some generated files in there."
  (let* ((system-folder (merge-pathnames
                         (uiop:strcat (ql-dist:prefix system) "/")
                         (merge-pathnames #p"root/common-lisp/" env)))
         (debian-folder (merge-pathnames #p"debian/" system-folder)))
    (loop for file in `((,#'control-file . "control")
                        (,#'changelog-file . "changelog")
                        (,#'compat-file . "compat")
                        (,#'copyright-file . "copyright")
                        (,#'rules-file . "rules"))
       do (with-open-file (f (merge-pathnames (cdr file) debian-folder)
                             :direction :output
                             :if-does-not-exist :create)
            (write-sequence (funcall (car file) system) f)))))

(defun build-package (env system)
  (uiop:chdir (merge-pathnames
               (uiop:strcat (ql-dist:prefix system) "/")
               (merge-pathnames #p"root/common-lisp/" env)))
  ;; Don't sign the packages for now.
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
