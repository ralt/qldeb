(in-package #:qldeb)

(defun build-debian-package (env system)
  (debianize-system env system)
  (build-package env system)
  (copy-package env system))

(defun debianize-system (env system)
  "
  Debianizing a system is just creating the debian/ folder and
  putting some generated files in there.
  "
  (format t "debianizing ~A...~%" (ql-dist:name system))
  (let* ((system-folder (merge-pathnames
                         (uiop:strcat (ql-dist:prefix system) "/")
                         (merge-pathnames #p"root/common-lisp/" env)))
         (debian-folder (merge-pathnames #p"debian/" system-folder))
         (system-file (read-from-string
                       (uiop:read-file-string
                        (merge-pathnames
                         (format nil "~A.asd"
                                 (ql-dist:system-file-name system))
                         system-folder)))))
    (ensure-directories-exist debian-folder)
    (maphash
     (lambda (filename template)
       (with-open-file (f (merge-pathnames filename debian-folder)
                          :direction :output
                          :if-does-not-exist :create)
         (format t "rendering ~A~%" (namestring
                                     (merge-pathnames filename debian-folder)))
         (write-sequence (funcall (getf template :lambda)
                                  system-folder system-file system) f)))
     *templates*)
    (format t "~A debianized.~%" (ql-dist:name system))))

(defun build-package (env system)
  (format t "building ~A...~%" (ql-dist:name system))
  (uiop:chdir (merge-pathnames
               (uiop:strcat (ql-dist:prefix system) "/")
               (merge-pathnames #p"root/common-lisp/" env)))
  ;; Don't sign the packages for now. We don't
  ;; yet know how to handle gpg prompt.
  (uiop:run-program "dpkg-buildpackage -us -uc")
  (format t "~A debian package built.~%" (ql-dist:name system)))

(defun copy-package (env system)
  (dolist (dest (package-files system))
    (let ((source (merge-pathnames dest
                                   (merge-pathnames #p"root/common-lisp/" env))))
      (format t "moving ~A to ~A~%" source dest)
      (rename-file source dest))))

(defun package-files (system)
  (let ((name (ql-dist:name system))
        (version (system-version system)))
    (list
     (format nil "~A_~A_amd64.deb" name version)
     (format nil "~A_~A_amd64.changes" name version)
     (format nil "~A_~A.dsc" name version)
     (format nil "~A_~A.tar.xz" name version))))
