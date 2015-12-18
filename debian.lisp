(in-package #:qldeb)

(defun build-debian-package (env system cwd)
  (debianize-system env system)
  (build-package env system)
  (copy-package env system cwd))

(defun get-asd-form (system asd-file)
  "
  An asd file can have multiple systems in it.
  "
  (find-if (lambda (form)
             (when (and (second form)
                        (symbolp (second form)))
               (string= (ql-dist:name system)
                        ;; The system name
                        (string-downcase (symbol-name (second form))))))
           (uiop:read-file-forms asd-file)))

(defun debianize-system (env system)
  "
  Debianizing a system is just creating the debian/ folder and
  putting some generated files in there.
  "
  (format t "debianizing ~A...~%" (ql-dist:name system))
  (let* ((system-folder (merge-pathnames
                         (uiop:strcat (ql-dist:prefix (ql-dist:release system)) "/")
                         env))
         (debian-folder (merge-pathnames #p"debian/" system-folder))
         (system-form (get-asd-form system
                                    (merge-pathnames
                                     (format nil "~A.asd"
                                             (ql-dist:system-file-name system))
                                     system-folder))))
    ;; A previous debianizing might have already run.
    ;; The clean way would be to generate a single control file and
    ;; a .install per system, but meh.
    (when (probe-file debian-folder)
      (uiop:delete-directory-tree debian-folder :validate t))
    (ensure-directories-exist debian-folder)
    (maphash
     (lambda (filename template)
       (let ((path (merge-pathnames filename debian-folder)))
         (with-open-file (f path
                            :direction :output
                            :if-does-not-exist :create)
           (format t "rendering ~A...~%" path)
           (write-sequence (funcall (getf template :lambda)
                                    system-folder system-form system) f)
           (format t "~A rendered.~%" path))))
     *templates*)
    (format t "~A debianized.~%" (ql-dist:name system))))

(defun build-package (env system)
  (format t "building ~A...~%" (ql-dist:name system))
  (uiop:chdir (merge-pathnames
               (uiop:strcat (ql-dist:prefix (ql-dist:release system)) "/")
               env))
  ;; Don't sign the packages for now. We don't
  ;; yet know how to handle gpg prompt.
  (uiop:run-program "dpkg-buildpackage -us -uc -Zgzip")
  (format t "~A debian package built.~%" (ql-dist:name system)))

(defun copy-package (env system cwd)
  (dolist (dest (package-files system))
    (let ((source (merge-pathnames dest env)))
      (format t "moving ~A to ~A~%" source (merge-pathnames dest cwd))
      (rename-file source (merge-pathnames dest cwd)))))

(defun package-files (system)
  (let ((name (ql-dist:name system))
        (version (system-version system)))
    (list
     (format nil "~A_~A_amd64.deb" name version)
     (format nil "~A_~A_amd64.changes" name version)
     (format nil "~A_~A.dsc" name version)
     (format nil "~A_~A.tar.gz" name version))))
