(in-package #:qldeb)

(defun download-and-package (release)
  (let ((archive (download-release release)))
    (dolist (system (release-systems release))
      (log :info "building debian package for ~A...~%" (ql-dist:name system))
      (let ((package (handler-case
                         (make-debian-package archive system)
                       (error (e) (progn
                                    (log :err "error generating package ~A: ~A~%"
                                         (ql-dist:name system) e)
                                    (return-from download-and-package))))))
        (log :info "debian package for ~A built.~%" (ql-dist:name system))
        (handler-case
            (progn
              (deb-packager:write-deb-file (deb-packager::package-pathname package)
                                           package)
              (log :info "~A written to disk.~%"
                   (deb-packager::package-pathname package)))
          (error (e) (log :err "error writing ~A: ~A~%"
                          (deb-packager::package-pathname package) e)))))))

(defun release-systems (release)
  "Fetches all the systems provided by a release."
  (remove-if-not (lambda (system)
                   (eq (ql-dist:release system) release))
                 (ql-dist:provided-systems (ql-dist:dist release))))

(defun download-release (release)
  (let ((url (ql-dist:archive-url release)))
    (log :info "downloading ~A...~%" url)
    (let ((archive (drakma:http-request url :force-binary t)))
      (unless (check-archive release archive)
        (log :debug "~A download corrupted, trying again...~%" url)
        (return-from download-release (download-release release)))
      (log :info "~A's archive is downloaded~%" (ql-dist:project-name release))
      archive)))

(defun check-archive (release archive)
  (and (check-archive-md5 (ql-dist::archive-md5 release) archive)
       (check-archive-size (ql-dist:archive-size release) archive)))

(defun sum (digest value)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence digest value)))

(defun check-archive-md5 (sum archive)
  (string= sum (sum :md5 archive)))

(defun check-archive-size (size archive)
  (= (length archive) size))
