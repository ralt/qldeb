(in-package #:qldeb)

(defun system-version (system)
  (format nil "~{~A~}" (uiop:split-string (ql-dist:version (ql-dist:dist system))
                                          :separator "-")))

(defun archive-name (release)
  (pathname
   (first
    (last
     (uiop:split-string (ql-dist:archive-url release) :separator "/")))))

(defun install-release (env release systems)
  (multiple-value-prog1 systems
    (let ((archive-url (ql-dist:archive-url release))
          (name (archive-name release)))
      (download-release env release (merge-pathnames name env) archive-url)
      (extract-release env name))))

(defun extract-release (env name)
  (format t "extracting ~A...~%" name)
  (uiop:run-program
   (format nil "tar xf ~A -C ~A"
           (namestring (merge-pathnames name env))
           (namestring env)))
  (format t "~A extracted~%" name))

(defun download-release (env release path url)
  (format t "downloading ~A...~%" url)
  (let ((archive (drakma:http-request url :force-binary t)))
    (unless (check-archive release archive)
      (format t "~A corrupted, trying again...~%" (namestring path))
      (return-from download-release (download-release env release path url)))
    (with-open-file (f path
                       :direction :output
                       :element-type '(unsigned-byte 8)
                       :if-does-not-exist :create)
      (write-sequence archive f))
    (format t "~A is downloaded to ~A~%" url (namestring path))))

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
