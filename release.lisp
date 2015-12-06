(in-package #:qldeb)

(defun releases-objects (dist)
  ;;; Returns a list built like this:
  ;;; ((<QL-DIST:RELEASE {1}> (<QL-DIST:SYSTEM {1a}> <QL-DIST:SYSTEM {1b}> ...))
  ;;;  (<QL-DIST:RELEASE {2}> (<QL-DIST:SYSTEM {2a}> <QL-DIST:SYSTEM {2b}> ...)))
  (loop for release in (ql-dist:provided-releases dist)
     collect (cons release (list (find-systems dist release)))))

(defun find-systems (dist release)
  (remove-if-not (lambda (systems)
                   (equal (ql-dist:release systems) release))
                 (ql-dist:provided-systems dist)))

(defun archive-name (release)
  (pathname
   (first
    (last
     (uiop:split-string (ql-dist:archive-url release) :separator "/")))))

(defun install-release (env release-object)
  (multiple-value-prog1 (second release-object)
    (let* ((release (first release-object))
           (archive-url (ql-dist:archive-url release))
           (name (archive-name release)))
      (download-release (merge-pathnames name env) archive-url)
      (extract-release env name))))

(defun extract-release (env name)
  (uiop:run-program
   (format nil "tar xf ~A -C ~A"
           (merge-pathnames name env)
           (merge-pathnames name #p"/root/common-lisp/"))))

(defun download-release (path url)
  (with-open-file (f path
                     :direction :output
                     :element-type '(unsigned-byte 8)
                     :if-does-not-exist :create)
    (format t "downloading ~A...~%" url)
    (let ((archive (drakma:http-request archive-url :force-binary t)))
      (unless (check-archive release archive)
        (format t "~A corrupted, trying again...~%" (namestring path))
        (return-from download-release (download-release env release-object)))
      (write-sequence archive f)
      (format t "~A is downloaded to ~A~%" url (namestring path)))))

(defun check-archive (release archive)
  (and (check-archive-sha1 (ql-dist:archive-content-sha1 release) archive)
       (check-archive-md5 (ql-dist::archive-md5 release) archive)
       (check-archive-size (ql-dist:archive-size release) archive)))

(defun sum (digest value)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence digest value)))

(defun check-archive-sha1 (sum archive)
  (string= sum (sum :sha1 archive)))

(defun check-archive-md5 (sum archive)
  (string= sum (sum :md5 archive)))

(defun check-archive-size (size archive)
  (= (length archive) size))
