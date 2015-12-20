(in-package #:qldeb)

;; Bugfix for gzip-stream:gzip-input-stream
(defclass gzip-input-stream (gzip-stream:gzip-input-stream)
  ((gzip-stream::read-buffer :accessor gzip-stream::read-buffer-of :initform
                             (flexi-streams:make-in-memory-input-stream
                              (make-array 0 :element-type '(unsigned-byte 8))))))

(defun gunzipped-stream (sequence)
  (make-instance
   'gzip-input-stream
   :understream (flexi-streams:make-in-memory-input-stream
                 (coerce sequence '(vector (unsigned-byte 8))))))

(defun make-debian-package (archive-array system)
  (let* ((archive (archive:open-archive
                   'archive:tar-archive
                   (gunzipped-stream archive-array)))
         (package (make-deb-packager archive system))
         (data-files (make-data-files archive system)))
    (multiple-value-prog1 package
      (deb-packager:initialize-control-files package #())
      (deb-packager:initialize-data-files package data-files)
      (archive:close-archive archive))))
