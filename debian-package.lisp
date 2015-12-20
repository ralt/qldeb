(in-package #:qldeb)

;; Bugfix for gzip-stream:gzip-input-stream
(defclass gzip-input-stream (gzip-stream:gzip-input-stream)
  ((gzip-stream::read-buffer :accessor gzip-stream::read-buffer-of :initform
                             (flexi-streams:make-in-memory-input-stream
                              (make-array 0 :element-type '(unsigned-byte 8))))))

(defmacro with-targz-archive (vars &body body)
  `(let ((,(first vars) (archive:open-archive
                         'archive:tar-archive
                         (gunzipped-stream ,(second vars)))))
     (multiple-value-prog1 ,@body
       (archive:close-archive ,(first vars)))))

(defun gunzipped-stream (sequence)
  (make-instance
   'gzip-input-stream
   :understream (flexi-streams:make-in-memory-input-stream
                 (coerce sequence '(vector (unsigned-byte 8))))))

(defun make-debian-package (archive-array system)
  (let ((package (with-targz-archive (archive archive-array)
                   (make-deb-packager archive system)))
        (data-files (with-targz-archive (archive archive-array)
                      (make-data-files archive system))))
    (multiple-value-prog1 package
      (deb-packager:initialize-control-files package #())
      (deb-packager:initialize-data-files package data-files))))
