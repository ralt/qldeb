(in-package #:qldeb)

(defun make-data-files (archive system)
  (let ((data-files (make-array 0 :adjustable t :fill-pointer 0)))
    (multiple-value-prog1 data-files
      (loop-entries (entry archive)
         (when (archive:entry-regular-file-p entry)
           (let ((bytes (make-array (archive::size entry)
                                    :element-type '(unsigned-byte 8))))
             (read-sequence bytes (archive:entry-stream entry))
             (vector-push-extend
              (make-instance
               'deb-packager:deb-file
               :path (pathname
                      (format nil "usr/share/common-lisp/source/~A/~A"
                              (system-name system)
                              (format-path (archive::%name entry))))
               :content bytes
               :size (length bytes)
               :mode (octal-to-integer (archive::mode entry)))
              data-files)))))))

(defun octal-to-integer (octal)
  (parse-integer (format nil "~8R" octal)))

(defun format-path (path)
  (format nil "~{~A~^/~}" (subseq
                           (uiop:split-string
                            (flexi-streams:octets-to-string path)
                            :separator "/")
                           1)))
