(in-package #:qldeb)

;;;; Archive-related helpers

(defun archive-entry (archive path)
  (archive:do-archive-entries (entry archive)
    (when (and (archive:entry-regular-file-p entry)
               (pathname-match-p path (pathname
                                       (flexi-streams:octets-to-string
                                        (archive::%name entry)))))
      (return-from archive-entry entry))))

(defun read-entry (entry)
  (flexi-streams:make-flexi-stream (archive:entry-stream entry)))
