(in-package #:qldeb)

;;;; Archive-related helpers


;;; Copy-paste of archive's with a custom handler-case.
(defmacro do-archive-entries ((entry archive &optional result)
                              &body body)
  "Iterate over the entries in ARCHIVE.  For each entry, ENTRY is bound to
an ARCHIVE-ENTRY representing the entry.  RESULT is used as in DOTIMES."
  (let ((archive-var (gensym)))
    `(let ((,archive-var ,archive))
       (do ((,entry (handler-case (archive:read-entry-from-archive ,archive-var)
                      (archive:unhandled-read-header-error () nil))
                    (handler-case (archive:read-entry-from-archive ,archive-var)
                      (archive:unhandled-read-header-error () nil))))
          ((null ,entry) ,result)
        ,@body
        (archive:discard-entry ,archive-var ,entry)))))

(defun archive-entry (archive path)
  (archive:do-archive-entries (entry archive)
    (when (and (archive:entry-regular-file-p entry)
               (pathname-match-p path (pathname
                                       (flexi-streams:octets-to-string
                                        (archive::%name entry)))))
      (return-from archive-entry entry))))

(defun read-entry (entry)
  (flexi-streams:make-flexi-stream (archive:entry-stream entry)))
