(in-package #:qldeb)

;;;; Archive-related helpers

(defmacro loop-entries (vars &body body)
  (let ((unhandled-error (gensym))
        (continue (gensym)))
    `(loop for ,(first vars) = (handler-case
                                   (archive:read-entry-from-archive ,(second vars))
                                 (archive:unhandled-read-header-error ()
                                   ',unhandled-error))
        until (null ,(first vars))
        do (block ,continue
             (when (eq ,(first vars) ',unhandled-error)
               (return-from ,continue))
             (progn ,@body)
             (archive:discard-entry ,(second vars) ,(first vars))))))

(defun archive-entry (archive path)
  (loop-entries (entry archive)
     (when (and
            (archive:entry-regular-file-p entry)
            (pathname-match-p path (pathname
                                    (flexi-streams:octets-to-string
                                     (archive::%name entry)))))
       (return-from archive-entry entry))))

(defun read-entry (entry)
  (flexi-streams:make-flexi-stream (archive:entry-stream entry)))
