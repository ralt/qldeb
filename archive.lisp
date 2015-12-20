(in-package #:qldeb)

;;;; Archive-related helpers

(defun archive-entry (archive path)
  (archive:do-archive-entries (entry archive)
    (when (and (archive:entry-regular-file-p entry)
               (pathname-match-p path (archive::entry-pathname entry)))
      entry)))

(defun read-entry (entry)
  (flexi-streams:make-flexi-stream (archive:entry-stream entry)))
