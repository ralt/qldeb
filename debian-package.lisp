(in-package #:qldeb)

(defun make-debian-package (release archive-array system)
  (let ((archive (archive:open-archive
                  'archive:tar-archive
                  (flexi-streams:make-in-memory-input-stream archive-array)))
        (package (make-package archive system))
        (control-files (make-control-files archive system))
        (data-files (make-data-files archive system)))
    (multiple-value-prog1 package
      (deb-packager:initialize-control-files package control-files)
      (deb-packager:initialize-data-files package data-files)
      (archive:close-archive archive))))
