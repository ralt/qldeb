(in-package #:qldeb)

(defun make-package (archive system))

(defun make-control-files (archive system))

(defun make-data-files (archive system))

(defun get-asd-form (system asd-file)
  "
  An asd file can have multiple systems in it.
  "
  (find-if (lambda (form)
             (when (and (second form)
                        (symbolp (second form)))
               (string= (ql-dist:name system)
                        ;; The system name
                        (string-downcase (symbol-name (second form))))))
           (read-from-string asd-file)))


(defvar *dummy-author-email* "dummy@author.com")
(defvar *dummy-author* (format nil "Dummy author <~A>" *dummy-author-email*))
(defvar *dummy-description* "Dummy short description")

(defun author (form)
  (if (getf form :author)
      (validated-author (getf form :author))
      *dummy-author*))

(defun validated-author (author)
  (if (ppcre:scan ".+<.+>" author)
      author
      (format nil "~A <~A>" author *dummy-author-email*)))

(defun format-dependencies (system dependencies)
  (mapcar (lambda (dependency)
            (list (format nil "~A (>= ~A)" dependency (system-version system))
                  (format nil "~A (<< ~A)" dependency (1+ (parse-integer
                                                           (system-version system))))))
          dependencies))

(defun system-version (system)
  (format nil "~{~A~}" (uiop:split-string (ql-dist:version (ql-dist:dist system))
                                          :separator "-")))
