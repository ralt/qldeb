(in-package #:qldeb)

(defun asd-path (release system)
  (merge-pathnames (uiop:strcat (ql-dist:system-file-name system) ".asd")
                   (uiop:strcat (ql-dist:prefix release) "/")))

(defun make-deb-packager (archive system release)
  (let ((asd-form (get-asd-form
                   system
                   (read-entry (archive-entry archive (asd-path release system))))))
    (make-instance
     'deb-packager:deb-package
     :name (ql-dist:name system)
     :changelog (make-changelog-entry system asd-form)
     :description (or (getf asd-form :description) *dummy-description*)
     :architecture "all"
     :depends (when (ql-dist:required-systems system)
                (format-dependencies
                 system
                 (ql-dist:required-systems system)))
     :long-description (when (getf asd-form :long-description)
                         (format-long-description
                          (getf asd-form :long-description)))
     :maintainer (author asd-form))))

(defun make-changelog-entry (system asd-form)
  (make-array
   1
   :initial-contents (list (make-instance
                            'deb-packager:changelog-entry
                            :version (system-version system)
                            :author (author asd-form)
                            :message "qldeb changelog"
                            :date 1434665940))))

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

(defun get-asd-form (system asd-stream)
  "
  An asd file can have multiple systems in it.
  "
  (find-if (lambda (form)
             (when (and (second form)
                        (symbolp (second form)))
               (string= (ql-dist:name system)
                        ;; The system name
                        (string-downcase (symbol-name (second form))))))
           (uiop:slurp-stream-forms asd-stream)))

(defun format-long-description (text)
  (let ((scanner (ppcre:create-scanner "^[ ]*$" :multi-line-mode t)))
    (ppcre:regex-replace-all scanner (format nil "~{ ~A~%~}" (uiop:split-string text :separator "
")) " .")))
