(in-package #:qldeb)

(defun system-name (system)
  (format-system-name (ql-dist:name system)))

(defun format-system-name (name)
  (uiop:strcat "cl-" (ppcre:regex-replace-all "/" name "--")))

(defun asd-path (system)
  (merge-pathnames (uiop:strcat (ql-dist:system-file-name system) ".asd")
                   (uiop:strcat (ql-dist:prefix (ql-dist:release system)) "/")))

(defun make-deb-packager (archive system)
  (let ((asd-form (system-file-info
                   (read-entry (archive-entry archive (asd-path system)))
                   system)))
    (make-instance
     'deb-packager:deb-package
     :name (make-symbol (system-name system))
     :changelog (make-changelog-entry system asd-form)
     :description (or (getf asd-form :description) *dummy-description*)
     :architecture "all"
     :build-depends nil
     :depends (when (ql-dist:required-systems system)
                (format-dependencies
                 system
                 (ql-dist:required-systems system)))
     :long-description (if (getf asd-form :long-description)
                           (format-long-description
                            (getf asd-form :long-description))
                           "")
     :maintainer (author asd-form))))

(defun make-changelog-entry (system asd-form)
  (make-array
   1
   :initial-contents (list (make-instance
                            'deb-packager:changelog-entry
                            :version (format nil "~D" (system-version system))
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
            (format nil "~A (>= ~A), ~A (<< ~A)"
                    (format-system-name dependency) (system-version system)
                    (format-system-name dependency) (1+ (system-version system))))
          dependencies))

(defun system-version (system)
  (parse-integer
   (format nil "~{~A~}" (uiop:split-string
                         (ql-dist:version (ql-dist:dist system))
                         :separator "-"))))

(defun format-long-description (text)
  (let ((scanner (ppcre:create-scanner "^[ ]*$" :multi-line-mode t)))
    (ppcre:regex-replace-all
     scanner
     (format nil "~{ ~A~%~}"
             (uiop:split-string text :separator "
"))
     " .")))
