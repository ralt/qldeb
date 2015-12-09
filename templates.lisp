(in-package #:qldeb)

(djula:add-template-directory (asdf:system-relative-pathname "qldeb"
                                                             "templates/"))

(defvar *templates* (make-hash-table :test #'equal))

(dolist (file '("control" "changelog" "compat" "copyright" "rules"))
  (setf (gethash file *templates*) (djula:compile-template* file)))

(defun control-file (system-file system)
  (djula:render-template*
   (gethash "control" *templates*) nil
   :name (ql-dist:name system)
   :author (getf system-file :author)
   :short-description (getf system-file :description)
   (when (getf system-file :long-description)
     :long-description (format-long-description
                        (getf system-file :long-description)))
   (when (ql-dist:required-systems system)
     :dependencies (format-dependencies
                    system
                    (ql-dist:required-systems system)))))

(defun format-dependencies (system dependencies)
  (mapcar (lambda (dependency)
            (format nil "~A (>= ~A), ~A (<< ~A)"
                    dependency
                    (system-version system)
                    dependency
                    (1+ (parse-integer (system-version system)))))
          dependencies))

(defun format-long-description (text)
  (format nil "~{ ~A~%~}~%" (uiop:split-string text :separator "
")))

(defun changelog-file (system-file system))

(defun compat-file (system-file system)
  (declare (ignore system-file system))
  (djula:render-template* (gethash "compat" *templates*) nil))

(defun copyright-file (system-file system))

(defun rules-file (system-file system))
