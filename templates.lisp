(in-package #:qldeb)

(djula:add-template-directory (asdf:system-relative-pathname "qldeb"
                                                             "templates/"))

(defvar *templates* (make-hash-table :test #'equal))

(dolist (file '("control" "changelog" "compat" "copyright" "rules" "install"))
  (setf (gethash file *templates*) (djula:compile-template* file)))

(defun control-file (folder file system)
  (declare (ignore folder))
  (djula:render-template*
   (gethash "control" *templates*) nil
   :name (ql-dist:name system)
   :author (getf file :author)
   :short-description (getf file :description)
   (when (getf file :long-description)
     :long-description (format-long-description
                        (getf file :long-description)))
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

(defun changelog-file (folder file system)
  (declare (ignore folder))
  (djula:render-template*
   (gethash "changelog" *templates*) nil
   :name (ql-dist:name system)
   :version (system-version system)
   :author (getf file :author)
   :date (local-time:to-rfc1123-timestring (local-time:now))))

(defun compat-file (folder file system)
  (declare (ignore folder file system))
  (djula:render-template* (gethash "compat" *templates*) nil))

(defun copyright-file (folder file system)
  (declare (ignore folder system))
  (djula:render-template*
   (gethash "copyright" *templates*) nil
   ;; Make this case-insensitive
   :mit-license (cl-ppcre:scan "^mit license$" (getf file :license))))

(defun rules-file (folder file system)
  (declare (ignore folder file system))
  (djula:render-template* (gethash "rules" *templates*) nil))

(defun install-file (folder file system)
  (declare (ignore file))
  (djla:render-template*
   (gethash "install" *templates*) nil
   :lines (format-lines system folder)))

(defun format-lines (system folder))
