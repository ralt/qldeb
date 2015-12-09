(in-package #:qldeb)

(djula:add-template-directory (asdf:system-relative-pathname "qldeb"
                                                             "templates/"))

(defvar *templates* (make-hash-table :test #'equal))

(dolist (file '("control" "changelog" "compat" "copyright" "rules" "install"))
  (setf (gethash file *templates*) (djula:compile-template* file)))

(defmacro render (template &body body)
  `(djula:render-template* (gethash ,template *templates*) nil
                           ,@body))

(defun control-file (folder file system)
  (declare (ignore folder))
  (render "control"
   :name (ql-dist:name system)
   :author (getf file :author)
   :short-description (getf file :description)
   :long-description (when (getf file :long-description)
                       (format-long-description
                        (getf file :long-description)))
   :dependencies (when (ql-dist:required-systems system)
                   (format-dependencies
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
  (format nil "~{ ~A~%~}" (uiop:split-string text :separator "
")))

(defun changelog-file (folder file system)
  (declare (ignore folder))
  (render "changelog"
   :name (ql-dist:name system)
   :version (system-version system)
   :author (getf file :author)
   :date (local-time:to-rfc1123-timestring (local-time:now))))

(defun compat-file (folder file system)
  (declare (ignore folder file system))
  (render "compat"))

(defun copyright-file (folder file system)
  (declare (ignore folder system))
  (render "copyright"
   :mit-license (cl-ppcre:scan "(?i)^mit license$" (getf file :license))))

(defun rules-file (folder file system)
  (declare (ignore folder file system))
  (render "rules"))

(defun install-file (folder file system)
  (declare (ignore file))
  (render "install"
   :lines (format-lines system folder)))

(defun format-lines (system folder)
  (let ((files nil)
        (folder-namestring-length (length (namestring folder))))
    (fad:walk-directory
     folder
     (lambda (file)
       (push
        ;; Relative path
        (subseq (namestring file)
                folder-namestring-length)
        files)))
    (mapcar (lambda (file)
              (format nil "~A /usr/share/common-lisp/source/~A/~A"
                      file (ql-dist:name system) file))
            files)))
