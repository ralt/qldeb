(in-package #:qldeb)

(djula:add-template-directory (asdf:system-relative-pathname "qldeb"
                                                             "templates/"))

(defvar *templates* (make-hash-table :test #'equal))

(defmacro deftemplate (template vars &body args)
  (let ((tmpl (gensym))
        (compiled-template (gensym)))
    `(let* ((,tmpl ,(string-downcase (symbol-name template)))
            (,compiled-template (djula:compile-template* ,tmpl)))
       (setf
        (gethash ,tmpl *templates*)
        (list
         :compiled-template ,compiled-template
         :lambda (lambda ,vars
                   (declare (ignorable ,@vars))
                   (djula:render-template* ,compiled-template nil ,@args)))))))

(deftemplate control (folder file system)
  :name (ql-dist:name system)
  :author (getf file :author)
  :short-description (getf file :description)
  :long-description (when (getf file :long-description)
                      (format-long-description
                       (getf file :long-description)))
  :dependencies (when (ql-dist:required-systems system)
                  (format-dependencies
                   system
                   (ql-dist:required-systems system))))

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

(deftemplate changelog (folder file system)
  :name (ql-dist:name system)
  :version (system-version system)
  :author (getf file :author)
  :date (local-time:to-rfc1123-timestring (local-time:now)))

(deftemplate compat (folder file system))

(deftemplate copyright (folder file system)
  :mit-license (cl-ppcre:scan "(?i)^mit license$" (getf file :license)))

(deftemplate rules (folder file system))

(deftemplate install (folder file system)
  :lines (format-install-lines system folder))

(defun format-install-lines (system folder)
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
