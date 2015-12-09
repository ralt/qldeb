(in-package #:qldeb)

(djula:add-template-directory (asdf:system-relative-pathname "qldeb"
                                                             "templates/"))

(defvar *templates* (make-hash-table :test #'equal))

(defmacro deftemplate (fn-name vars template &body args)
  `(progn
     (setf (gethash ,template *templates*) (djula:compile-template* ,template))
     (defun ,fn-name ,vars
       (declare (ignorable ,@vars))
       (djula:render-template* ,template nil ,@args))))

(deftemplate control-file (folder file system)
  "control"
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

(deftemplate changelog-file (folder file system)
  "changelog"
  :name (ql-dist:name system)
  :version (system-version system)
  :author (getf file :author)
  :date (local-time:to-rfc1123-timestring (local-time:now)))

(deftemplate compat-file (folder file system)
  "compat")

(deftemplate copyright-file (folder file system)
  "copyright"
  :mit-license (cl-ppcre:scan "(?i)^mit license$" (getf file :license)))

(deftemplate rules-file (folder file system)
  "rules")

(deftemplate install-file (folder file system)
  "install"
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
