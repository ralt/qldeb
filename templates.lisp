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

(deftemplate control (folder form system)
  :name (ql-dist:name system)
  :author (author form)
  :short-description (or (getf form :description) *dummy-description*)
  :long-description (when (getf form :long-description)
                      (format-long-description
                       (getf form :long-description)))
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
  (let ((scanner (ppcre:create-scanner "^[ ]*$" :multi-line-mode t)))
    (ppcre:regex-replace-all scanner (format nil "~{ ~A~%~}" (uiop:split-string text :separator "
")) " .")))

(deftemplate changelog (folder form system)
  :name (ql-dist:name system)
  :version (system-version system)
  :author (author form)
  :date (local-time:to-rfc1123-timestring (local-time:now)))

(deftemplate compat (folder form system))

(deftemplate copyright (folder form system)
  :mit-license (cl-ppcre:scan "(?i)^mit license$" (getf form :license)))

(deftemplate rules (folder form system))

(deftemplate install (folder form system)
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
                      file (ql-dist:name system)
                      (uiop:pathname-directory-pathname file)))
            files)))
