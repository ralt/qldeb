(in-package #:qldeb)

(djula:add-template-directory (asdf:system-relative-pathname "qldeb"
                                                             "templates/"))

(defvar *templates* (make-hash-table :test #'equal))

(dolist (file '("control" "changelog" "compat" "copyright" "rules"))
  (setf (gethash file *templates*) (djula:compile-template* file)))

(defun control-file (system))

(defun changelog-file (system))

(defun compat-file (system))

(defun copyright-file (system))

(defun rules-file (system))
