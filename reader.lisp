(in-package #:qldeb)

(defun defsystem-form-info (defsystem)
  (list :name (string-downcase (second defsystem))
        :license (or (getf defsystem :license)
                     (getf defsystem :licence))
        :description (getf defsystem :description)
        :homepage (getf defsystem :homepage)
        :author (getf defsystem :author)
        :version (getf defsystem :version)))

(defun read-sharpdot-expression (stream character argument)
  (declare (ignore character argument))
  (let ((form (read stream)))
    (prin1-to-string form)))

(defun make-sharpdot-readtable ()
  (let ((readtable (copy-readtable nil)))
    (set-dispatch-macro-character #\# #\.
                                  #'read-sharpdot-expression
                                  readtable)
    readtable))

(defparameter *sharpdot-readtable* (make-sharpdot-readtable))

(defun system-file-info (stream system)
  "Read SYSTEM-FILE and look for a DEFSYSTEM form matching its
  pathname-name. Return the interesting properties of the
  form (license, description, perhaps more) as a plist."
  (let* ((*read-eval* nil)
         (*readtable* *sharpdot-readtable*)
         (cffi-grovel-p (find-package '#:cffi-grovel)))
    (unless cffi-grovel-p
      (let ((package (make-package '#:cffi-grovel)))
        (let ((symbol (intern "GROVEL-FILE" package)))
          (export symbol package))))
    (unwind-protect
         (handler-case
             (loop for form = (read stream nil stream)
                until (eq form stream)
                when (and (consp form)
                          (string-equal (first form) "DEFSYSTEM")
                          (string-equal (second form) (ql-dist:name system)))
                return (defsystem-form-info form))
           (sb-int:simple-reader-error () nil))
      (unless cffi-grovel-p
        (delete-package '#:cffi-grovel)))))
