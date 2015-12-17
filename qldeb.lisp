(in-package #:qldeb)

(defmacro with-temp (var &body body)
  (let ((temp (gensym)))
    `(let ((,temp (pathname (uiop:strcat (sb-posix:mkdtemp "/tmp/qldeb.XXXXXX") "/"))))
       (let ((,@var ,temp))
         ,@body)
       (uiop:delete-directory-tree ,temp :validate t))))

(defun main (args)
  (declare (ignore args))
  (setf lparallel:*kernel* (lparallel:make-kernel (* 2 (cpu-count))))
  (with-temp (env)
    (let ((releases-channel (lparallel:make-channel))
          (builds-channel (lparallel:make-channel))
          (dist (ql-dist:find-dist "quicklisp"))
          (releases (ql-dist:provided-releases dist)))
      (dolist (release releases)
        (lparallel:submit-task releases-channel #'install-release
                               env release (find-systems dist release)))
      (dotimes (i (1- (length releases)))
        (declare (ignore i))
        (lparallel:submit-task builds-channel
                               (lambda (systems)
                                 (dolist (system systems)
                                   (build-debian-package env system)))
                               (lparallel:receive-result releases-channel)))
      ;; Just wait for the builds to be done
      (dotimes (i (1- (length releases)))
        (declare (ignore i))
        (lparallel:receive-result builds-channel)))))

(defun find-systems (dist release)
  (remove-if-not (lambda (system)
                   (eq (ql-dist:release system) release))
                 (ql-dist:provided-systems dist)))

;;; Copy pasted from sb-cpu-affinity
(defun cpu-count ()
  (let* ((key "processor")
         (len (length key)))
    (with-open-file (f "/proc/cpuinfo")
      (loop for line = (read-line f nil nil)
         while line
         count (when (> (length line) len)
                 (string= key line :end2 len))))))
