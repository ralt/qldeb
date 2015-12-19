(in-package #:qldeb)

(defun main (args)
  (declare (ignore args))
  (setf lparallel:*kernel* (lparallel:make-kernel (* 2 (cpu-count))))
  (let* ((channel (lparallel:make-channel))
         (dist (ql-dist:find-dist "quicklisp"))
         (releases (ql-dist:provided-releases dist)))
    (submit-tasks channel #'download-and-package releases)
    (wait-for-tasks channel (length releases))))

(defun submit-tasks (channel fn objects)
  (dolist (object objects)
    (lparallel:submit-task channel fn object)))

(defun wait-for-tasks (channel length)
  (dotimes (i length)
    (lparallel:receive-result channel)))

;;; Copy pasted from sb-cpu-affinity
(defun cpu-count ()
  (let* ((key "processor")
         (len (length key)))
    (with-open-file (f "/proc/cpuinfo")
      (loop for line = (read-line f nil nil)
         while line
         count (when (> (length line) len)
                 (string= key line :end2 len))))))
