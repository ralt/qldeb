(in-package #:qldeb)

(defmacro with-chroot (var &body body)
  (let ((temp (gensym)))
    `(let ((,temp (pathname (uiop:strcat (sb-posix:mkdtemp "/tmp/qldeb.XXXXXX") "/"))))
       (uiop:run-program
        (format nil "cdebootstrap \\
--arch=amd64 \\
--flavour=build \\
--include=devscripts \\
debian/jessie \\
~A \\
http://httpredir.debian.org/debian" (namestring ,temp)))
       (let ((,@var ,temp))
         ,@body)
       (uiop:delete-directory-tree ,temp :validate t))))

(defun main (args)
  (declare (ignore args))
  (unless (= (sb-posix:geteuid) 0)
    (format t "you're not root, go away~%")
    (uiop:quit -1))
  (setf lparallel:*kernel* (lparallel:make-kernel (* 2 (cpu-count))))
  (with-chroot (env)
    (let ((channel (lparallel:make-channel)))
      (dolist (release-object (release-objects (ql-dist:find-dist "quicklisp")))
        (lparallel:submit-task channel #'download-release
                               env release-object))
      (loop while systems = (lparallel:receive-result channel)
         do (mapcar (lambda (system)
                      (build-debian-package env system))
                    systems)))))

(defun releases-objects (dist)
  ;;; Returns a list built like this:
  ;;; ((<QL-DIST:RELEASE {1}> (<QL-DIST:SYSTEM {1a}> <QL-DIST:SYSTEM {1b}> ...))
  ;;;  (<QL-DIST:RELEASE {2}> (<QL-DIST:SYSTEM {2a}> <QL-DIST:SYSTEM {2b}> ...)))
  (loop for release in (ql-dist:provided-releases dist)
     collect (cons release (list (find-systems dist release)))))

(defun find-systems (dist release)
  (remove-if-not (lambda (systems)
                   (equal (ql-dist:release systems) release))
                 (ql-dist:provided-systems dist)))

(defun download-release (env release-object))

(defun build-debian-package (env system))

;;; Copy pasted from sb-cpu-affinity
(defun cpu-count ()
  (let* ((key "processor")
         (len (length key)))
    (with-open-file (f "/proc/cpuinfo")
      (loop for line = (read-line f nil nil)
         while line
         count (when (> (length line) len)
                 (string= key line :end2 len))))))
