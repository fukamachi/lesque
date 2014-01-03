#|
  This file is a part of lesque project.
  Copyright (c) 2014 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage lesque-worker.process-coordinator
  (:use :cl
        :split-sequence)
  (:nicknames :lesque.worker.process-coordinator)
  (:import-from :trivial-shell
                :shell-command)
  (:export :worker-pids))
(in-package :lesque-worker.process-coordinator)

(defun get-worker-pids (command)
  (multiple-value-bind (output error-message status)
      (shell-command command)
    (declare (ignorable error-message))

    (unless (= status 0)
      (error error-message))

    ;; XXX: inefficient
    (loop for line in (split-sequence #\Newline output)
          when (search "lesque" line)
            collect (first (split-sequence #\Space line :count 1)))))

#+(or cygwin win windows mswindows win32 mingw32)
(defun worker-pids ()
  (error "Windows isn't supported yet."))

#+(or solaris sun sunos)
(defun worker-pids ()
  (get-worker-pids "ps -A -o pid,args"))

#-(or cygwin windows mswindows win32 mingw32
      solaris sun sunos)
(defun worker-pids ()
  (get-worker-pids "ps -A -o pid,command"))
