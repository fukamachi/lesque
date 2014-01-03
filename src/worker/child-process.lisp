#|
  This file is a part of lesque project.
  Copyright (c) 2014 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage lesque-worker.child-process
  (:use :cl)
  (:nicknames :lesque.worker.child-process)
  (:import-from :lesque.worker.util
                :terminate)
  (:import-from :lesque.connection
                :connection
                :reconnect
                :with-redis-connection)
  (:import-from :lesque.worker.signal-handling
                :unset-signal-handler)
  (:import-from :lesque.job
                :job
                :perform
                :fail-job)
  (:import-from :lesque.exception
                :make-exception)
  (:import-from :lesque.error
                :crash-error)
  (:import-from :osicat-posix
                :sigterm
                :sigint
                :sigquit
                :sigusr1
                :sigusr2
                :sigkill
                :fork
                :kill
                :waitpid))
(in-package :lesque-worker.child-process)

(cl-syntax:use-syntax :annot)

@export
(defclass child-process ()
  ((client :type connection
           :initarg :client
           :accessor client-connection)
   (pid :type (or integer null)
        :accessor child-pid)))

(defmethod child-job ((child child-process) (job job) perform-fn)
  (reconnect (client-connection child))
  (unregister-signal-handlers)
  (with-redis-connection (client-connection child)
    (ignore-errors (funcall perform-fn job)))
  (terminate))

@export
(defmethod fork-and-perform ((child child-process) (job job) &optional perform-fn)
  (setf (child-pid child) #+sbcl (sb-posix:fork)
                          #-sbcl (osicat-posix:fork))

  (if (= (child-pid child) 0)
      (child-job child job (or perform-fn
                               (lambda (job) (perform job))))
      (multiple-value-bind (pid status)
          (waitpid (child-pid child))
        (unless (= status 0)
          (fail-job job (make-exception (make-instance 'crash-error
                                                       :status status))))))

  (setf (child-pid child) nil))

@export
(defmethod kill-child ((child child-process))
  (unless (child-pid child)
    (return-from kill-child))

  (when (waitpid (child-pid child) :no-hang t)
    (log:debug "Child ~D already quit."
               (child-pid child))
    (return-from kill-child))

  (log:debug "Sending KILL signal to child ~D" (child-pid child))
  (kill (child-pid child) sigkill))

(defun unregister-signal-handlers ()
  (unset-signal-handler sigterm)
  (unset-signal-handler sigint)
  (unset-signal-handler sigquit)
  (unset-signal-handler sigusr1)
  (unset-signal-handler sigusr2))
