#|
  This file is a part of lesque project.
  Copyright (c) 2014 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage lesque.worker
  (:use :cl
        :annot.class)
  (:nicknames :lesque-worker)
  (:import-from :lesque.worker.registry
                :worker-registry
                :remote-shutdown-p
                :remote-shutdown
                :prune-dead-workers
                :register-worker
                :unregister-worker
                :worker-working-on
                :done-working-worker)
  (:import-from :lesque.worker.child-process
                :child-process
                :fork-and-perform
                :kill-child)
  (:import-from :lesque.worker.signal-handling
                :set-signal-handler)
  (:import-from :lesque.connection
                :connection
                :reconnect
                :with-redis-connection)
  (:import-from :lesque.job
                :job
                :make-job
                :perform
                :encode-job)
  (:import-from :lesque.coder
                :get-default-coder
                :decode)
  (:import-from :lesque.stat
                :get-stat
                :incr-stat)
  (:import-from :lesque.exception
                :with-backtrace-condition)
  (:import-from :lesque.util
                :redis-key)
  (:import-from :red
                :sadd
                :smembers)
  (:import-from :osicat-posix
                :getpid
                :uname
                :sigterm
                :sigint
                :sigquit
                :sigusr1)
  (:import-from :split-sequence
                :split-sequence)
  (:import-from :alexandria
                :when-let
                :ensure-list))
(in-package :lesque.worker)

(cl-syntax:use-syntax :annot)

@export
(defun run (queue-or-queues &key (host "localhost") (port 6379) (interval 5))
  (let* ((queues (ensure-list queue-or-queues))
         (worker (make-instance 'worker
                                :queues queues
                                :client (make-instance 'connection
                                                       :host host
                                                       :port port))))
    (with-redis-connection (worker-client worker)
      (work worker :interval interval))))

@export
@export-accessors
(defclass worker ()
  ((queues :type list
           :initarg :queues
           :initform (error "Please give each worker at least one queue.")
           :accessor worker-queues)
   (client :type connection
           :initarg :client
           :reader worker-client)
   (pid :type integer
        :initarg :pid
        :initform (getpid))
   (hostname :type string
             :initarg :hostname
             :initform (nth-value 1 (uname)))
   (shutdown-p :type boolean
               :initform nil)
   (child :type (or child-process null)
          :accessor worker-child
          :initform nil)
   (registry :type worker-registry
             :initarg :registry
             :initform (make-instance 'worker-registry)
             :accessor worker-registry)))

@export
(defmethod worker-shutdown-p ((worker worker))
  (or (slot-value worker 'shutdown-p)
      (remote-shutdown-p (worker-registry worker)
                         (worker-id worker))))

(defmethod print-object ((worker worker) stream)
  (with-slots (hostname pid queues) worker
    (format stream "#<WORKER hostname=~A / pid=~A / queues=~{~A~^,~}>"
            hostname pid queues)))

@export
(defmethod work ((worker worker) &key (interval 5))
  (handler-case (with-backtrace-condition
                  (startup worker)
                  (work-loop worker :interval interval))
    (error (exception)
      (unregister-worker (worker-registry worker) (worker-id worker)
                         :exception exception)
      (error exception))))

(defmethod work-loop ((worker worker) &key interval)
  (loop
    while (not (worker-shutdown-p worker))
    do (register-worker (worker-registry worker) (worker-id worker))
       (let ((job (reserve-job worker :timeout interval)))
         (if job
             (process-job worker job)
             (log:debug "Timed out after ~D seconds" interval))))
  (unregister-worker (worker-registry worker) (worker-id worker)))

@export
(defmethod startup ((worker worker))
  (log:info "Starting...")
  (reconnect (worker-client worker))
  (register-signal-handlers worker)
  (prune-dead-workers (worker-registry worker))
  (register-worker (worker-registry worker) (worker-id worker)))

@export
(defmethod shutdown ((worker worker) &key (remote nil))
  (log:info "Exiting...")
  (setf (slot-value worker 'shutdown-p) t)
  (when remote
    (remote-shutdown (worker-registry worker)
                     (worker-id worker))))

@export
(defmethod force-shutdown ((worker worker))
  (shutdown worker)
  (when (worker-child worker)
    (kill-child (worker-child worker))))

@export
(defmethod worker-id ((worker worker))
  (with-slots (hostname pid queues) worker
    (format nil "~A:~A:~{~A~^,~}"
            hostname pid queues)))

@export
(defmethod (setf worker-id) (id (worker worker))
  (destructuring-bind (hostname pid queues)
      (split-sequence #\: id :count 3)
    (setf (slot-value worker 'hostname) hostname
          (slot-value worker 'pid) pid
          (slot-value worker 'queues) (split-sequence #\, queues))))

@export
(defmethod process-job ((worker worker) (job job))
  (log:info "got: ~A" job)
  (worker-working-on (worker-registry worker)
                     (worker-id worker)
                     (encode-job job))
  (unwind-protect (fork-for-child worker job)
    (incr-stat "processed")
    (incr-stat (format nil "processed:~A" (worker-id worker)))
    (done-working-worker (worker-registry worker)
                         (worker-id worker))))

@export
(defmethod perform-job ((worker worker) (job job))
  (handler-case (perform job)
    (error ()
      (incr-stat "failed")
      (incr-stat (format nil "failed:~A" (worker-id worker))))))

@export
(defmethod reserve-job ((worker worker) &key (timeout 5))
  (let ((ret (apply #'red:blpop
                    (append
                     (mapcar (lambda (queue)
                               (redis-key "queue" queue))
                             (worker-queues worker))
                     (list timeout)))))
    (if ret
        (destructuring-bind (queue payload) ret
          (log:debug "Found job on ~A" queue)
          (decode (get-default-coder) payload)
          (make-job :queue queue
                    :payload payload))
        nil)))

@export
(defmethod processed-job-count ((worker worker))
  (get-stat (format nil "processed:~A" (worker-id worker))))

@export
(defmethod failed-job-count ((worker worker))
  (get-stat (format nil "failed:~A" (worker-id worker))))

(defmethod fork-for-child ((worker worker) (job job))
  (unwind-protect (progn
                    (setf (worker-child worker)
                          (make-instance 'child-process
                                         :client (worker-client worker)))
                    (fork-and-perform (worker-child worker) job
                                      (lambda (job)
                                        (perform-job worker job))))
    (setf (worker-child worker) nil)))

(defun register-signal-handlers (worker)
  (set-signal-handler sigterm (lambda () (shutdown worker)))
  (set-signal-handler sigint  (lambda () (force-shutdown worker)))
  (set-signal-handler sigquit (lambda () (shutdown worker)))
  (set-signal-handler sigusr1 (lambda () (kill-child (worker-child worker)))))
