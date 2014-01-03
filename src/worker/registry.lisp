#|
  This file is a part of lesque project.
  Copyright (c) 2014 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage lesque-worker.registry
  (:use :cl
        :split-sequence)
  (:nicknames :lesque.worker.registry)
  (:import-from :lesque.worker.process-coordinator
                :worker-pids)
  (:import-from :lesque.job
                :make-job
                :fail-job)
  (:import-from :lesque.coder
                :coder
                :get-default-coder
                :decode)
  (:import-from :lesque.stat
                :clear-stat)
  (:import-from :lesque.exception
                :make-exception)
  (:import-from :lesque.error
                :dirty-exit)
  (:import-from :lesque.util
                :redis-key)
  (:import-from :redis
                :with-pipelining)
  (:import-from :red
                :smembers
                :sismember
                :srem
                :del
                #+nil :get
                #+nil :set
                :exists)
  (:import-from :osicat-posix
                :uname)
  (:import-from :local-time
                :format-timestring
                :now)
  (:import-from :alexandria
                :when-let))
(in-package :lesque-worker.registry)

(cl-syntax:use-syntax :annot)

@export
(defvar *redis-workers-key* "workers")
@export
(defvar *redis-single-worker-key* "worker")

@export
(defclass worker-registry ()
  ((coder :type coder
          :initarg :coder
          :initform (get-default-coder)
          :accessor worker-registry-coder)))

@export
(defmethod all-workers ((registry worker-registry))
  (declare (ignore registry))
  (red:smembers (redis-key *redis-workers-key*)))

@export
(defmethod worker-exists-p ((registry worker-registry) worker-id)
  (declare (ignore registry))
  (red:sismember (redis-key *redis-workers-key*) worker-id))

@export
(defmethod unregister-worker ((registry worker-registry) worker-id
                              &key (exception (make-exception (make-instance 'dirty-exit))))
  (when-let (job-info (processing-job registry worker-id))
    (fail-job
     (make-job :queue (gethash "queue" job-info)
               :payload (gethash "payload" job-info))
     exception))

  (redis:with-pipelining
    (red:srem (redis-key *redis-workers-key*) worker-id)
    (red:del (redis-key *redis-single-worker-key* worker-id)
             (redis-key *redis-single-worker-key* worker-id "started")
             (redis-key *redis-single-worker-key* worker-id "shutdown"))
    (clear-stat (format nil "processed:~A" worker-id))
    (clear-stat (format nil "failed:~A" worker-id))))

@export
(defmethod register-worker ((registry worker-registry) worker-id)
  (declare (ignore registry))
  (redis:with-pipelining
    (red:sadd (redis-key *redis-workers-key*) worker-id)
    (red:set (redis-key *redis-single-worker-key* worker-id "started")
             (format-timestring nil (now)))))

@export
(defmethod worker-working-on ((registry worker-registry) worker-id encoded-job)
  (declare (ignore registry))
  (red:set (redis-key *redis-single-worker-key* worker-id)
           encoded-job))

@export
(defmethod done-working-worker ((registry worker-registry) worker-id)
  (declare (ignore registry))
  (red:del (redis-key *redis-single-worker-key* worker-id)))

@export
(defmethod worker-status ((registry worker-registry) worker-id)
  (declare (ignore registry))
  (if (red:exists (redis-key *redis-single-worker-key* worker-id))
      :working
      :idle))

@export
(defmethod worker-started-time ((registry worker-registry) worker-id)
  (declare (ignore registry))
  (red:get (redis-key *redis-single-worker-key* worker-id "started")))

@export
(defmethod processing-job ((registry worker-registry) worker-id)
  (let ((job-info (red:get (redis-key *redis-single-worker-key* worker-id))))
    (if job-info
        (decode (worker-registry-coder registry) job-info)
        nil)))

@export
(defmethod prune-dead-workers ((registry worker-registry))
  (when-let (all-worker-ids (all-workers registry))
    (let ((known-worker-pids (worker-pids))
          (registry-hostname (nth-value 1 (uname))))
      (loop for worker-id in all-worker-ids
            for (host) = (split-sequence #\: worker-id :count 1)
            when (and (string= host registry-hostname)
                      (member worker-id known-worker-pids :test #'equal))
              do (unregister-worker registry worker-id)))))

@export
(defmethod remote-shutdown ((registry worker-registry) worker-id)
  (red:set (redis-key *redis-single-worker-key* worker-id "shutdown")
           "true"))

@export
(defmethod remote-shutdown-p ((registry worker-registry) worker-id)
  (red:get (redis-key *redis-single-worker-key* worker-id "shutdown")))
