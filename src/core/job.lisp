#|
  This file is a part of lesque project.
  Copyright (c) 2014 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage lesque.job
  (:use :cl
        :annot.class)
  (:import-from :lesque.connection
                :connection
                :*connection*
                :with-redis-connection)
  (:import-from :lesque.backend
                :enqueue-to-queue)
  (:import-from :lesque.failure
                :make-failure
                :save-failure)
  (:import-from :lesque.exception
                :with-backtrace-condition)
  (:import-from :lesque.coder
                :coder
                :get-default-coder
                :encode
                :decode)
  (:import-from :lesque.util
                :redis-key
                :symbol-to-string)
  (:import-from :local-time
                :now
                :format-timestring)
  (:import-from :alexandria
                :plist-hash-table))
(in-package :lesque.job)

(cl-syntax:use-syntax :annot)

@export
(defun make-job (&key queue payload)
  (let* ((coder (get-default-coder))
         (payload-hash (decode coder payload))
         (job-class (find-class (read-from-string
                                 (gethash "class" payload-hash)))))
    (make-instance job-class
                   :queue queue
                   :class (gethash "class" payload-hash)
                   :args  (gethash "args"  payload-hash))))

@export
@export-accessors
(defclass job ()
  ((class :type (or string symbol)
          :initarg :class
          :accessor job-class)
   (args :type list
         :initarg :args
         :accessor job-args)
   (queue :type string
          :initarg :queue
          :accessor job-queue)
   (coder :type coder
          :initarg :coder
          :initform (get-default-coder)
          :accessor job-coder)))

(defmethod initialize-instance :after ((job job) &key payload &allow-other-keys)
  (when payload
    (with-slots (class args) job
      (let ((parsed-payload (decode (job-coder job) payload)))
        (setf class (gethash "class" parsed-payload)
              args  (gethash "args"  parsed-payload)))))
  (unless (slot-boundp job 'class)
    (setf (slot-value job 'class)
          (class-name (class-of job))))
  (unless (slot-boundp job 'queue)
    (setf (slot-value job 'queue)
          (default-queue-name job))))

(defmethod print-object ((job job) stream)
  (with-slots (queue class args) job
    (format stream "#<~A queue=~A / class=~A / args=~A>"
            (class-name (class-of job))
            queue class args)))

@export
(defgeneric perform (job))

@export
(defgeneric enqueue-job (connection job))

@export
(defgeneric fail-job (job exception))

@export
(defgeneric job-payload (job))

@export
(defgeneric encode-job (job))

@export
(defgeneric default-queue-name (job))

(defmethod perform :around ((job job))
  (handler-case (with-backtrace-condition
                  (call-next-method))
    (error (exception)
      (fail-job job exception)
      (error exception))))

(defmethod enqueue-job ((conn connection) (job job))
  (enqueue-to-queue conn
                    (job-queue job)
                    (job-payload job)))

(defmethod fail-job ((job job) exception)
  (log:info "~A failed: ~A" job exception)
  (save-failure
   (make-failure :exception exception
                 :queue (job-queue job)
                 :payload (job-payload job))))

(defmethod job-payload ((job job))
  (with-slots (class args) job
    (encode (job-coder job)
            (plist-hash-table
             `("class" ,(etypecase class
                          (string class)
                          (symbol (symbol-to-string class)))
               "args" ,args)))))

(defmethod encode-job ((job job))
  (encode (job-coder job)
          (plist-hash-table
           `("queue" ,(job-queue job)
             "run_at" ,(format-timestring nil (now))
             "payload" ,(job-payload job)))))

(defmethod default-queue-name ((job job))
  (symbol-to-string (class-name (class-of job))))

@export
(defclass function-job (job) ())

(defmethod function-name ((job function-job))
  (first (job-args job)))

(defmethod perform ((job function-job))
  (let ((function-name (function-name job)))
    (apply (symbol-function
            (etypecase function-name
              (string (read-from-string function-name))
              (symbol function-name)))
           (rest (job-args job)))))

(defmethod default-queue-name ((job function-job))
  (etypecase (function-name job)
    (string (function-name job))
    (symbol (symbol-to-string (function-name job)))))
