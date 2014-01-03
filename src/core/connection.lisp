#|
  This file is a part of lesque project.
  Copyright (c) 2014 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage lesque.connection
  (:use :cl
        :annot.class)
  (:import-from :lesque.coder
                :coder
                :get-default-coder)
  (:import-from :redis
                :redis-connection
                #+nil :open-connection
                :close-connection
                :connection-open-p))
(in-package :lesque.connection)

(cl-syntax:use-syntax :annot)

@export
(defvar *connection* nil)

@export
@export-accessors
(defclass connection ()
  ((host :type 'string
         :initarg :host
         :initform "localhost")
   (port :type 'integer
         :initarg :port
         :initform 6379)
   (redis :type (or redis-connection null)
          :initform nil
          :accessor redis-connection)
   (coder :type coder
          :initarg :coder
          :initform (get-default-coder)
          :accessor connection-coder)))

(defmethod print-object ((connection connection) stream)
  (format stream "#<~A ~A:~D>"
          (class-name (class-of connection))
          (slot-value connection 'host)
          (slot-value connection 'port)))

@export
(defmethod open-connection ((connection connection))
  (setf (redis-connection connection)
        (make-instance 'redis:redis-connection
                       :host (slot-value connection 'host)
                       :port (slot-value connection 'port)))
  (setf *connection* connection))

@export
(defmethod connected-p ((conn connection))
  (and (slot-boundp conn 'redis)
       (redis-connection conn)
       (redis::connection-open-p (redis-connection conn))))

@export
(defmacro with-redis-connection (conn &body body)
  `(progn
     (unless (connected-p ,conn)
       (open-connection ,conn))
     (let ((redis::*connection* (redis-connection ,conn)))
       ,@body)))

@export
(defun connect (&rest initargs &key host port)
  (declare (ignore host port))
  (open-connection (apply #'make-instance 'connection initargs)))

@export
(defmethod disconnect ((conn connection))
  (unless (connected-p conn)
    (return-from disconnect))

  (redis:close-connection (redis-connection conn))
  (when (eq *connection* conn)
    (setf *connection* nil)))

@export
(defmethod reconnect ((conn connection))
  (if (connected-p conn)
      (progn
        (redis:close-connection (redis-connection conn))
        (redis:open-connection (redis-connection conn)))
      (open-connection conn)))
