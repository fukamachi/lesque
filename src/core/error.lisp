#|
  This file is a part of lesque project.
  Copyright (c) 2014 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage lesque.error
  (:use :cl))
(in-package :lesque.error)

(cl-syntax:use-syntax :annot)

@export
(define-condition lesque-error (simple-error) ()
  (:documentation "Base class of all other error exceptions. Use this to catch all Lesque errors."))

@export
(define-condition lesque-job-error (lesque-error) ())

@export
(define-condition dirty-exit (lesque-error) ())

@export
(define-condition crash-error (lesque-job-error)
  ((status :type integer
           :initarg :status
           :initform (error "`:STATUS' is required for `CRASH-ERROR'.")))
  (:report
   (lambda (condition stream)
     (format stream
             "Unexpected exit status ~D"
             (slot-value condition 'status)))))
