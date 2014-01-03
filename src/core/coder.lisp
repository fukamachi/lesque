#|
  This file is a part of lesque project.
  Copyright (c) 2014 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage lesque.coder
  (:use :cl)
  (:import-from :yason
                #+nil :encode
                :parse))
(in-package :lesque.coder)

(cl-syntax:use-syntax :annot)

@export
(defparameter *default-coder-class* 'json-coder)

@export
(defun get-default-coder ()
  (make-instance *default-coder-class*))

@export
(defclass coder () ())

@export (defgeneric encode (coder object))
@export (defgeneric decode (coder object))

@export
(defclass json-coder (coder) ())

(defmethod encode ((coder json-coder) object)
  (declare (ignore coder))
  (with-output-to-string (s)
    (yason:encode object s)))

(defmethod decode ((coder json-coder) object)
  (yason:parse object))
