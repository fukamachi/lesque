#|
  This file is a part of lesque project.
  Copyright (c) 2014 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage lesque-worker.util
  (:use :cl)
  (:nicknames :lesque.worker.util)
  (:import-from :cffi
                :defcallback
                :foreign-funcall
                :callback))
(in-package :lesque-worker.util)

(cl-syntax:use-syntax :annot)

@export
(defun terminate (&optional (status 0))
  #+ccl (ccl:quit status)
  #+sbcl (sb-ext:exit :code status)
  #+allegro (excl:exit status :quiet t)
  #+clisp (ext:quit status)
  #+cmucl (unix:unix-exit status)
  #+ecl (ext:quit status)
  #-(or ccl sbcl allegro clisp cmucl ecl) (cl-user::quit))
