#|
  This file is a part of lesque project.
  Copyright (c) 2014 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage lesque-asd
  (:use :cl :asdf))
(in-package :lesque-asd)

(defsystem lesque
  :version "0.1"
  :author "Eitarow Fukamachi"
  :license "MIT"
  :depends-on (:cl-syntax-annot
               :cl-redis
               :yason
               :local-time
               :trivial-backtrace
               :log4cl
               :alexandria)
  :components ((:module "src/core"
                :components
                ((:file "lesque" :depends-on ("connection" "backend" "job" "failure" "coder" "util"))
                 (:file "connection" :depends-on ("coder"))
                 (:file "backend" :depends-on ("connection" "queue" "coder" "util"))
                 (:file "job" :depends-on ("connection" "backend" "failure" "exception" "coder" "util"))
                 (:file "queue" :depends-on ("coder" "util"))
                 (:file "stat" :depends-on ("connection" "util"))
                 (:file "coder")
                 (:file "failure" :depends-on ("exception"))
                 (:file "failure/redis" :depends-on ("connection" "failure" "backend" "exception" "coder" "util"))
                 (:file "exception")
                 (:file "error")
                 (:file "util"))))
  :description "Redis-backed library for creating background jobs, placing them on multiple queues, and processing them later."
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op lesque-test))))
