#|
  This file is a part of lesque project.
  Copyright (c) 2014 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage lesque-test-asd
  (:use :cl :asdf))
(in-package :lesque-test-asd)

(defsystem lesque-test
  :author "Eitarow Fukamachi"
  :license "MIT"
  :depends-on (:lesque
               :cl-test-more)
  :components ((:module "t"
                :components
                ((:test-file "lesque"))))

  :defsystem-depends-on (:cl-test-more)
  :perform (test-op :after (op c)
                    (funcall (intern #. (string :run-test-system) :cl-test-more)
                             c)))
