;;;; metering.asd

(asdf:defsystem #:metering
  :name "Metering"
  :version "3.0"
  :description "Portable Code Profiling Tool"
  :author "Mark Kantrowitz <mkant@cs.cmu.edu>"
  :maintainer "Daniel Kochmański <daniel@turtleware.eu>"
  :license "Public Domain"
  :homepage "https://gitlab.common-lisp.net/dkochmanski/metering"
  :serial t
  :depends-on ()
  :components ((:cl-source-file.cl "metering"))
  :in-order-to ((asdf:test-op
                 (asdf:test-op #:metering/test))))

(asdf:defsystem #:metering/test
  :depends-on (#:metering #:fiveam)
  :components ((:file "metering-test"))
  :perform (asdf:test-op (o s)
             (funcall (intern (string '#:run!) :metering/test)
                      :metering-suite)))
