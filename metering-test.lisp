;;; Ensure the package
(defpackage #:metering/test
  (:use #:cl #:5am))
(in-package #:metering/test)

;;; Ensure the suite
(def-suite :metering-suite
    :description "The example test suite.")
(in-suite :metering-suite)

;;; Define a few functions to monitor functions
(defun test-fun-1 ()
  (dotimes (v 100) (cons 'b 'c)))

(defun test-fun-2 (x)
  (if (> x 3)
      x
      (test-fun-2 (1+ x))))

;;; Define some tests
(test dumb-test
  "Test if the testing works"
  (is (= 1 1)))

(test monitor-null
  "Check if adding non-existant function doesn't change the list of
monitored functions"
  (is (equal (mon:monitor nil)
	     (mon:monitor xyz))))

(test with-monitoring
  "Monitor two functions"
  (finishes (mon:with-monitoring (test-fun-1 test-fun-2) ()
	      (dotimes (v 1001)
		(test-fun-1)
		(test-fun-2 1)))))
