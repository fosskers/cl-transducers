(defpackage transducers/tests/main
  (:use :cl
        :transducers
        :rove))
(in-package :transducers/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :transducers)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
