(defpackage transducers/tests
  (:use :cl :parachute)
  (:local-nicknames (#:t #:transducers)))

(in-package :transducers/tests)

;; NOTE: To run this test file, execute `(asdf:test-system :transducers)' in your Lisp.

(define-test reduction)

(define-test "Collecting"
  :parent reduction
  (is equal '() (t:transduce (t:map #'identity) #'t:cons '()))
  (is equalp #() (t:transduce (t:map #'identity) #'t:vector #()))
  (is equal "hello" (t:transduce (t:map #'identity) #'t:string "hello")))

(define-test "Counting"
  :parent reduction
  (is = 0 (t:transduce (t:map #'identity) #'t:count '()))
  (is = 3 (t:transduce (t:map #'identity) #'t:count '(1 2 3)))
  (is = 0 (t:transduce (t:map #'identity) #'t:count #()))
  (is = 3 (t:transduce (t:map #'identity) #'t:count #(1 2 3))))

(define-test "Predicates"
  :parent reduction
  (false (t:transduce (t:map #'identity) (t:any #'evenp) '(1 3 5 7 9)))
  (true  (t:transduce (t:map #'identity) (t:any #'evenp) '(1 3 5 7 9 2)))
  (true  (t:transduce (t:map #'identity) (t:all #'oddp) '(1 3 5 7 9)))
  (false (t:transduce (t:map #'identity) (t:all #'oddp) '(1 3 5 7 9 2))))

(define-test "First and Last"
  :parent reduction
  (is = 7  (t:transduce (t:filter #'oddp) (t:first 0) '(2 4 6 7 10)))
  (is = 10 (t:transduce (t:map #'identity) (t:last 0) '(2 4 6 7 10))))

(define-test "Folding and Finding"
  :parent reduction
  (is = 1000 (t:transduce (t:map #'identity) (t:fold #'cl:max 0) '(1 2 3 4 1000 5 6)))
  (is = 6 (t:transduce (t:map #'identity) (t:find #'evenp) '(1 3 5 6 9))))

(define-test transduction)

(define-test "Mapping"
  :parent transduction
  :depends-on (reduction)
  (is equal '() (t:transduce (t:map #'1+) #'t:cons '()))
  (is equal '(2 3 4) (t:transduce (t:map #'1+) #'t:cons '(1 2 3))))

(define-test "Filtering"
  :parent transduction
  :depends-on (reduction)
  (is equal '(2 4) (t:transduce (t:filter #'evenp) #'t:cons '(1 2 3 4 5)))
  (is equal '(2 5 8) (t:transduce (t:filter-map #'cl:first) #'t:cons '(() (2 3) () (5 6) () (8 9))))
  (is equal '(1 2 3 "abc") (t:transduce #'t:unique #'t:cons '(1 2 1 3 2 1 2 "abc")))
  (is equal '(1 2 3 4 3)
      (t:transduce #'t:dedup #'t:cons '(1 1 1 2 2 2 3 3 3 4 3 3))))

(define-test "Taking and Dropping"
  :parent transduction
  :depends-on (reduction)
  (is equal '() (t:transduce (t:drop 100) #'t:cons '(1 2 3 4 5)))
  (is equal '(4 5) (t:transduce (t:drop 3) #'t:cons '(1 2 3 4 5)))
  (is equal '(7 8 9) (t:transduce (t:drop-while #'evenp) #'t:cons '(2 4 6 7 8 9)))
  (is equal '() (t:transduce (t:take 0) #'t:cons '(1 2 3 4 5)))
  (is equal '(1 2 3) (t:transduce (t:take 3) #'t:cons '(1 2 3 4 5)))
  (is equal '() (t:transduce (t:take-while #'evenp) #'t:cons '(1)))
  (is equal '(2 4 6 8) (t:transduce (t:take-while #'evenp) #'t:cons '(2 4 6 8 9 2))))

(define-test "Flattening"
  :parent transduction
  :depends-on (reduction)
  (is equal '(1 2 3 4 5 6 7 8 9)
      (t:transduce #'t:concatenate #'t:cons '((1 2 3) (4 5 6) (7 8 9))))
  (is equal '(1 2 3 0 4 5 6 0 7 8 9 0)
      (t:transduce #'t:flatten #'t:cons '((1 2 3) 0 (4 (5) 6) 0 (7 8 9) 0))))

(define-test "Pairing"
  :parent transduction
  :depends-on (reduction)
  (is equal '((1 2 3) (4 5)) (t:transduce (t:segment 3) #'t:cons '(1 2 3 4 5)))
  (is equal '((1 2 3) (2 3 4) (3 4 5))
      (t:transduce (t:window 3) #'t:cons '(1 2 3 4 5)))
  (is equal '((2 4 6) (7 9 1) (2 4 6) (3))
      (t:transduce (t:group-by #'evenp) #'t:cons '(2 4 6 7 9 1 2 4 6 3))))

(define-test "Other"
  :parent transduction
  :depends-on (reduction)
  (is equal '(1 3 5 7 9)
      (t:transduce (t:step 2) #'t:cons '(1 2 3 4 5 6 7 8 9)))
  (is equal '(1 0 2 0 3) (t:transduce (t:intersperse 0) #'t:cons '(1 2 3)))
  (is equal '((0 . "a") (1 . "b") (2 . "c"))
      (t:transduce #'t:enumerate #'t:cons '("a" "b" "c")))
  (is equal '(0 1 3 6 10)
      (t:transduce (t:scan #'+ 0) #'t:cons '(1 2 3 4)))
  (is equal '(0 1)
      (t:transduce (t:comp (t:scan #'+ 0) (t:take 2))
                   #'t:cons '(1 2 3 4))))

(define-test "Composition"
  :parent transduction
  :depends-on (reduction
               "Taking and Dropping"
               "Filtering"
               "Other")
  (is equal '(12 20 30)
      (t:transduce (t:comp
                     #'t:enumerate
                     (t:map (lambda (pair) (* (car pair) (cdr pair))))
                     (t:filter #'evenp)
                     (t:drop 3)
                     (t:take 3))
                   #'t:cons
                   '(1 2 3 4 5 6 7 8 9 10))))

#+nil
(t:transduce (t:comp (t:drop 3) (t:take 2)) #'t:cons '(1 2 3 4 5 6))
#+nil
(funcall (t:comp #'1+ (lambda (n) (* 2 n))) 7)

(define-test "Generators"
  :depends-on (reduction transduction)
  (is equal '() (t:transduce (t:take 0) #'t:cons (t:ints 0)))
  (is equal '(0 1 2 3) (t:transduce (t:take 4) #'t:cons (t:ints 0)))
  (is equal '(0 -1 -2 -3) (t:transduce (t:take 4) #'t:cons (t:ints 0 :step -1)))
  (is equal '(1 2 3 1 2 3 1) (t:transduce (t:take 7) #'t:cons (t:cycle '(1 2 3))))
  (is equal '(1 2 3 1 2 3 1) (t:transduce (t:take 7) #'t:cons (t:cycle #(1 2 3))))
  (is equal "hellohe" (t:transduce (t:take 7) #'t:string (t:cycle "hello")))
  (is equal '() (t:transduce (t:take 7) #'t:cons (t:cycle '()))))

(define-test "Higher Order Transducers"
  :depends-on (reduction transduction)
  (is equal '(1 4 1 4 1)
      (t:transduce (t:comp (t:take 5)
                           (t:map #'1+)
                           (t:branch #'evenp
                                     (t:map (t:comp #'write-to-string #'1+))
                                     (t:map (t:const "Odd!")))
                           (t:map #'length))
                   #'t:cons (t:ints 1)))
  (is equal '((1 2 3 4 5 6 7 8 9) . 12)
      (t:transduce (t:comp (t:take 9)
                           (t:map #'1+)
                           (t:split (t:comp (t:filter #'evenp) (t:take 3)) #'+)
                           (t:map #'1-))
           #'t:cons (t:ints 1))))

#+nil
(test 'transducers/tests)
