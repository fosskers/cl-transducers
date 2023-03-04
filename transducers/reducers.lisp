(in-package :transducers)

(declaim (ftype (function (&optional list t) list) cons))
(defun cons (&optional (acc nil a-p) (input nil i-p))
  "Collect all results as a list."
  (cond ((and a-p i-p) (cl:cons input acc))
        ((and a-p (not i-p)) (reverse acc))
        (t '())))

(declaim (ftype (function (&optional list character) *) string))
(defun string (&optional (acc nil a-p) (input #\z i-p))
  "Collect a stream of characters into to a single string."
  (cond ((and a-p i-p) (cl:cons input acc))
        ((and a-p (not i-p)) (cl:concatenate 'cl:string (reverse acc)))
        (t '())))

#+nil
(string-transduce (map #'char-upcase) #'string "hello")

(declaim (ftype (function (&optional list t) *) vector))
(defun vector (&optional (acc nil a-p) (input nil i-p))
  "Collect a stream of values into to a vector."
  (cond ((and a-p i-p) (cl:cons input acc))
        ((and a-p (not i-p)) (cl:concatenate 'cl:vector (reverse acc)))
        (t '())))

#+nil
(vector-transduce (map #'1+) #'vector #(1 2 3))

(declaim (ftype (function (&optional fixnum t) fixnum) count))
(defun count (&optional (acc 0 a-p) (input nil i-p))
  "Count the number of elements that made it through the transduction."
  (declare (ignore input))
  (cond ((and a-p i-p) (1+ acc))
        ((and a-p (not i-p)) acc)
        (t 0)))

#+nil
(transduce #'pass #'count '(1 2 3 4 5))

(defun average (fallback)
  "Calculate the average value of all numeric elements in a transduction. A
FALLBACK must be provided in case no elements made it through the
transduction (thus protecting from division-by-zero)."
  (let ((items 0))
    (lambda (&optional (acc 0 a-p) (input 0 i-p))
      (cond ((and a-p i-p)
             (incf items)
             (+ acc input))
            ((and a-p (not i-p))
             (if (= 0 items) fallback
                 (/ acc items)))
            (t 0)))))

(declaim (ftype (function ((function (t) *)) *) any))
(defun any (pred)
  "Yield non-NIL if any element in the transduction satisfies PRED. Short-circuits
the transduction as soon as the condition is met."
  (lambda (&optional (acc nil a-p) (input nil i-p))
    (cond ((and a-p i-p)
           (let ((test (funcall pred input)))
             (if test
                 (make-reduced :val test)
                 nil)))
          ((and a-p (not i-p)) acc)
          (t nil))))

#+nil
(transduce #'pass (any #'evenp) '(1 3 5 7 9))
#+nil
(transduce #'pass (any #'evenp) '(1 3 5 7 9 2))

(declaim (ftype (function ((function (t) *)) *) all))
(defun all (pred)
  "Yield non-NIL if all elements of the transduction satisfy PRED. Short-circuits
with NIL if any element fails the test."
  (lambda (&optional (acc nil a-p) (input nil i-p))
    (cond ((and a-p i-p)
           (let ((test (funcall pred input)))
             (if (and acc test)
                 test
                 (make-reduced :val nil))))
          ((and a-p (not i-p)) acc)
          (t t))))

#+nil
(transduce #'pass (all #'oddp) '(1 3 5 7 9))
#+nil
(transduce #'pass (all #'oddp) '(1 3 5 7 9 2))

(defun first (seed)
  "Yield the first value of the transduction, or the SEED if there were none."
  (lambda (&optional (acc nil a-p) (input nil i-p))
    (cond ((and a-p i-p) (make-reduced :val input))
          ((and a-p (not i-p)) acc)
          (t seed))))

#+nil
(transduce (filter #'oddp) (first 0) '(2 4 6 7 10))

(defun last (seed)
  "Yield the final value of the transduction, or the SEED if there were none."
  (lambda (&optional (acc nil a-p) (input nil i-p))
    (cond ((and a-p i-p) input)
          ((and a-p (not i-p) acc))
          (t seed))))

#+nil
(transduce #'pass (last 0) '(2 4 6 7 10))

(declaim (ftype (function ((function (t t) *) t) *) fold))
(defun fold (f seed)
  "The fundamental reducer. `fold' creates an ad-hoc reducer based on
a given 2-argument function. A SEED is also required as the initial accumulator
value, which also becomes the return value in case there were no input left in
the transduction.

Functions like `+' and `*' are automatically valid reducers, because they yield
sane values even when given 0 or 1 arguments. Other functions like `max' cannot
be used as-is as reducers since they require at least 2 arguments. For functions
like this, `fold' is appropriate."
  (lambda (&optional (acc nil a-p) (input nil i-p))
    (cond ((and a-p i-p) (funcall f acc input))
          ((and a-p (not i-p)) acc)
          (t seed))))

#+nil
(transduce #'pass (fold #'cl:max 0) '(1 2 3 4 1000 5 6))

(defun max (seed)
  "Yield the maximum value of the transduction, or the SEED if there were none."
  (fold #'cl:max seed))

(defun min (seed)
  "Yield the minimum value of the transduction, or the SEED if there were none."
  (fold #'cl:min seed))

(declaim (ftype (function ((function (t) *)) *) find))
(defun find (pred)
  "Find the first element in the transduction that satisfies a given PRED. Yields
`nil' if no such element were found."
  (lambda (&optional (acc nil a-p) (input nil i-p))
    (cond ((and a-p i-p)
           (if (funcall pred input)
               (make-reduced :val input)
               nil))
          ((and a-p (not i-p)) acc)
          (t nil))))

#+nil
(transduce #'pass (find #'evenp) '(1 3 5 6 9))
