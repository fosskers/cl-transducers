(in-package :transducers)

(declaim (ftype (function (&optional list t) list) cons))
(defun cons (&optional (acc nil a-p) (input nil i-p))
  "Reducer: Collect all results as a list."
  (cond ((and a-p i-p) (cl:cons input acc))
        ((and a-p (not i-p)) (nreverse acc))
        (t '())))

(declaim (ftype (function (&optional list t) list) snoc))
(defun snoc (&optional (acc nil a-p) (input nil i-p))
  "Reducer: Collect all results as a list, but results are reversed.
In theory, slightly more performant than `cons' since it performs no final
reversal."
  (cond ((and a-p i-p) (cl:cons input acc))
        ((and a-p (not i-p)) acc)
        (t '())))

(defun string (&optional (acc nil a-p) (input #\z i-p))
  "Reducer: Collect a stream of characters into to a single string."
  (cond ((and a-p i-p) (cl:cons input acc))
        ((and a-p (not i-p)) (cl:concatenate 'cl:string (nreverse acc)))
        (t '())))

#+nil
(string-transduce (map #'char-upcase) #'string "hello")

(defun vector (&optional (acc nil a-p) (input nil i-p))
  "Reducer: Collect a stream of values into a vector."
  (cond ((and a-p i-p) (cl:cons input acc))
        ((and a-p (not i-p)) (cl:concatenate 'cl:vector (nreverse acc)))
        (t '())))

#+nil
(vector-transduce (map #'1+) #'vector #(1 2 3))

(declaim (ftype (function (&optional (or cl:hash-table null) t) cl:hash-table) hash-table))
(defun hash-table (&optional (acc nil a-p) (input nil i-p))
  "Reducer: Collect a stream of key-value cons pairs into a hash table."
  (cond ((and a-p i-p) (destructuring-bind (key . val) input
                         (setf (gethash key acc) val)
                         acc))
        ((and a-p (not i-p)) acc)
        (t (make-hash-table :test #'equal))))

#+nil
(transduce #'enumerate #'hash-table '("a" "b" "c"))

(declaim (ftype (function (&optional fixnum t) fixnum) count))
(defun count (&optional (acc 0 a-p) (input nil i-p))
  "Reducer: Count the number of elements that made it through the transduction."
  (declare (ignore input))
  (cond ((and a-p i-p) (1+ acc))
        ((and a-p (not i-p)) acc)
        (t 0)))

#+nil
(transduce #'pass #'count '(1 2 3 4 5))

(defun median (&optional (acc nil a-p) (input nil i-p))
  "Reducer: Calculate the median value of all numeric elements in a transduction.
The elements are sorted once before the median is extracted.

# Conditions

- `empty-transduction': when no values made it through the transduction."
  (cond ((and a-p i-p) (cl:cons input acc))
        ((and a-p (not i-p))
         (if (null acc)
             (error 'empty-transduction :msg "`median' called on an empty transduction.")
             ;; HACK 2024-08-22 More robust comparison.
             ;;
             ;; It would be nice if there were a generic way to compare, or if
             ;; the user could pass in a function for this.
             (let* ((cmp    (etypecase (car acc)
                              (cl:string #'string<)
                              (t #'<)))
                    (len    (length acc))
                    (ix     (floor (/ len 2)))
                    (sorted (sort acc cmp)))
               (nth ix sorted))))
        (t '())))

#+nil
(transduce #'pass #'median '(0 1 2 3 4))

(defun average (&optional (acc nil a-p) (input nil i-p))
  "Reducer: Calculate the average value of all numeric elements in a transduction.

# Conditions

- `empty-transduction': when no values made it through the transduction."
  (cond ((and a-p i-p)
         (destructuring-bind (count . total) acc
           (cl:cons (1+ count) (+ total input))))
        ((and a-p (not i-p))
         (destructuring-bind (count . total) acc
           (if (= 0 count)
               (error 'empty-transduction :msg "`average' called on an empty transduction.")
               (/ total count))))
        (t (cl:cons 0 0))))

#+nil
(transduce #'pass #'average '(1 2 3 4 5 6))
#+nil
(transduce (filter #'evenp) #'average '(1 3 5))

(defmacro any (pred)
  "Deprecated: Use `anyp'."
  (warn "`any' is deprecated; use `anyp' instead.")
  `(anyp ,pred))

(declaim (ftype (function ((function (t) *)) *) anyp))
(defun anyp (pred)
  "Reducer: Yield t if any element in the transduction satisfies PRED.
Short-circuits the transduction as soon as the condition is met."
  (lambda (&optional (acc nil a-p) (input nil i-p))
    (cond ((and a-p i-p)
           (let ((test (funcall pred input)))
             (if test
                 (make-reduced :val t)
                 nil)))
          ((and a-p (not i-p)) acc)
          (t nil))))

#+nil
(transduce #'pass (any #'evenp) '(1 3 5 7 9))
#+nil
(transduce #'pass (any #'evenp) '(1 3 5 7 9 2))

(defmacro all (pred)
  "Deprecated: Use `allp'."
  (warn "`all' is deprecated; use `allp' instead.")
  `(allp ,pred))

(declaim (ftype (function ((function (t) *)) *) allp))
(defun allp (pred)
  "Reducer: Yield t if all elements of the transduction satisfy PRED.
Short-circuits with NIL if any element fails the test."
  (lambda (&optional (acc nil a-p) (input nil i-p))
    (cond ((and a-p i-p)
           (let ((test (funcall pred input)))
             (if (and acc test)
                 t
                 (make-reduced :val nil))))
          ((and a-p (not i-p)) acc)
          (t t))))

#+nil
(transduce #'pass (all #'oddp) '(1 3 5 7 9))
#+nil
(transduce #'pass (all #'oddp) '(1 3 5 7 9 2))

(defun first (&optional (acc 'transducers-none a-p) (input nil i-p))
  "Reducer: Yield the first value of the transduction. As soon as this first value
is yielded, the entire transduction stops.

# Conditions

- `empty-transduction': when no values made it through the transduction.
"
  (cond ((and a-p i-p) (make-reduced :val input))
        ((and a-p (not i-p))
         (if (eq 'transducers-none acc)
             (restart-case (error 'empty-transduction :msg "first: the transduction was empty.")
               (use-value (value)
                 :report "Supply a fallback value and end the transduction."
                 :interactive (lambda () (prompt-new-value "Fallback: "))
                 value))
             acc))
        (t 'transducers-none)))

#+nil
(transduce (filter #'oddp) #'first '(2 4 6 7 10))
#+nil
(transduce (filter #'oddp) #'first '(2 4 6 10))

(defun last (&optional (acc 'transducers-none a-p) (input nil i-p))
  "Reducer: Yield the last value of the transduction.

# Conditions

- `empty-transduction': when no values made it through the transduction.
"
  (cond ((and a-p i-p) input)
        ((and a-p (not i-p))
         (if (eq 'transducers-none acc)
             (restart-case (error 'empty-transduction :msg "last: the transduction was empty.")
               (use-value (value)
                 :report "Supply a fallback value and end the transduction."
                 :interactive (lambda () (prompt-new-value "Fallback: "))
                 value))
             acc))
        (t 'transducers-none)))

#+nil
(transduce #'pass #'last '(2 4 6 7 10))
#+nil
(transduce #'pass #'last '())

(declaim (ftype (function ((function (t t) *) &optional t) *) fold))
(defun fold (f &optional (seed nil seed-p))
  "Reducer: The fundamental reducer. `fold' creates an ad-hoc reducer based on
a given 2-argument function. An optional SEED value can also be given as the
initial accumulator value, which also becomes the return value in case there
were no input left in the transduction.

Functions like `+' and `*' are automatically valid reducers, because they yield
sane values even when given 0 or 1 arguments. Other functions like `max' cannot
be used as-is as reducers since they can't be called without arguments. For
functions like this, `fold' is appropriate.

# Conditions

- `empty-transduction': if no SEED is given and the transduction is empty.
"
  (if seed-p
      (lambda (&optional (acc nil a-p) (input nil i-p))
        (cond ((and a-p i-p) (funcall f acc input))
              ((and a-p (not i-p)) acc)
              (t seed)))
      (lambda (&optional (acc nil a-p) (input nil i-p))
        (cond ((and a-p i-p)
               (if (eq acc 'transducers-none)
                   input
                   (funcall f acc input)))
              ((and a-p (not i-p))
               (if (eq acc 'transducers-none)
                   (restart-case (error 'empty-transduction :msg "fold was called without a seed, but the transduction was also empty.")
                     (use-value (value)
                       :report "Supply a default value and end the transduction."
                       :interactive (lambda () (prompt-new-value "Default value: "))
                       value))
                   acc))
              (t 'transducers-none)))))

#+nil
(transduce #'pass (fold #'cl:max) '())
#+nil
(transduce #'pass (fold #'cl:max 0) '(1 2 3 4 1000 5 6))
#+nil
(transduce #'pass (fold #'cl:max) '(1 2 3 4 1000 5 6))

(defun max (default)
  "Deprecated: Use `fold' and pass it `cl:max' instead."
  (warn "`max' is deprecated; use `fold' instead.")
  (fold #'cl:max default))

(defun min (default)
  "Deprecated: Use `fold' and pass it `cl:min' instead."
  (warn "`min' is deprecated; use `fold' instead.")
  (fold #'cl:min default))

(declaim (ftype (function ((function (t) *) &key (:default t)) *) find))
(defun find (pred &key default)
  "Reducer: Find the first element in the transduction that satisfies a given PRED.
Yields `nil' if no such element were found."
  (lambda (&optional (acc nil a-p) (input nil i-p))
    (cond ((and a-p i-p)
           (if (funcall pred input)
               (make-reduced :val input)
               default))
          ((and a-p (not i-p)) acc)
          (t default))))

#+nil
(transduce #'pass (find #'evenp) '(1 3 5 6 9))
#+nil
(transduce #'pass (find #'evenp :default 1000) '(1 3 5 9))

(defun for-each (&rest vargs)
  "Reducer: Run through every item in a transduction for their side effects.
Throws away all results and yields t."
  (declare (ignore vargs))
  t)

#+nil
(transduce (map (lambda (n) (format t "~a~%" n))) #'for-each #(1 2 3 4))
