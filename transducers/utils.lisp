(in-package :transducers)

(define-condition imbalanced-plist (error)
  ((key :initarg :key
        :accessor imbalanced-plist-key))
  (:documentation "A given `plist' source had an uneven number of keys.")
  (:report (lambda (condition stream)
             (format stream "The final key ~a had no value." (imbalanced-plist-key condition)))))

(define-condition empty-transduction (error)
  ((msg :initarg :msg
        :accessor empty-transduction-msg))
  (:documentation "A transduction was empty when it was expected not to be.")
  (:report (lambda (condition stream)
             (format stream "~a~&" (empty-transduction-msg condition)))))

(defun prompt-new-value (prompt)
  (format *query-io* prompt)
  (force-output *query-io*)
  (list (read *query-io*)))

(defun ensure-function (arg)
  (cond ((functionp arg) arg)
        ((symbolp arg) (ensure-function (symbol-function arg)))
        (t (error "Argument is not a function: ~a" arg))))

;; TODO Make this a macro.
(defun comp (function &rest functions)
  "Function composition.

(funcall (comp #'1+ #'length) \"foo\") == (1+ (length \"foo\"))"
  (reduce (lambda (f g)
            (let ((f (ensure-function f))
                  (g (ensure-function g)))
              (lambda (&rest arguments)
                (funcall f (apply g arguments)))))
          functions
          :initial-value function))

#+nil
(funcall (comp (const 1337) (lambda (n) (* 2 n)) #'1+) 1)

(defun const (item)
  "Return a function that ignores its argument and returns ITEM instead."
  (lambda (x)
    (declare (ignore x))
    item))

(defstruct reduced
  "A wrapper that signals that reduction has completed."
  val)

(declaim (ftype (function ((or t reduced)) reduced) ensure-reduced))
(defun ensure-reduced (x)
  "Ensure that X is reduced."
  (if (reduced-p x)
      x
      (make-reduced :val x)))

(declaim (ftype (function ((or t reduced)) *) ensure-unreduced))
(defun ensure-unreduced (x)
  "Ensure that X is unreduced."
  (if (reduced-p x)
      (reduced-val x)
      x))

(defun preserving-reduced (reducer)
 "A helper function that wraps a reduced value twice since reducing
functions (like list-reduce) unwraps them. tconcatenate is a good example: it
re-uses its reducer on its input using list-reduce. If that reduction finishes
early and returns a reduced value, list-reduce would 'unreduce' that value and
try to continue the transducing process."
  (lambda (a b)
    (let ((result (funcall reducer a b)))
      (if (reduced-p result)
          (make-reduced :val result)
          result))))

(defun zipmap (keys vals)
  "Form a hashmap with the KEYS mapped to the corresponding VALS.

Borrowed from Clojure, thanks guys."
  (let ((table (make-hash-table :test #'equal)))
    (mapc (lambda (k v) (setf (gethash k table) v)) keys vals)
    table))
