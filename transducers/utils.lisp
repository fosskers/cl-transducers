(in-package :transducers)

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

(declaim (ftype (function ((or t reduced)) reduced) ensure-reduced))
(defun ensure-reduced (x)
  "Ensure that X is reduced."
  (if (reduced? x)
      x
      (reduced x)))

(declaim (ftype (function ((or t reduced)) *) ensure-unreduced))
(defun ensure-unreduced (x)
  "Ensure that X is unreduced."
  (if (reduced? x)
      (reduced-val x)
      x))

(defun preserving-reduced (reducer)
  "A helper function that wraps a reduced value twice since reducing
functions (like list-reduce) unwraps them. `concatenate' is a good example: it
re-uses its reducer on its input using list-reduce. If that reduction finishes
early and returns a reduced value, list-reduce would 'unreduce' that value and
try to continue the transducing process."
  (lambda (a b)
    (let ((result (funcall reducer a b)))
      (if (reduced? result)
          (reduced result)
          result))))

(defun zipmap (keys vals)
  "Form a hashmap with the KEYS mapped to the corresponding VALS.

Borrowed from Clojure, thanks guys."
  (let ((table (make-hash-table :test #'equal)))
    (mapc (lambda (k v) (setf (gethash k table) v)) keys vals)
    table))

#++
(defun split-string (string &key max (separator '(#\Space #\Tab)))
  "Split STRING into a list of components separated by
any of the characters in the sequence SEPARATOR.
If MAX is specified, then no more than max(1,MAX) components will be returned,
starting the separation from the end, e.g. when called with arguments
 \"a.b.c.d.e\" :max 3 :separator \".\" it will return (\"a.b.c\" \"d\" \"e\")."
  (block ()
    (let ((list nil) (words 0) (end (length string)))
      (when (zerop end) (return nil))
      (flet ((separatorp (char) (find char separator))
             (done () (return (cons (subseq string 0 end) list))))
        (loop
          :for start = (if (and max (>= words (1- max)))
                           (done)
                           (position-if #'separatorp string :end end :from-end t))
          :do (when (null start) (done))
              (push (subseq string (1+ start) end) list)
              (incf words)
              (setf end start))))))

(declaim (ftype (function (cl:string &key (:separator character)) list) string-split))
(defun string-split (string &key (separator #\space))
  "You know what this does."
  (labels ((recurse (acc start end)
             (declare (type fixnum start end))
             (cond ((and (<= start 0) (<= end 0)) acc)
                   ;; FIXME: 2025-01-13 This case can probably be simplified.
                   ((and (zerop start) (eql separator (char string start)))
                    (cl:cons "" (cl:cons (subseq string (1+ start) (1+ end)) acc)))
                   ((zerop start) (cl:cons (subseq string start (1+ end)) acc))
                   ((eql separator (char string start))
                    (recurse (cl:cons (subseq string (1+ start) (1+ end)) acc)
                             (1- start)
                             (1- start)))
                   (t (recurse acc (1- start) end)))))
    (let ((end (1- (length string))))
      (recurse '() end end))))

#++
(subseq "hello" 0 2)

#++
(string-split ",Hello,my,name,is,Colin," :separator #\,)

