(in-package :transducers)

(defgeneric transduce (xform f source)
  (:documentation "The entry-point for processing some data source via transductions.

This requires three things:

- A transducer function, or a composed chain of them
- A reducing function
- A source

Note: `comp' can be used to chain transducers together.

When ran, `transduce' will pull values from the source, transform them via the
transducers, and reduce into some single value (likely some collection but not
necessarily). `transduce' will only pull as many values from the source as are
actually needed, and does so one at a time. This ensures that large
sources (like files) don't consume too much memory.

# Examples

Assuming that you've required this library with a local nickname of `t', here's
how we can filter an infinite source and reduce into a single sum:

(t:transduce (t:comp (t:filter #'oddp)
                     (t:take 1000)
                     (t:map (lambda (n) (* n n))))
             #'+ (t:ints 1))
;; => 1333333000 (31 bits, #x4F790C08)

Note that due to how transducer and reducer functions are composed internally,
the order provided to `comp' gets applied from top to bottom. In the above
example, this means that `filter' is applied first, and `map' last.

There are a variety of functions to instead reduce into a collection:

(t:transduce (t:map #'1+) #'t:vector '(1 2 3))
;; => #(2 3 4)

Many standard collections can be easily \"sourced\", including those that aren't
normally so conveniently traversed like Hash Tables, Property Lists, and lines
of a file.

;; Read key-value pairs from a plist and recollect into a Hash Table.
(t:transduce #'t:pass #'t:hash-table (t:plist `(:a 1 :b 2 :c 3)))

# Custom Sources

Since `transduce' is generic, you can use `defmethod' to define your own custom
sources. See `sources.lisp' and `entry.lisp' for examples of how to do this.

"))

(defmethod transduce (xform f (source cl:string))
  (string-transduce xform f source))

(defmethod transduce (xform f (source list))
  "Transducing over an alist works automatically via this method, and the pairs are
streamed as-is as cons cells."
  (list-transduce xform f source))

(defmethod transduce (xform f (source cl:vector))
  (vector-transduce xform f source))

(defmethod transduce (xform f (source reversed))
  (reversed-transduce xform f source))

(defmethod transduce (xform f (source cl:hash-table))
  "Yields key-value pairs as cons cells."
  (hash-table-transduce xform f source))

(defmethod transduce (xform f (source pathname))
  (file-transduce xform f source))

(defmethod transduce (xform f (source generator))
  (generator-transduce xform f source))

(defmethod transduce (xform f (source stream))
  (stream-transduce xform f source))

(defmethod transduce (xform f (source plist))
  "Yields key-value pairs as cons cells.

# Conditions

- `imbalanced-pist': if the number of keys and values do not match."
  (plist-transduce xform f source))

(defmethod transduce (xform f fallback)
  "Fallback for types which don't implement this. Always errors.

# Conditions

- `no-transduce-implementation': an unsupported type was transduced over."
  (declare (ignore xform f))
  (error 'no-transduce-implementation :type (type-of fallback)))

#+nil
(transduce (map #'char-upcase) #'string "hello")
#+nil
(transduce (map #'1+) #'vector '(1 2 3 4))
#+nil
(transduce (map #'1+) #'+ #(1 2 3 4))
#+nil
(let ((hm (make-hash-table :test #'equal)))
  (setf (gethash 'a hm) 1)
  (setf (gethash 'b hm) 2)
  (setf (gethash 'c hm) 3)
  (transduce (filter #'evenp) (max 0) hm))
#+nil
(transduce (map #'1+) #'+ 1)  ;; Expected to fail.

(declaim (ftype (function (t t list) *) list-transduce))
(defun list-transduce (xform f coll)
  (let* ((init   (funcall f))
         (xf     (funcall xform f))
         (result (list-reduce xf init coll)))
    (funcall xf result)))

(declaim (ftype (function ((function (&optional t t) *) t list) *) list-reduce))
(defun list-reduce (f identity lst)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (labels ((recurse (acc items)
             (if (null items)
                 acc
                 (let ((v (safe-call f acc (car items))))
                   (if (reduced? v)
                       (reduced-val v)
                       (recurse v (cdr items)))))))
    (recurse identity lst)))

#+nil
(transduce (map (lambda (item) (if (= item 1) (error "無念") item)))
           #'cons '(0 1 2 3))

(declaim (ftype (function (t t cl:vector) *) vector-transduce))
(defun vector-transduce (xform f coll)
  (let* ((init   (funcall f))
         (xf     (funcall xform f))
         (result (vector-reduce xf init coll)))
    (funcall xf result)))

(defun vector-reduce (f identity vec)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((len (length vec)))
    (labels ((recurse (acc i)
               (if (= i len)
                   acc
                   (let ((acc (safe-call f acc (aref vec i))))
                     (if (reduced? acc)
                         (reduced-val acc)
                         (recurse acc (1+ i)))))))
      (recurse identity 0))))

#+nil
(vector-transduce (map #'1+) #'cons #(1 2 3 4 5))

(defun reversed-transduce (xform f coll)
  (let* ((init   (funcall f))
         (xf     (funcall xform f))
         (result (reversed-reduce xf init coll)))
    (funcall xf result)))

(defun reversed-reduce (f identity rev)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let* ((vec (reversed-vector rev))
         (len (length vec)))
    (labels ((recurse (acc i)
               (if (< i 0)
                   acc
                   (let ((acc (safe-call f acc (aref vec i))))
                     (if (reduced? acc)
                         (reduced-val acc)
                         (recurse acc (1- i)))))))
      (recurse identity (1- len)))))

#+nil
(transduce #'pass #'cons (reversed (cl:vector 1 2 3)))

(declaim (ftype (function (t t cl:string) *) string-transduce))
(defun string-transduce (xform f coll)
  (vector-transduce xform f coll))

#+nil
(string-transduce (map #'char-upcase) #'cons "hello")

(declaim (ftype (function (t t cl:hash-table) *) hash-table-transduce))
(defun hash-table-transduce (xform f coll)
  "Transduce over the contents of a given Hash Table."
  (let* ((init   (funcall f))
         (xf     (funcall xform f))
         (result (hash-table-reduce xf init coll)))
    (funcall xf result)))

(defun hash-table-reduce (f identity ht)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (with-hash-table-iterator (iter ht)
    (labels ((recurse (acc)
               (multiple-value-bind (entry-p key value) (iter)
                 (if (not entry-p)
                     acc
                     (let ((acc (safe-call f acc (cl:cons key value))))
                       (if (reduced? acc)
                           (reduced-val acc)
                           (recurse acc)))))))
      (recurse identity))))

#+nil
(let ((hm (make-hash-table :test #'equal)))
  (setf (gethash 'a hm) 1)
  (setf (gethash 'b hm) 2)
  (setf (gethash 'c hm) 3)
  (hash-table-transduce (comp (map #'cdr) (filter #'evenp)) (fold #'cl:max 0) hm))

(defun file-transduce (xform f filename)
  "Transduce over the lines of the file named by a FILENAME."
  (let* ((init   (funcall f))
         (xf     (funcall xform f))
         (result (file-reduce xf init filename)))
    (funcall xf result)))

(defun file-reduce (f identity filename)
  (with-open-file (stream filename)
    (stream-reduce f identity stream)))

#+nil
(file-transduce #'pass #'count #p"/home/colin/history.txt")

(defun stream-transduce (xform f stream)
  "Transduce over the lines of a given STREAM. Note: Closing the stream is the
responsiblity of the caller!"
  (let* ((init   (funcall f))
         (xf     (funcall xform f))
         (result (stream-reduce xf init stream)))
    (funcall xf result)))

(defun stream-reduce (f identity stream)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (labels ((recurse (acc)
             (let ((line (read-line stream nil)))
               (if (not line)
                   acc
                   (let ((acc (safe-call f acc line)))
                     (if (reduced? acc)
                         (reduced-val acc)
                         (recurse acc)))))))
    (recurse identity)))

#+nil
(with-open-file (stream #p"/home/colin/.sbclrc")
  (transduce #'pass #'count stream))

(defun generator-transduce (xform f gen)
  "Transduce over a potentially endless stream of values from a generator GEN."
  (let* ((init   (funcall f))
         (xf     (funcall xform f))
         (result (generator-reduce xf init gen)))
    (funcall xf result)))

(defun generator-reduce (f identity gen)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (labels ((recurse (acc)
             (let ((val (funcall (generator-func gen))))
               (cond ((eq *done* val) acc)
                     (t (let ((acc (safe-call f acc val)))
                          (if (reduced? acc)
                              (reduced-val acc)
                              (recurse acc))))))))
    (recurse identity)))

(declaim (ftype (function (t t plist) *) plist-transduce))
(defun plist-transduce (xform f coll)
  (let* ((init   (funcall f))
         (xf     (funcall xform f))
         (result (plist-reduce xf init coll)))
    (funcall xf result)))

(declaim (ftype (function ((function (&optional t t) *) t plist) *) plist-reduce))
(defun plist-reduce (f identity lst)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (labels ((recurse (acc items)
             (cond ((null items) acc)
                   ((null (cdr items))
                    (let ((key (car items)))
                      (restart-case (error 'imbalanced-plist :key key)
                        (use-value (value)
                          :report "Supply a value for the final key."
                          :interactive (lambda () (prompt-new-value (format nil "Value for key ~a: " key)))
                          (recurse acc (list key value))))))
                   (t (let ((v (safe-call f acc (cl:cons (car items) (second items)))))
                        (if (reduced? v)
                            (reduced-val v)
                            (recurse v (cdr (cdr items)))))))))
    (recurse identity (plist-list lst))))

#+nil
(transduce #'pass #'cons (plist `(:a 1 :b 2 :c 3)))
#+nil
(transduce (map #'car) #'cons (plist `(:a 1 :b 2 :c 3)))
#+nil
(transduce (map #'cdr) #'+ (plist `(:a 1 :b 2 :c)))  ;; Imbalanced plist for testing.
#+nil
(transduce #'pass #'cons '((:a . 1) (:b . 2) (:c . 3)))
