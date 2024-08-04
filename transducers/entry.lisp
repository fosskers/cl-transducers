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

(defmacro pipe* (fn source &rest transducers-and-reducer)
  (let ((transducers (butlast transducers-and-reducer))
        (reducer (car (cl:last transducers-and-reducer))))
    (if (cdr transducers)
        `(,fn (comp ,@transducers) ,reducer ,source)
        `(,fn ,(car transducers) ,reducer ,source))))

(defmacro pipe (source &rest transducers-and-reducer)
  "Structure `transduce' as a pipeline.

The second up to (but not including) the last argument is used as the
transducers, and the last argument is used as the reducer. If there are more
than one transducer, they are wrapped in `comp'.

(macroexpand-1 '(pipe source t1 reducer))
;; => (TRANSDUCE T1 REDUCER SOURCE), T

(macroexpand-1 '(pipe source t1 t2 reducer))
;; => (TRANSDUCE (COMP T1 T2) REDUCER SOURCE), T
"
  (assert (>= (length transducers-and-reducer) 2) nil
          "Missing transducer or reducer.")
  `(pipe* transduce ,source ,@transducers-and-reducer))

#+nil
(pipe '(1 2 3) (take 1) #'*)
#+nil
(pipe (cycle '(1 2 3)) (filter #'oddp) (map #'1+) (take 10) #'*)

(defun make-transducer (xform f)
  (lambda (source)
    (source-iter-transduce xform f (ensure-source-iter source))))

(defmethod transduce (xform f (source source-iter))
  (funcall (make-transducer xform f) source))

(defmethod transduce (xform f (source list))
  "Transducing over an alist works automatically via this method, and the pairs are
streamed as-is as cons cells."
  (funcall (make-transducer xform f) (list-iter source)))

(defmethod transduce (xform f (source cl:string))
  (funcall (make-transducer xform f) (string-iter source)))

(defmethod transduce (xform f (source cl:vector))
  (funcall (make-transducer xform f) (vector-iter source)))

(defmethod transduce (xform f (source pathname))
  "Transduce over the lines of the file named by a FILENAME."
  (funcall (make-transducer xform f) (file-line-iter source)))

(defmethod transduce (xform f (source stream))
  "Transduce over the lines of a given STREAM. Note: Closing the stream is the
responsiblity of the caller!"
  (funcall (make-transducer xform f) (stream-line-iter source)))

(defmethod transduce (xform f (source cl:hash-table))
  "Yields key-value pairs as cons cells."
  (hash-table-transduce xform f source))

(defmethod transduce (xform f (source plist))
  "Yields key-value pairs as cons cells.

# Conditions

- `imbalanced-pist': if the number of keys and values do not match."
  (funcall (make-transducer xform f) (plist-iter (plist-list source))))

(defmethod transduce (xform f source)
  "Fallback for types which don't implement this. Always errors.

# Conditions

- `no-transduce-implementation': an unsupported type was transduced over."
  (funcall (make-transducer xform f) (source->source-iter source)))

(declaim (ftype (function (t t cl:hash-table) *) hash-table-transduce))
(defun hash-table-transduce (xform f coll)
  "Transduce over the contents of a given Hash Table."
  (let* ((init   (funcall f))
         (xf     (funcall xform f))
         (result (hash-table-reduce xf init coll)))
    (funcall xf result)))

(defun hash-table-reduce (f identity ht)
  (with-hash-table-iterator (iter ht)
    (labels ((recurse (acc)
               (multiple-value-bind (entry-p key value) (iter)
                 (if (not entry-p)
                     acc
                     (let ((acc (safe-call f acc (cl:cons key value))))
                       (if (reduced-p acc)
                           (reduced-val acc)
                           (recurse acc)))))))
      (recurse identity))))

#+nil
(transduce (map #'1+) #'cons #(1 2 3 4 5))

#+nil
(transduce #'pass #'cons (plist `(:a 1 :b 2 :c 3)))
#+nil
(transduce (map #'car) #'cons (plist `(:a 1 :b 2 :c 3)))
#+nil
(transduce (map #'cdr) #'+ (plist `(:a 1 :b 2 :c)))  ;; Imbalanced plist for testing.
#+nil
(transduce #'pass #'cons '((:a . 1) (:b . 2) (:c . 3)))
#+nil
(transduce (map #'char-upcase) #'string "hello")
#+nil
(transduce (map #'1+) #'vector '(1 2 3 4))
#+nil
(vector-transduce (map #'1+) #'cons #(1 2 3 4 5))
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
#+nil
(transduce (map (lambda (item) (if (= item 1) (error "無念") item)))
           #'cons '(0 1 2 3))
#+nil
(transduce #'pass #'count #p"/home/colin/history.txt")
#+nil
(vector-transduce (map #'1+) #'cons #(1 2 3 4 5))
#+nil
(with-open-file (stream #p"/home/colin/.sbclrc")
  (transduce #'pass #'count stream))
