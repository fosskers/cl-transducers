(defpackage transducers
  (:use :cl)
  (:local-nicknames (#:q #:sycamore)
                    (#:s #:fset))
  (:shadow #:map #:concatenate #:log #:step
           #:cons #:count #:first #:last #:max #:min #:find #:string #:vector)
  ;; --- Entry Points --- ;;
  (:export #:transduce)
  ;; --- Transducers -- ;;
  (:export #:map
           #:filter #:filter-map #:unique #:dedup
           #:drop #:drop-while #:take #:take-while
           #:concatenate #:flatten
           #:segment #:window #:group-by
           #:intersperse #:enumerate #:step #:scan
           #:log)
  ;; --- Reducers -- ;;
  (:export #:cons #:vector #:string
           #:count
           #:any #:all
           #:first #:last
           #:fold #:max #:min #:find)
  ;; --- Generators --- ;;
  (:export #:range #:cycle))

(in-package :transducers)

;; TODO
;; pass/void
;;
;; GENERATORS
;; unfold
;; cycle
;; prime-sieve?

;; --- Transducers --- ;;

(defun map (f)
  "Apply a function F to all elements of the transduction."
  (lambda (reducer)
    (lambda (&optional (result nil r-p) (input nil i-p))
      (cond ((and r-p i-p) (funcall reducer result (funcall f input)))
            ((and r-p (not i-p)) (funcall reducer result))
            (t (funcall reducer))))))

#+nil
(list-transduce (map #'1+) #'cons '(1 2 3 4 5))

(defun filter (pred)
  "Only keep elements from the transduction that satisfy PRED."
  (lambda (reducer)
    (lambda (&optional (result nil r-p) (input nil i-p))
      (cond ((and r-p i-p)
             (if (funcall pred input)
                 (funcall reducer result input)
                 result))
            ((and r-p (not i-p)) (funcall reducer result))
            (t (funcall reducer))))))

#+nil
(list-transduce (filter #'evenp) #'cons '(1 2 3 4 5))

(defun filter-map (f)
  "Apply a function F to the elements of the transduction, but only keep results
that are non-nil."
  (lambda (reducer)
    (lambda (&optional (result nil r-p) (input nil i-p))
      (cond ((and r-p i-p)
             (let ((x (funcall f input)))
               (if x
                   (funcall reducer result x)
                   result)))
            ((and r-p (not i-p)) (funcall reducer result))
            (t (funcall reducer))))))

#+nil
(list-transduce (filter-map #'cl:first) #'cons '(() (2 3) () (5 6) () (8 9)))

(declaim (ftype (function (fixnum) *) drop))
(defun drop (n)
  "Drop the first N elements of the transduction."
  (lambda (reducer)
    (let ((new-n (1+ n)))
      (lambda (&optional (result nil r-p) (input nil i-p))
        (cond ((and r-p i-p)
               (setf new-n (1- new-n))
               (if (> new-n 0)
                   result
                   (funcall reducer result input)))
              ((and r-p (not i-p)) (funcall reducer result))
              (t (funcall reducer)))))))

#+nil
(list-transduce (drop 3) #'cons '(1 2 3 4 5))

(defun drop-while (pred)
  "Drop elements from the front of the transduction that satisfy PRED."
  (lambda (reducer)
    (let ((drop? t))
      (lambda (&optional (result nil r-p) (input nil i-p))
        (cond ((and r-p i-p) (if (and (funcall pred input) drop?)
                                 result
                                 (progn (setf drop? nil)
                                        (funcall reducer result input))))
              ((and r-p (not i-p)) (funcall reducer result))
              (t (funcall reducer)))))))

#+nil
(list-transduce (drop-while #'evenp) #'cons '(2 4 6 7 8 9))

(declaim (ftype (function (fixnum) *) take))
(defun take (n)
  "Keep the first N elements of the transduction."
  (lambda (reducer)
    (let ((new-n n))
      (lambda (&optional (result nil r-p) (input nil i-p))
        (cond ((and r-p i-p)
               (let ((result (if (> new-n 0)
                                 (funcall reducer result input)
                                 result)))
                 (setf new-n (1- new-n))
                 (if (not (> new-n 0))
                     (ensure-reduced result)
                     result)))
              ((and r-p (not i-p)) (funcall reducer result))
              (t (funcall reducer)))))))

#+nil
(list-transduce (take 3) #'cons '(1 2 3 4 5))

(defun take-while (pred)
  "Keep only elements which satisfy a given PRED, and stop the transduction as
soon as any element fails the test."
  (lambda (reducer)
    (lambda (&optional (result nil r-p) (input nil i-p))
      (cond ((and r-p i-p)
             (if (not (funcall pred input))
                 (ensure-reduced result)
                 (funcall reducer result input)))
            ((and r-p (not i-p)) (funcall reducer result))
            (t (funcall reducer))))))

#+nil
(list-transduce (take-while #'evenp) #'cons '(2 4 6 8 9 2))

(defun concatenate (reducer)
  "Concatenate all the sublists in the transduction."
  (let ((preserving-reducer (preserving-reduced reducer)))
    (lambda (&optional (result nil r-p) (input nil i-p))
      (cond ((and r-p i-p) (list-reduce preserving-reducer result input))
            ((and r-p (not i-p)) (funcall reducer result))
            (t (funcall reducer))))))

#+nil
(list-transduce #'concatenate #'cons '((1 2 3) (4 5 6) (7 8 9)))

(defun flatten (reducer)
  "Entirely flatten all lists in the transduction, regardless of nesting."
  (lambda (&optional (result nil r-p) (input nil i-p))
    (cond ((and r-p i-p)
           (if (listp input)
               (list-reduce (preserving-reduced (flatten reducer)) result input)
               (funcall reducer result input)))
          ((and r-p (not i-p)) (funcall reducer result))
          (t '()))))

#+nil
(list-transduce #'flatten #'cons '((1 2 3) 0 (4 (5) 6) 0 (7 8 9) 0))

(declaim (ftype (function (fixnum) *) segment))
(defun segment (n)
  "Partition the input into lists of N items. If the input stops, flush any
accumulated state, which may be shorter than N."
  (unless (> n 0)
    (error "The arguments to segment must be a positive integer."))
  (lambda (reducer)
    (let ((i 0)
          (collect '()))
      (lambda (&optional (result nil r-p) (input nil i-p))
        (cond ((and r-p i-p)
               (setf collect (cons collect input))
               (setf i (1+ i))
               (if (< i n)
                   result
                   (let ((next-input (reverse collect)))
                     (setf i 0)
                     (setf collect '())
                     (funcall reducer result next-input))))
              ((and r-p (not i-p))
               (let ((result (if (zerop i)
                                 result
                                 (funcall reducer result (reverse collect)))))
                 (setf i 0)
                 (if (reduced-p result)
                     (funcall reducer (reduced-val result))
                     (funcall reducer result))))
              (t (funcall reducer)))))))

#+nil
(list-transduce (segment 3) #'cons '(1 2 3 4 5))

(defun group-by (f)
  "Group the input stream into sublists via some function F. The cutoff criterion
is whether the return value of F changes between two consecutive elements of the
transduction."
  (lambda (reducer)
    (let ((prev 'nothing)
          (collect '()))
      (lambda (&optional (result nil r-p) (input nil i-p))
        (cond ((and r-p i-p)
               (let ((fout (funcall f input)))
                 (if (or (equal fout prev) (eq prev 'nothing))
                     (progn (setf prev fout)
                            (setf collect (cl:cons input collect))
                            result)
                     (let ((next-input (reverse collect)))
                       (setf prev fout)
                       (setf collect (list input))
                       (funcall reducer result next-input)))))
              ((and r-p (not i-p))
               (let ((result (if (null collect)
                                 result
                                 (funcall reducer result (reverse collect)))))
                 (setf collect '())
                 (if (reduced-p result)
                     (funcall reducer (reduced-val result))
                     (funcall reducer result))))
              (t (funcall reducer)))))))

#+nil
(list-transduce (group-by #'evenp) #'cons '(2 4 6 7 9 1 2 4 6 3))

(defun intersperse (elem)
  "Insert an ELEM between each value of the transduction."
  (lambda (reducer)
    (let ((send-elem? nil))
      (lambda (&optional (result nil r-p) (input nil i-p))
        (cond ((and r-p i-p)
               (if send-elem?
                   (let ((result (funcall reducer result elem)))
                     (if (reduced-p result)
                         result
                         (funcall reducer result input)))
                   (progn (setf send-elem? t)
                          (funcall reducer result input))))
              ((and r-p (not i-p)) (funcall reducer result))
              (t (funcall reducer)))))))

#+nil
(list-transduce (intersperse 0) #'cons '(1 2 3))

(defun enumerate (reducer)
  "Index every value passed through the transduction into a cons pair. Starts at 0."
  (let ((n 0))
    (lambda (&optional (result nil r-p) (input nil i-p))
      (cond ((and r-p i-p)
             (let ((input (cl:cons n input)))
               (setf n (1+ n))
               (funcall reducer result input)))
            ((and r-p (not i-p) (funcall reducer result)))
            (t (funcall reducer))))))

#+nil
(list-transduce #'enumerate #'cons '("a" "b" "c"))

(defun log (logger)
  "Call some LOGGER function for each step of the transduction. The LOGGER must
accept the running results and the current element as input. The original
results of the transduction are passed through as-is."
  (lambda (reducer)
    (lambda (&optional (result nil r-p) (input nil i-p))
      (cond ((and r-p i-p)
             (funcall logger result input)
             (funcall reducer result input))
            ((and r-p (not i-p)) (funcall reducer result))
            (t (funcall reducer))))))

#+nil
(list-transduce (log (lambda (_ n) (format t "Got: ~a~%" n))) #'cons '(1 2 3 4 5))

(declaim (ftype (function (fixnum) *) window))
(defun window (n)
  "Yield N-length windows of overlapping values. This is different from `segment' which
yields non-overlapping windows. If there were fewer items in the input than N,
then this yields nothing."
  (unless (> n 0)
    (error "The arguments to window must be a positive integer."))
  (lambda (reducer)
    (let ((i 0)
          (q (q:make-amortized-queue)))
      (lambda (&optional (result nil r-p) (input nil i-p))
        (cond ((and r-p i-p)
               (setf q (q:amortized-enqueue q input))
               (setf i (1+ i))
               (cond ((< i n) result)
                     ((= i n) (funcall reducer result (q:amortized-queue-list q)))
                     (t (setf q (q:amortized-dequeue q))
                        (funcall reducer result (q:amortized-queue-list q)))))
              ((and r-p (not i-p)) (funcall reducer result))
              (t (funcall reducer)))))))

#+nil
(list-transduce (window 3) #'cons '(1 2 3 4 5))

(defun unique (reducer)
  "Only allow values to pass through the transduction once each.
Stateful; this uses a set internally so could get quite heavy if you're not
careful."
  (let ((set (s:empty-set)))
    (lambda (&optional (result nil r-p) (input nil i-p))
      (cond ((and r-p i-p)
             (if (s:contains? set input)
                 result
                 (progn (setf set (s:with set input))
                        (funcall reducer result input))))
            ((and r-p (not i-p)) (funcall reducer result))
            (t (funcall reducer))))))

#+nil
(list-transduce #'unique #'cons '(1 2 1 3 2 1 2 "abc"))

(defun dedup (reducer)
  "Remove adjacent duplicates from the transduction."
  (let ((prev 'nothing))
    (lambda (&optional (result nil r-p) (input nil i-p))
      (cond ((and r-p i-p)
             (if (equal prev input)
                 result
                 (progn (setf prev input)
                        (funcall reducer result input))))
            ((and r-p (not i-p)) (funcall reducer result))
            (t (funcall reducer))))))

#+nil
(list-transduce #'dedup #'cons '(1 1 1 2 2 2 3 3 3 4 3 3))

(declaim (ftype (function (fixnum) *) step))
(defun step (n)
  "Only yield every Nth element of the transduction. The first element of the
transduction is always included. Therefore:

(transduce (step 2) #'cons '(1 2 3 4 5 6 7 8 9))
=> (1 3 5 7 9)
"
  (when (< n 1)
    (error "The argument to skip must be greater than 0."))
  (lambda (reducer)
    (let ((curr 1))
      (lambda (&optional (result nil r-p) (input nil i-p))
        (cond ((and r-p i-p)
               (if (= 1 curr)
                   (progn (setf curr n)
                          (funcall reducer result input))
                   (progn (setf curr (1- curr))
                          result)))
              ((and r-p (not i-p)) (funcall reducer result))
              (t (funcall reducer)))))))

#+nil
(transduce (step 2) #'cons '(1 2 3 4 5 6 7 8 9))

(declaim (ftype (function ((function (t t) *) t) *) scan))
(defun scan (f seed)
  "Build up successsive values from the results of previous applications of a given
function F.

(transduce (scan #'+ 0) #'cons '(1 2 3 4))
=> (0 1 3 6 10)"
  (lambda (reducer)
    (let ((prev seed))
      (lambda (&optional (result nil r-p) (input nil i-p))
        (cond ((and r-p i-p)
               (let* ((old prev)
                      (result (funcall reducer result old)))
                 (cond ((reduced-p result) result)
                       (t (let ((new (funcall f prev input)))
                            (setf prev new)
                            result)))))
              ((and r-p (not i-p))
               (let ((result (funcall reducer result prev)))
                 (cond ((reduced-p result) (funcall reducer (reduced-val result)))
                       (t (funcall reducer result)))))
              (t (funcall reducer)))))))

#+nil
(transduce (scan #'+ 0) #'cons '(1 2 3 4))
;; #+nil
;; (transduce (alexandria:compose (scan #'+ 0) (take 2))
;;            #'cons '(1 2 3 4))

;; --- Reducers --- ;;

(declaim (ftype (function (&optional list t) list) cons))
(defun cons (&optional (acc nil a-p) (input nil i-p))
  "A transducer-friendly consing reducer with '() as the identity."
  (cond ((and a-p i-p) (cl:cons input acc))
        ((and a-p (not i-p)) (reverse acc))
        (t '())))

(declaim (ftype (function (&optional list character) *) string))
(defun string (&optional (acc nil a-p) (input #\z i-p))
  "Reduce a stream of characters into to a single string."
  (cond ((and a-p i-p) (cl:cons input acc))
        ((and a-p (not i-p)) (cl:concatenate 'cl:string (reverse acc)))
        (t '())))

#+nil
(string-transduce (map #'char-upcase) #'string "hello")

(defun vector (&optional (acc nil a-p) (input #\z i-p))
  "Reduce a stream of values into to a vector."
  (cond ((and a-p i-p) (cl:cons input acc))
        ((and a-p (not i-p)) (cl:concatenate 'cl:vector (reverse acc)))
        (t '())))

#+nil
(vector-transduce (map #'1+) #'vector #(1 2 3))

(declaim (ftype (function (&optional t t) fixnum) count))
(defun count (&optional (acc nil a-p) (input nil i-p))
  "Count the number of elements that made it through the transduction."
  (declare (ignore input))
  (cond ((and a-p i-p) (1+ acc))
        ((and a-p (not i-p)) acc)
        (t 0)))

#+nil
(list-transduce (map #'identity) #'count '(1 2 3 4 5))

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
(list-transduce (map #'identity) (any #'evenp) '(1 3 5 7 9))
#+nil
(list-transduce (map #'identity) (any #'evenp) '(1 3 5 7 9 2))

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
(list-transduce (map #'identity) (all #'oddp) '(1 3 5 7 9))
#+nil
(list-transduce (map #'identity) (all #'oddp) '(1 3 5 7 9 2))

(defun first (seed)
  "Yield the first value of the transduction, or the SEED if there were none."
  (lambda (&optional (acc nil a-p) (input nil i-p))
    (cond ((and a-p i-p) (make-reduced :val input))
          ((and a-p (not i-p)) acc)
          (t seed))))

#+nil
(list-transduce (filter #'oddp) (first 0) '(2 4 6 7 10))

(defun last (seed)
  "Yield the final value of the transduction, or the SEED if there were none."
  (lambda (&optional (acc nil a-p) (input nil i-p))
    (cond ((and a-p i-p) input)
          ((and a-p (not i-p) acc))
          (t seed))))

#+nil
(list-transduce (map #'identity) (last 0) '(2 4 6 7 10))

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
(list-transduce (map #'identity) (fold #'cl:max 0) '(1 2 3 4 1000 5 6))

(defun max (seed)
  "Yield the maximum value of the transduction, or the SEED if there were none."
  (fold #'cl:max seed))

(defun min (seed)
  "Yield the minimum value of the transduction, or the SEED if there were none."
  (fold #'cl:min seed))

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
(list-transduce (map #'identity) (find #'evenp) '(1 3 5 6 9))

;; --- Entry Points --- ;;

;; TODO docs and examples.
(defgeneric transduce (xform f source)
  (:documentation "The entry-point for processing some data source via transductions."))

(defmethod transduce (xform f (source cl:string))
  (string-transduce xform f source))

(defmethod transduce (xform f (source list))
  (list-transduce xform f source))

(defmethod transduce (xform f (source cl:vector))
  (vector-transduce xform f source))

(defmethod transduce (xform f (source hash-table))
  (hash-table-transduce xform f source))

(defmethod transduce (xform f (source pathname))
  (file-transduce xform f source))

(defmethod transduce (xform f (source generator))
  (generator-transduce xform f source))

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
(transduce (map #'identity) #'count #p"/home/colin/history.txt")

(declaim (ftype (function (t t list) *) list-transduce))
(defun list-transduce (xform f coll)
  (let* ((init   (funcall f))
         (xf     (funcall xform f))
         (result (list-reduce xf init coll)))
    (funcall xf result)))

(defun list-reduce (f identity lst)
  (if (null lst)
      identity
      (let ((v (funcall f identity (car lst))))
        (if (reduced-p v)
            (reduced-val v)
            (list-reduce f v (cdr lst))))))

(declaim (ftype (function (t t cl:vector) *) vector-transduce))
(defun vector-transduce (xform f coll)
  (let* ((init   (funcall f))
         (xf     (funcall xform f))
         (result (vector-reduce xf init coll)))
    (funcall xf result)))

(defun vector-reduce (f identity vec)
  (let ((len (length vec)))
    (labels ((recurse (acc i)
               (if (= i len)
                  acc
                  (let ((acc (funcall f acc (aref vec i))))
                    (if (reduced-p acc)
                        (reduced-val acc)
                        (recurse acc (1+ i)))))))
      (recurse identity 0))))

#+nil
(vector-transduce (map #'1+) #'cons #(1 2 3 4 5))

(declaim (ftype (function (t t cl:string) *) string-transduce))
(defun string-transduce (xform f coll)
  (vector-transduce xform f coll))

#+nil
(string-transduce (map #'char-upcase) #'cons "hello")

(declaim (ftype (function (t t hash-table) *) hash-table-transduce))
(defun hash-table-transduce (xform f coll)
  "Transduce over the contents of a given Hash Table."
  (let* ((init   (funcall f))
         (xf     (funcall xform f))
         (result (hash-table-reduce xf init coll)))
    (funcall xf result)))

;; FIXME It may be more correct to pass both the key and value together via
;; `values'.
(defun hash-table-reduce (f identity ht)
  (with-hash-table-iterator (iter ht)
    (labels ((recurse (acc)
               (multiple-value-bind (entry-p key value) (iter)
                 (declare (ignore key))
                 (if (not entry-p)
                     acc
                     (let ((acc (funcall f acc value)))
                       (if (reduced-p acc)
                           (reduced-val acc)
                           (recurse acc)))))))
      (recurse identity))))

#+nil
(let ((hm (make-hash-table :test #'equal)))
  (setf (gethash 'a hm) 1)
  (setf (gethash 'b hm) 2)
  (setf (gethash 'c hm) 3)
  (hash-table-transduce (filter #'evenp) (max 0) hm))

(defun file-transduce (xform f filename)
  "Transduce over the lines of the file named by a FILENAME."
  (let* ((init   (funcall f))
         (xf     (funcall xform f))
         (result (file-reduce xf init filename)))
    (funcall xf result)))

(defun file-reduce (f identity filename)
  (with-open-file (stream filename)
    (labels ((recurse (acc)
               (let ((line (read-line stream nil)))
                 (if (not line)
                     acc
                     (let ((acc (funcall f acc line)))
                       (if (reduced-p acc)
                           (reduced-val acc)
                           (recurse acc)))))))
      (recurse identity))))

#+nil
(file-transduce (map #'identity) #'count "/home/colin/history.txt")

(defun generator-transduce (xform f gen)
  "Transduce over a potentially endless stream of values from a generator GEN."
  (let* ((init   (funcall f))
         (xf     (funcall xform f))
         (result (generator-reduce xf init gen)))
    (funcall xf result)))

(defun generator-reduce (f identity gen)
  (labels ((recurse (acc)
             (let ((val (funcall (generator-func gen))))
               (cond ((eq *done* val) acc)
                     (t (let ((acc (funcall f acc val)))
                          (if (reduced-p acc)
                              (reduced-val acc)
                              (recurse acc))))))))
    (recurse identity)))

;; --- GENERATORS --- ;;

(defstruct generator
  "A wrapper around a function that can potentially yield endless values."
  (func nil :read-only t :type (function () *)))

(defparameter *done* 'done
  "A value to signal the end of an unfolding process.")

(declaim (ftype (function (fixnum fixnum) generator) range))
(defun range (start end)
  "Yield all the numbers from START to END."
  (let* ((curr start)
         (inc (if (< start end) #'1+ #'1-))
         (func (lambda ()
                 (cond ((= curr end) *done*)
                       (t (let ((old curr))
                            (setf curr (funcall inc curr))
                            old))))))
    (make-generator :func func)))

#+nil
(transduce (map #'identity) #'cons (range 0 10))

(declaim (ftype (function (list) generator) cycle))
(defun cycle (seq)
  "Yield the values of a given SEQ endlessly."
  (if (null seq)
      (make-generator :func (lambda () *done*))
      (let* ((curr seq)
             (func (lambda ()
                     (cond ((null curr)
                            (setf curr (cdr seq))
                            (car seq))
                           (t (let ((next (car curr)))
                                (setf curr (cdr curr))
                                next))))))
        (make-generator :func func))))

#+nil
(transduce (take 10) #'cons (cycle '(1 2 3)))

;; --- Other Utilities --- ;;

(defstruct reduced
  "A wrapper that signals that reduction has completed."
  val)

(defun ensure-reduced (x)
  "Ensure that X is reduced."
  (if (reduced-p x)
      x
      (make-reduced :val x)))

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
