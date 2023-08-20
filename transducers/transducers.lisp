(defpackage transducers
  (:use :cl)
  (:local-nicknames (#:q #:sycamore))
  (:shadow #:map #:concatenate #:log #:step #:split
           #:cons #:count #:first #:last #:max #:min #:find #:string #:vector #:hash-table
           #:random)
  ;; --- Entry Points --- ;;
  (:export #:transduce)
  ;; --- Transducers -- ;;
  (:export #:pass #:map
           #:filter #:filter-map #:unique #:dedup
           #:drop #:drop-while #:take #:take-while
           #:concatenate #:flatten
           #:segment #:window #:group-by
           #:intersperse #:enumerate #:step #:scan
           #:log
           #:once
           #:csv)
  ;; --- Higher Order Transducers --- ;;
  (:export #:branch #:inject #:split)
  ;; --- Reducers -- ;;
  (:export #:cons #:snoc #:vector #:string #:hash-table
           #:count #:average
           #:anyp #:allp #:any #:all
           #:first #:last
           #:fold #:max #:min #:find
           #:for-each)
  ;; --- Generators --- ;;
  (:export #:ints #:cycle #:repeat #:random #:shuffle)
  ;; --- Utilities --- ;;
  (:export #:comp #:const
           #:make-reduced #:reduced-p #:reduced-val
           #:empty-transduction)
  (:documentation "Ergonomic, efficient data processing."))

(in-package :transducers)

(defun pass (reducer)
  "Transducer: Just pass along each value of the transduction. Same in intent with
applying `map' to `identity', but this should be slightly more efficient. It is
at least shorter to type."
  (lambda (result &optional (input nil i-p))
    (if i-p (funcall reducer result input)
        (funcall reducer result))))

(declaim (ftype (function ((function (t) *)) *) map))
(defun map (f)
  "Transducer: Apply a function F to all elements of the transduction."
  (lambda (reducer)
    (lambda (result &optional (input nil i-p))
      (if i-p (funcall reducer result (funcall f input))
          (funcall reducer result)))))

#+nil
(transduce (map #'1+) #'cons '(1 2 3 4 5))

(declaim (ftype (function ((function (t) *)) *) filter))
(defun filter (pred)
  "Transducer: Only keep elements from the transduction that satisfy PRED."
  (lambda (reducer)
    (lambda (result &optional (input nil i-p))
      (if i-p (if (funcall pred input)
                  (funcall reducer result input)
                  result)
          (funcall reducer result)))))

#+nil
(transduce (filter #'evenp) #'cons '(1 2 3 4 5))

(declaim (ftype (function ((function (t) *)) *) filter-map))
(defun filter-map (f)
  "Transducer: Apply a function F to the elements of the transduction, but only
keep results that are non-nil.

(transduce (filter-map #'cl:first) #'cons '(() (2 3) () (5 6) () (8 9)))
=> (2 5 8)
"
  (lambda (reducer)
    (lambda (result &optional (input nil i-p))
      (if i-p (let ((x (funcall f input)))
                (if x
                    (funcall reducer result x)
                    result))
          (funcall reducer result)))))

#+nil
(transduce (filter-map #'cl:first) #'cons '(() (2 3) () (5 6) () (8 9)))

(declaim (ftype (function (fixnum) *) drop))
(defun drop (n)
  "Transducer: Drop the first N elements of the transduction."
  (lambda (reducer)
    (let ((new-n (1+ n)))
      (lambda (result &optional (input nil i-p))
        (cond (i-p
               (setf new-n (1- new-n))
               (if (> new-n 0)
                   result
                   (funcall reducer result input)))
              (t (funcall reducer result)))))))

#+nil
(transduce (drop 3) #'cons '(1 2 3 4 5))

(declaim (ftype (function ((function (t) *)) *) drop-while))
(defun drop-while (pred)
  "Transducer: Drop elements from the front of the transduction that satisfy PRED."
  (lambda (reducer)
    (let ((drop? t))
      (lambda (result &optional (input nil i-p))
        (if i-p (if (and drop? (funcall pred input))
                    result
                    (progn (setf drop? nil)
                           (funcall reducer result input)))
            (funcall reducer result))))))

#+nil
(transduce (drop-while #'evenp) #'cons '(2 4 6 7 8 9))

(declaim (ftype (function (fixnum) *) take))
(defun take (n)
  "Transducer: Keep only the first N elements of the transduction."
  (lambda (reducer)
    (let ((new-n n))
      (lambda (result &optional (input nil i-p))
        (if i-p (let ((result (if (> new-n 0)
                                  (funcall reducer result input)
                                  result)))
                  (setf new-n (1- new-n))
                  (if (<= new-n 0)
                      (ensure-reduced result)
                      result))
            (funcall reducer result))))))

#+nil
(transduce (take 3) #'cons '(1 2 3 4 5))
#+nil
(transduce (take 0) #'cons '(1 2 3 4 5))

(declaim (ftype (function ((function (t) *)) *) take-while))
(defun take-while (pred)
  "Transducer: Keep only elements which satisfy a given PRED, and stop the
transduction as soon as any element fails the test."
  (lambda (reducer)
    (lambda (result &optional (input nil i-p))
      (if i-p (if (not (funcall pred input))
                  (make-reduced :val result)
                  (funcall reducer result input))
          (funcall reducer result)))))

#+nil
(transduce (take-while #'evenp) #'cons '(2 4 6 8 9 2))

(defun concatenate (reducer)
  "Transducer: Concatenate all the sublists in the transduction."
  (let ((preserving-reducer (preserving-reduced reducer)))
    (lambda (result &optional (input nil i-p))
      (if i-p (list-reduce preserving-reducer result input)
          (funcall reducer result)))))

#+nil
(transduce #'concatenate #'cons '((1 2 3) (4 5 6) (7 8 9)))

(defun flatten (reducer)
  "Transducer: Entirely flatten all lists in the transduction, regardless of
nesting."
  (lambda (result &optional (input nil i-p))
    (if i-p (if (listp input)
                (list-reduce (preserving-reduced (flatten reducer)) result input)
                (funcall reducer result input))
        (funcall reducer result))))

#+nil
(transduce #'flatten #'cons '((1 2 3) 0 (4 (5) 6) 0 (7 8 9) 0))

(declaim (ftype (function (fixnum) *) segment))
(defun segment (n)
  "Transducer: Partition the input into lists of N items. If the input stops, flush
any accumulated state, which may be shorter than N."
  (unless (> n 0)
    (error "The arguments to segment must be a positive integer."))
  (lambda (reducer)
    (let ((i 0)
          (collect '()))
      (lambda (result &optional (input nil i-p))
        (cond (i-p
               (setf collect (cl:cons input collect))
               (setf i (1+ i))
               (if (< i n)
                   result
                   (let ((next-input (reverse collect)))
                     (setf i 0)
                     (setf collect '())
                     (funcall reducer result next-input))))
              (t (let ((result (if (zerop i)
                                   result
                                   (funcall reducer result (reverse collect)))))
                   (setf i 0)
                   (if (reduced-p result)
                       (funcall reducer (reduced-val result))
                       (funcall reducer result)))))))))

#+nil
(transduce (segment 3) #'cons '(1 2 3 4 5))

(declaim (ftype (function ((function (t) *)) *) group-by))
(defun group-by (f)
  "Transducer: Group the input stream into sublists via some function F. The cutoff
criterion is whether the return value of F changes between two consecutive elements of the
transduction.

(transduce (group-by #'evenp) #'cons '(2 4 6 7 9 1 2 4 6 3))
=> ((2 4 6) (7 9 1) (2 4 6) (3))
"
  (lambda (reducer)
    (let ((prev 'nothing)
          (collect '()))
      (lambda (result &optional (input nil i-p))
        (if i-p (let ((fout (funcall f input)))
                  (if (or (equal fout prev) (eq prev 'nothing))
                      (progn (setf prev fout)
                             (setf collect (cl:cons input collect))
                             result)
                      (let ((next-input (reverse collect)))
                        (setf prev fout)
                        (setf collect (list input))
                        (funcall reducer result next-input))))
            (let ((result (if (null collect)
                              result
                              (funcall reducer result (reverse collect)))))
              (setf collect '())
              (if (reduced-p result)
                  (funcall reducer (reduced-val result))
                  (funcall reducer result))))))))

#+nil
(transduce (group-by #'evenp) #'cons '(2 4 6 7 9 1 2 4 6 3))

(defun intersperse (elem)
  "Transducer: Insert an ELEM between each value of the transduction."
  (lambda (reducer)
    (let ((send-elem? nil))
      (lambda (result &optional (input nil i-p))
        (if i-p (if send-elem?
                    (let ((result (funcall reducer result elem)))
                      (if (reduced-p result)
                          result
                          (funcall reducer result input)))
                    (progn (setf send-elem? t)
                           (funcall reducer result input)))
            (funcall reducer result))))))

#+nil
(transduce (intersperse 0) #'cons '(1 2 3))

(defun enumerate (reducer)
  "Transducer: Index every value passed through the transduction into a cons pair.
Starts at 0."
  (let ((n 0))
    (lambda (result &optional (input nil i-p))
      (if i-p (let ((input (cl:cons n input)))
                (setf n (1+ n))
                (funcall reducer result input))
          (funcall reducer result)))))

#+nil
(transduce #'enumerate #'cons '("a" "b" "c"))

(defun log (logger)
  "Transducer: Call some LOGGER function for each step of the transduction. The
LOGGER must accept the running results and the current element as input. The
original results of the transduction are passed through as-is."
  (lambda (reducer)
    (lambda (result &optional (input nil i-p))
      (cond (i-p
             (funcall logger result input)
             (funcall reducer result input))
            (t (funcall reducer result))))))

#+nil
(transduce (log (lambda (_ n) (format t "Got: ~a~%" n))) #'cons '(1 2 3 4 5))

(declaim (ftype (function (fixnum) *) window))
(defun window (n)
  "Transducer: Yield N-length windows of overlapping values. This is different from
`segment' which yields non-overlapping windows. If there were fewer items in the
input than N, then this yields nothing."
  (unless (> n 0)
    (error "The arguments to window must be a positive integer."))
  (lambda (reducer)
    (let ((i 0)
          (q (q:make-amortized-queue)))
      (lambda (result &optional (input nil i-p))
        (cond (i-p
               (setf q (q:amortized-enqueue q input))
               (setf i (1+ i))
               (cond ((< i n) result)
                     ((= i n) (funcall reducer result (q:amortized-queue-list q)))
                     (t (setf q (q:amortized-dequeue q))
                        (funcall reducer result (q:amortized-queue-list q)))))
              (t (funcall reducer result)))))))

#+nil
(transduce (window 3) #'cons '(1 2 3 4 5))

(defun unique (reducer)
  "Transducer: Only allow values to pass through the transduction once each.
Stateful; this uses a hash table internally so could get quite heavy if you're
not careful."
  (let ((seen (make-hash-table :test #'equal)))
    (lambda (result &optional (input nil i-p))
      (if i-p (if (gethash input seen)
                  result
                  (progn (setf (gethash input seen) t)
                         (funcall reducer result input)))
          (funcall reducer result)))))

#+nil
(transduce #'unique #'cons '(1 2 1 3 2 1 2 "abc"))

(defun dedup (reducer)
  "Transducer: Remove adjacent duplicates from the transduction."
  (let ((prev 'nothing))
    (lambda (result &optional (input nil i-p))
      (if i-p (if (equal prev input)
                  result
                  (progn (setf prev input)
                         (funcall reducer result input)))
          (funcall reducer result)))))

#+nil
(transduce #'dedup #'cons '(1 1 1 2 2 2 3 3 3 4 3 3))

(declaim (ftype (function (fixnum) *) step))
(defun step (n)
  "Transducer: Only yield every Nth element of the transduction. The first element
of the transduction is always included. Therefore:

(transduce (step 2) #'cons '(1 2 3 4 5 6 7 8 9))
=> (1 3 5 7 9)
"
  (when (< n 1)
    (error "The argument to skip must be greater than 0."))
  (lambda (reducer)
    (let ((curr 1))
      (lambda (result &optional (input nil i-p))
        (if i-p (if (= 1 curr)
                    (progn (setf curr n)
                           (funcall reducer result input))
                    (progn (setf curr (1- curr))
                           result))
            (funcall reducer result))))))

#+nil
(transduce (step 2) #'cons '(1 2 3 4 5 6 7 8 9))

(declaim (ftype (function ((function (t t) *) t) *) scan))
(defun scan (f seed)
  "Transducer: Build up successsive values from the results of previous
applications of a given function F.

(transduce (scan #'+ 0) #'cons '(1 2 3 4))
=> (0 1 3 6 10)"
  (lambda (reducer)
    (let ((prev seed))
      (lambda (result &optional (input nil i-p))
        (if i-p (let* ((old prev)
                       (result (funcall reducer result old)))
                  (cond ((reduced-p result) result)
                        (t (let ((new (funcall f prev input)))
                             (setf prev new)
                             result))))
            (let ((result (funcall reducer result prev)))
              (cond ((reduced-p result) (funcall reducer (reduced-val result)))
                    (t (funcall reducer result)))))))))

#+nil
(transduce (scan #'+ 0) #'cons '(1 2 3 4))
#+nil
(transduce (comp (scan #'+ 0) (take 2)) #'cons '(1 2 3 4))

(defun once (item)
  "Transducer: Inject some ITEM into the front of the transduction."
  (lambda (reducer)
    (let ((item item))
      (lambda (result &optional (input nil i-p))
        (if i-p (if item
                    (let ((res (funcall reducer result item)))
                      (if (reduced-p res)
                          res
                        (progn (setq item nil)
                               (funcall reducer res input))))
                  (funcall reducer result input))
          (funcall reducer result))))))

#+nil
(transduce (comp (filter (lambda (n) (> n 10)))
                 (once 'hi)
                 (take 3))
           #'cons (ints 1))

(defun csv (reducer)
  "Transducer: Interpret the data stream as CSV data.

The first item found is assumed to be the header list, and it
will be used to construct useable hashmaps for all subsequent
items.

Note: This function makes no attempt to convert types from the
original parsed strings. If you want numbers, you will need to
further parse them yourself.

This function is expected to be passed \"bare\" to `transduce', so there is no
need for the caller to manually pass a REDUCER."
  (let ((headers nil))
    (lambda (result &optional (input nil i-p))
      (if i-p (let ((items (split-csv-line input)))
                (if headers (funcall reducer result (zipmap headers items))
                  (progn (setf headers items)
                         result)))
        (funcall reducer result)))))

#+nil
(transduce (comp (once "Name,Age")
                 #'csv
                 (map (lambda (hm) (gethash "Name" hm))))
           #'cons '("Alice,35" "Bob,26"))

(defun split-csv-line (line)
  "Split a LINE of CSV data in a sane way.

This removes any extra whitespace that might be hanging around between elements."
  (mapcar (lambda (s) (string-trim " " s))
          (uiop:split-string line :separator ",")))

;; --- Higher Order Transducers --- ;;

(defun branch (pred ta tb)
  "Transducer: If a PRED yields non-NIL on a value, proceed with transducer chain
TA. Otherwise, follow chain TB. This produces a kind of diamond pattern of data
flow within the transduction:

     /4a-5a-6a\\
1-2-3          7-8-9
     \\4b-5b---/

Assuming that TA here is some composition of three transducer steps, and TB is a
composition of two. Naturally, if you have other steps beyond the fork (Step 7
above), you should make sure that they can handle the return values of both
sides!

(transduce (comp (map #'1+)
                 (branch #'evenp
                         (map (comp #'write-to-string #'1+))
                         (map (const \"Odd!\")))
                 (map #'length))
           #'cons (range 1 6))
=> (1 4 1 4 1)
"
  (lambda (reducer)
    (let ((fa (funcall ta reducer))
          (fb (funcall tb reducer)))
      (lambda (result &optional (input nil i-p))
        (if i-p (if (funcall pred input)
                    (funcall fa result input)
                    (funcall fb result input))
            ;; It _shouldn't_ matter that we're skipping the fork, since if
            ;; no input is left, we want to get access to the "real" reducer
            ;; at the bottom of the composed transducer stack. We know is at
            ;; least as deep as the original reducer we were passed.
            (funcall reducer result))))))

#+nil
(transduce (comp (take 5)
                 (map #'1+)
                 (branch #'evenp
                       (map (comp #'write-to-string #'1+))
                       (map (const "Odd!")))
                 (map #'length))
           #'cons (ints 1))

(defun split (ta ra)
  "Transducer: Split off a new transducer chain, feeding it each input as well. It
reduces on its own given RA reducer. The final result is a cons-cell where the
first value is the result of the original transduction, and the second is that
of the branch."
  (lambda (reducer)
    (let ((fa (funcall ta ra))
          (other-res (funcall ra)))
      (lambda (result &optional (input nil i-p))
        (cond (i-p
               (unless (reduced-p other-res)
                 (setf other-res (funcall fa other-res input)))
               (funcall reducer result input))
              (t (cl:cons (funcall reducer result)
                          (funcall fa (ensure-unreduced other-res)))))))))

#+nil
(transduce (comp (take 9)
                 (map #'1+)
                 (split (comp (filter #'evenp) (take 3)) #'+)
                 (map #'1-))
           #'cons (ints 1))

(defun inject (f)
  "Transducer: For each value in the transduction that actually affects the final
result (tested with `EQ'), inject an extra transduction step into the chain
immediately after this point. Accumulates, such that each new injection appears
before the previous one."
  (lambda (reducer)
    (let ((reducer reducer))
      (lambda (result &optional (input nil i-p))
        (if i-p (let ((new-res (funcall reducer result input)))
                  (if (eq result new-res)
                      new-res
                      (let* ((xform (funcall f input))
                             (next  (funcall xform reducer)))
                        (setf reducer next)
                        new-res)))
            (funcall reducer result))))))

#+nil
(transduce (comp (inject (lambda (prime) (filter (lambda (n) (/= 0 (mod n prime))))))
                 (take 10))
           #'cons (ints 3 :step 2))

#+nil
(transduce (map #'length) #'+ #p"transducers.lisp")

(defun par (f ta tb)
  "Transducer: Traverse two transducer paths at the same time, combining the
results of each path with a given function F before moving on. This is similar
to the `zip' concept from other languages.

Given the following transducer chain:

     /4a-5a-6a\\
1-2-3          7-8-9
     \\4b-5b---/

The function F would be applied right before Step 7. In this case, the function
would have to expect the output types of Step 6a and 5b as its arguments.

Note 1: If either branch yields a 'reduced' value, then the entire chain
short-circuits and that is the value applied to the reducer one final time.

Note 2: This function has potentially non-intuitive behaviour with regards to
functions like `filter' that don't always contribute to the final result. The
function F will only be applied (and thus pass values on) if both branches
produced a new value. If either branch 'died' for a particular value, then so
too will the other branch. If this is undesirable, see the higher-order
transducer `tri' for an alternative.
"
  (lambda (reducer)
    (let ((fa (funcall ta (last *done*)))
          (fb (funcall tb (last *done*))))
      (lambda (result &optional (input nil i-p))
        (if i-p (let ((ra (funcall fa result input))
                      (rb (funcall fb result input)))
                  (cond ((reduced-p ra) ra)
                        ((reduced-p rb) rb)
                        ((eq ra result) result)
                        ((eq rb result) result)
                        (t (funcall reducer result (funcall f ra rb)))))
            (funcall reducer result))))))

#+nil
(transduce (comp (take 2)
                 (map (lambda (n) (* 3 n)))
                 (par (lambda (a b) (+ a b))
                      (comp (map (lambda (a) (* 1000 a))))
                      (comp (map (lambda (b) (* 3 b)))))
                 (map (lambda (n) (* 2 n))))
           #'+ (ints 1))

;; FIXME There is a problem here. If one side reduces early (thus waiting for
;; the other), new input values are lost on the waiting branch. I must decide if
;; this is acceptable.
(defun tri (f ta ra tb rb)
  "The Trident."
  (lambda (reducer)
    (let* ((fa (funcall ta ra))
           (fb (funcall tb rb))
           (a-id (funcall ra))
           (b-id (funcall rb))
           (res-a a-id)
           (res-b b-id))
      (lambda (result &optional (input nil i-p))
        (cond (i-p
               (unless (reduced-p res-a)
                 (setf res-a (funcall fa res-a input)))
               (unless (reduced-p res-b)
                 (setf res-b (funcall fb res-b input)))
               (when (and (reduced-p res-a)
                          (reduced-p res-b))
                 (let* ((fused (funcall f
                                        (funcall fa (reduced-val res-a))
                                        (funcall fb (reduced-val res-b))))
                        (res (funcall reducer result fused)))
                   (setf res-a a-id)
                   (setf res-b b-id)
                   res)))
              (t (funcall reducer result)))))))

#+nil
(transduce (comp (take 19)
                 (map #'1+)
                 (tri #'*
                      (comp (filter #'evenp) (take 3)) #'+
                      (comp (filter #'oddp)  (take 4)) #'*)
                 (map #'1+))
           #'cons
           (ints 1))

