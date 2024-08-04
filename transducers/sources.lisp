(in-package :transducers)

(defstruct (generator (:copier nil) (:predicate nil))
  "A wrapper around a function that can potentially yield endless values."
  (func nil :read-only t :type (function () *)))

(defvar *done* 'done
  "A value to signal the end of an unfolding process.")

(defstruct (plist (:copier nil) (:predicate nil))
  (list nil :read-only t :type list))

(declaim (ftype (function (list) plist) plist))
(defun plist (plist)
  "Source: Yield key-value pairs from a Property List, usually known as a 'plist'.
The pairs are passed as a cons cell."
  (make-plist :list plist))

;; FIXME type signature, expecting `values' to be called within the given
;; function.
(declaim (ftype (function ((function (t) *) t) generator) unfold))
(defun unfold (f seed)
  (let* ((curr seed)
         (func (lambda ()
                 (multiple-value-bind (acc next) (funcall f curr)
                   (cond ((eq *done* next) *done*)
                         (t (setf curr acc)
                            next))))))
    (make-generator :func func)))

(declaim (ftype (function (t) generator) repeat))
(defun repeat (item)
  "Source: Endlessly yield a given ITEM."
  (make-generator :func (constantly item)))

#+nil
(transduce (take 4) #'cons (repeat 9))

(declaim (ftype (function (integer &key (:step fixnum)) generator) ints))
(defun ints (start &key (step 1))
  "Source: Yield all integers, beginning with START and advancing by an optional
STEP value which can be positive or negative. If you only want a specific range
within the transduction, then use `take-while' within your transducer chain."
  (let* ((curr start)
         (func (lambda ()
                 (let ((old curr))
                   (setf curr (+ curr step))
                   old))))
    (make-generator :func func)))

#+nil
(transduce (take 10) #'cons (ints 0 :step 2))

(declaim (ftype (function ((or single-float double-float integer)) generator) random))
(defun random (limit)
  "Source: Yield an endless stream of random numbers, based on a given LIMIT."
  (make-generator :func (lambda () (cl:random limit))))

#+nil
(transduce (take 20) #'cons (random 10))

(declaim (ftype (function (cl:vector) generator) shuffle))
(defun shuffle (vec)
  "Source: Endlessly yield random elements from a given vector. Recall also that
strings are vectors too, so:

(transduce (take 5) #'string (shuffle \"Númenor\"))
=> \"mNNrú\"
"
  (let* ((len (length vec))
         (func (lambda () (aref vec (cl:random len)))))
    (make-generator :func func)))

#+nil
(transduce (take 5) #'cons (shuffle #("Colin" "Tamayo" "Natsume")))

(defgeneric cycle (seq)
  (:documentation "Source: Yield the values of a given SEQ endlessly."))

(defmethod cycle ((seq list))
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

(defmethod cycle ((seq cl:vector))
  "This works for strings as well."
  (if (zerop (length seq))
      (make-generator :func (lambda () *done*))
      (let* ((ix 0)
             (len (length seq))
             (func (lambda ()
                     (cond ((>= ix len)
                            (setf ix 1)
                            (aref seq 0))
                           (t (let ((next (aref seq ix)))
                                (incf ix)
                                next))))))
        (make-generator :func func))))

#+nil
(transduce (take 10) #'cons (cycle '(1 2 3)))

(defstruct source-iter (:documentation "An iterator over a source of values.

`source-iter-next' should return the next element in the source, or `*done*'
when finished.  After `source-iter-next' returns `*done*' it should continue to
return `*done*' for each subsequent call without doing any work or fail. Each
value, other than `*done*' must be returned once and only once in the same order
as in the source.

`source-iter-initialize' must be called before `source-iter-next' is called the
first time. This should do required setup of the source, e.g. opening a file.
The function should be idempontent to avoid problems if called more than once.

`source-iter-finalize' must be called after `source-iter-next' returns `*done*'
the first time, or when choosing to abort iteration. This should clean up any
resources allocated by `source-iter-initialize'. The function should be
idempotent to avoid problems if called more than once. `source-iter-next' must
not be called after finalizing.")
  (next (lambda () *done*) :type (function () t))
  (initialize (lambda ()) :type (function () t))
  (finalize (lambda ()) :type (function () t)))

(declaim (ftype (function (list) source-iter) list-iter))
(defun list-iter (list)
  (let ((rest list))
    (make-source-iter :next (lambda ()
                              (if (null rest)
                                  *done*
                                  (pop rest))))))

(declaim (ftype (function (list) source-iter) plist-iter))
(defun plist-iter (lst)
  (let ((items lst))
    (make-source-iter
     :next (lambda ()
             (cond ((null items) *done*)
                   ((null (cdr items))
                    (let ((key (pop items)))
                      (restart-case (error 'imbalanced-plist :key key)
                        (use-value (value)
                          :report "Supply a value for the final key."
                          :interactive (lambda () (prompt-new-value (format nil "Value for key ~a: " key)))
                          (list key value)))))
                   (t (cl:cons (pop items) (pop items))))))))

(declaim (ftype (function (cl:vector) source-iter) vector-iter))
(defun vector-iter (vector)
  (let ((len (length vector))
        (i 0))
    (make-source-iter :next (lambda ()
                              (if (eql i len)
                                  *done*
                                  (prog1 (aref vector i)
                                    (incf i)))))))

(declaim (ftype (function (cl:string) source-iter) string-iter))
(defun string-iter (string)
  (vector-iter string))

(declaim (ftype (function (pathname) source-iter) file-line-iter))
(defun file-line-iter (pathname)
  (let ((file nil))
    (make-source-iter :next (lambda ()
                              (or (read-line file nil) *done*))
                      :initialize (lambda ()
                                    (unless file
                                      (setf file (open pathname))))
                      :finalize (lambda ()
                                  (when (and file (open-stream-p file))
                                    (close file)
                                    (setf file nil))))))

(declaim (ftype (function (stream) source-iter) stream-line-iter))
(defun stream-line-iter (stream)
  (make-source-iter :next (lambda ()
                            (or (read-line stream nil) *done*))))

(declaim (ftype (function (generator) source-iter) generator-iter))
(defun generator-iter (generator)
  (make-source-iter :next (lambda ()
                            (funcall (generator-func generator)))))

(defgeneric source->source-iter (source)
  (:documentation "Constructing a `source-iter' from SOURCE."))

(declaim (ftype (function (t) source-iter) ensure-source-iter))
(defun ensure-source-iter (thing)
  "Calls `source->source-iter' iff THING is not `source-iter-p'"
  (if (source-iter-p thing)
      thing
      (values (source->source-iter thing))))

(defmethod source->source-iter (fallback)
  (error 'no-transduce-implementation :type (type-of fallback)))

(defmethod source->source-iter ((source list))
  (list-iter source))

(defmethod source->source-iter ((source cl:vector))
  (vector-iter source))

(defmethod source->source-iter ((source pathname))
  (file-line-iter source))

(defmethod source->source-iter ((source stream))
  (stream-line-iter source))

(defmethod source->source-iter ((source generator))
  (generator-iter source))

(declaim (ftype (function ((function (t t) t) t (function () t)) (values t t t)) source-next-1))
(defun source-next-1 (f acc next)
  "Fetch and process the next value, i.e. (F ACC (NEXT))

NEXT should be a function as described by `source-iter-next'.
F is a \"reducer\" function for ACC. It can return `reduced' to tell that the
iteration is done.

Returns three values:
1. The result of calling the reducer, F on ACC and the next item. Only called
when the next item is not `*done*'.
2. The item produced by NEXT (see `source-iter-next' for semantics).
3. `*done*' if no item was produced or F decided to stop."
  (let* ((item (funcall next))
         (done (eq item *done*))
         (new-acc (if done
                      acc
                      (safe-call f acc item))))
    (if (reduced-p new-acc)
        (values (reduced-val new-acc) item *done*)
        (values new-acc item (if done *done* nil)))))

(declaim (ftype (function (t t source-iter) *) source-iter-transduce))
(defun source-iter-transduce (xform f source)
  (let* ((init (funcall f))
         (xf (funcall xform f))
         (result (source-iter-reduce xf init source)))
    (funcall xf result)))

(defun source-iter-reduce (xf init source)
  (prog2
      (funcall (source-iter-initialize source))
      (unwind-protect
           (labels ((recurse (acc)
                      (multiple-value-bind (new-acc item done) (source-next-1 xf acc (source-iter-next source))
                        (declare (ignore item))
                        (if done
                            new-acc
                            (recurse new-acc)))))
             (recurse init))
        (funcall (source-iter-finalize source)))))

(declaim (ftype (function (source-iter &rest source-iter) source-iter) multi-iter))
(defun multi-iter (source &rest more-sources)
  (let ((sources (mapcar #'ensure-source-iter (cl:cons source more-sources))))
    (make-source-iter
     :initialize (lambda ()
                   (dolist (source sources)
                     (funcall (source-iter-initialize source))))
     :finalize (lambda ()
                 (dolist (source sources)
                   (funcall (source-iter-finalize source))))
     :next (lambda ()
             (block next
               (let ((result '()))
                 (dolist (source sources (nreverse result))
                   (let ((value (funcall (source-iter-next source))))
                     (if (eq value *done*)
                         (return-from next *done*)
                         (push value result))))))))))
