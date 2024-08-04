(in-package :transducers)

(defstruct iter-acc
  (:documentation "Accumulator/state for the `iterator' reducer.

  `iter-acc-i-p' is true if the iterator reducer got an input.

  `iter-acc-input' is one of three possible values; 'UNINITIALIZED if the iterator has not
  yet started, `*done*' if the iterator has finished, or the previous input value
  for the iterator reducer if `iter-acc-i-p' is true.

  `iter-acc-acc' is the accumulator for the wrapped reducer.")
  (i-p nil :type boolean)
  (input nil :type t)
  (acc nil :type t))

(defstruct (iterator (:constructor %make-iterator)
                     (:print-function (lambda (iterator stream depth)
                                        (declare (ignore depth))
                                        (print-unreadable-object (iterator stream :type t :identity t)
                                          (with-slots (input acc) (iterator-acc iterator)
                                            (format stream "acc: ~a, input: ~a" acc input))))))
  (:documentation "Iterating over `iterator-iter' using `iterator-f' as the reducer, keeping state
  in `iterator-acc'.

`initialize-iterator' must be called before the first call to `next' or `next-1'.
`finalize-iterator' must be called after the last call to `next' or `next-1'.
See `with-iterator' for a convenient macro that does this.")
  (acc nil :type iter-acc)
  (f nil :type (function (&optional iter-acc t) iter-acc))
  (iter nil :type source-iter))

(declaim (ftype (function (iterator) t) initialize-iterator))
(defun initialize-iterator (iterator)
  "Initialize the itererators source. See `source-iter-initialize'."
  (funcall (source-iter-initialize (iterator-iter iterator))))

(declaim (ftype (function (iterator) t) finalize-iterator))
(defun finalize-iterator (iterator)
  "Finalize the iteranors source. See `source-iter-finalize'."
  (funcall (source-iter-finalize (iterator-iter iterator))))

(declaim (ftype (function (t t) (function (source-iter) iterator)) make-iterator))
(defun make-iterator (xform f)
  "Create blueprint for an `iterator'.

Returns a function that accepts a `source-iter' and creates an `iterator'."
  (lambda (source)
    (let ((iter-f (lambda (&optional (acc nil a-p) (input nil i-p))
                    (cond ((and a-p i-p) (make-iter-acc :acc (funcall f (iter-acc-acc acc) input)
                                                        :input input
                                                        :i-p i-p))
                          ((and a-p (not i-p)) (make-iter-acc :acc (funcall f (iter-acc-acc acc))
                                                              :input *done*
                                                              :i-p i-p))
                          (t (make-iter-acc :acc (funcall f)
                                            :input 'uninitialized
                                            :i-p i-p))))))
      (%make-iterator :acc (funcall iter-f)
                      :f (funcall xform iter-f)
                      :iter (ensure-source-iter source)))))

(declaim (ftype (function (iterator) (values iter-acc t t)) next-1))
(defun next-1 (iterator)
  "Step one source value for ITERATOR.

If ITERATOR is already marked as done, `*done*' is used as the source value, and
the function returns.

Note that this function does not handle `reduced', so it might be returned
as-is. This is done to allow the caller, e.g. `next' to know the reducer was not
called, i.e. the value was skipped.

Returns three values:
1. The new accumulator after applying `iterator-f' to the previous accumulator
and the source value.
2. The source value as produced by `source-next-1'
3. `*done*' when the source is exhausted"
  (with-slots (acc (source-iter iter)) iterator
    ;; Explicitly handle done. If the iterator has been told to stop, we don't
    ;; want to keep calling the underlying source-iter and keep doing work.
    (let ((input (iter-acc-input acc)))
      (when (eq input *done*)
        (return-from next-1 (values acc *done* *done*))))
    (multiple-value-bind (new-acc source-value done) (source-next-1 (iterator-f iterator)
                                                                    acc
                                                                    (source-iter-next source-iter))
      (declare (ignore source-value))
      (when done
        (setf new-acc (funcall (iterator-f iterator) new-acc)))
      (setf acc new-acc)
      (values acc (iter-acc-input acc) done))))

(declaim (ftype (function (iterator) (values t t t)) next))
(defun next (iterator)
  "Step one value for ITERATOR.

Calls `next-1' until a value is produced (or the source is exhausted).

For each step, returns the following three values:
1. The new accumulator (the underlying accumulator, not the iterators internal state)
2. The source value as produced by `source-next-1'
3. `*done*' when the source is exhausted"
  (let ((acc-before (iterator-acc iterator)))
    (multiple-value-bind (acc-after source-value done) (next-1 iterator)
      (cond
        ;; Done
        (done (values (iter-acc-acc acc-after) source-value done))
        ;; Never called the reducer, so the value never got through
        ((eq acc-before acc-after) (next iterator))
        ;; Got value
        ((iter-acc-i-p acc-after) (values (iter-acc-acc acc-after) source-value done))
        ;; Didn't get value
        (t (next iterator))))))

(defmacro with-iterator (iterator &body body)
  "Initialize and finalize ITERATOR around BODY."
  (let ((it (gensym "WITH-ITERATOR-")))
    `(let ((,it ,iterator))
       (initialize-iterator ,it)
       (unwind-protect (progn ,@body)
         (finalize-iterator ,it)))))

(declaim (ftype (function (iterator) iter-acc) iterator-reduce))
(defun iterator-reduce (iterator)
  "Reduce ITERATOR.

Exhausts the iterator in an `with-iterator' block and returns the final
accumulator."
  (with-iterator iterator
    (labels ((recurse ()
               (multiple-value-bind (acc value done) (next iterator)
                 (declare (ignore value))
                 (if done
                     acc
                     (recurse)))))
      (recurse))))

(defmacro iter* (source &rest transducers-and-reducer)
  `(pipe* iterator ,source ,@transducers-and-reducer))

(defmacro iter (source &rest transducers)
  (if transducers
      `(pipe* iterator ,source ,@transducers #'pass-reducer)
      `(pipe* iterator ,source #'pass #'pass-reducer)))

(defgeneric iterator (xform f source))

(defmethod iterator (xform f source)
  (funcall (make-iterator xform f) source))
