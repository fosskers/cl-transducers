(in-package :transducers)

(defstruct (generator (:copier nil) (:predicate nil))
  "A wrapper around a function that can potentially yield endless values."
  (func nil :read-only t :type (function () *)))

(defvar *done* 'done
  "A value to signal the end of an unfolding process.")

(defstruct (plist (:copier nil) (:predicate nil))
  (list nil :read-only t :type list))

(defun plist (plist)
  "Source: Yield key-value pairs from a Property List, usually known as a 'plist'."
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
  "Source: Yield an endless stream of random numbers."
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
                                (setf ix (1+ ix))
                                next))))))
        (make-generator :func func))))

#+nil
(transduce (take 10) #'cons (cycle '(1 2 3)))
