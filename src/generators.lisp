(in-package :transducers)

(defstruct generator
  "A wrapper around a function that can potentially yield endless values."
  (func nil :read-only t :type (function () *)))

(defparameter *done* 'done
  "A value to signal the end of an unfolding process.")

;; TODO type signature, expecting `values' to be called within the given
;; function.
(defun unfold (f seed)
  (let* ((curr seed)
         (func (lambda ()
                 (multiple-value-bind (acc next) (funcall f curr)
                   (cond ((eq *done* next) *done*)
                         (t (setf curr acc)
                            next))))))
    (make-generator :func func)))

;; TODO I don't know if I want this in the library.
;; (declaim (ftype (function ((function (t) *) t) generator) iterate))
;; (defun iterate (f seed)
;;   "Yield repeated applications of a function F to some SEED value.

;; (transduce (take 5) #'cons (iterate #'not t))
;; => (T NIL T NIL T)"
;;   (let* ((curr seed)
;;          (func (lambda ()
;;                  (let ((old curr))
;;                    (setf curr (funcall f curr))
;;                    old))))
;;     (make-generator :func func)))

#+nil
(transduce (take 5) #'cons (iterate #'not t))

(declaim (ftype (function (t) generator) repeat))
(defun repeat (item)
  "Endlessly yield a given ITEM."
  (make-generator :func (constantly item)))

#+nil
(transduce (take 4) #'cons (repeat 9))

(declaim (ftype (function (integer &key (:step fixnum)) generator) ints))
(defun ints (start &key (step 1))
  "Yield all integers, beginning with START and advancing by an optional STEP value
which can be positive or negative. If you only want a specific range within the
transduction, then use `take-while' within your transducer chain."
  (let* ((curr start)
         (func (lambda ()
                 (let ((old curr))
                   (setf curr (+ curr step))
                   old))))
    (make-generator :func func)))

#+nil
(transduce (take 10) #'cons (ints 0 :step 2))

(defgeneric cycle (seq)
  (:documentation "Yield the values of a given SEQ endlessly."))

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