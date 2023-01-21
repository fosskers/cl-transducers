(defpackage transducers
  (:use :cl))

(in-package :transducers)

(defun tmap (f)
  "Map an F across all elements of the transduction."
  (lambda (reducer)
    (lambda (&optional (result nil r-p) (input nil i-p))
      (cond ((and r-p i-p) (funcall reducer result (funcall f input)))
            ((and r-p (not i-p)) (funcall reducer result))
            (t (funcall reducer))))))

(defun tfilter (pred)
  "Only keep elements from the transduction that satisfy PRED."
  (lambda (reducer)
    (lambda (&optional (result nil r-p) (input nil i-p))
      (cond ((and r-p i-p)
             (if (funcall pred input)
                 (funcall reducer result input)
                 result))
            ((and r-p (not i-p)) (funcall reducer result))
            (t (funcall reducer))))))

(defun rcons ()
  "A transducer-friendly consing reducer with '() as the identity."
  (lambda (&optional (acc nil a-p) (input nil i-p))
    (cond ((and a-p i-p) (cons input acc))
          ((and a-p (not i-p)) (reverse acc))
          (t '()))))

(defun list-transduce (xform f coll)
  (list-transduce-work xform f (funcall f) coll))

(defun list-transduce-work (xform f init coll)
  (let* ((xf (funcall xform f))
         (result (list-reduce xf init coll)))
    (funcall xf result)))

(defun list-reduce (f identity lst)
  (if (null lst)
      identity
      (let ((v (funcall f identity (car lst))))
        (if (reduced-p v)
            (reduced-val v)
            (list-reduce f v (cdr lst))))))

(defstruct reduced
  "A wrapper that signals that reduction has completed."
  val)

(defun do-it (items)
  ;; (declare (optimize (speed 3) (safety 0)))
  (list-transduce (alexandria:compose (tmap2 #'1+)
                                      (tfilter #'evenp))
                  (rcons)
                  items))

(do-it '(1 2 3 4 5))
