(defpackage transducers
  (:use :cl)
  (:import-from :trivia lambda-match match))

(in-package :transducers)

(defun tmap (f)
  "Map an F across all elements of the transduction."
  (lambda (reducer)
    (lambda (&optional (result :tmap-res) (input :tmap-input))
      (cond ((and (not (eq result :tmap-res))
                  (not (eq input :tmap-input)))
             (funcall reducer result (funcall f input)))
            ((and result (eq input :tmap-input))
             (funcall reducer result))
            (t (funcall reducer))))))

(defun tfilter (pred)
  (lambda (reducer)
    (lambda (&optional (result :tfilter-res) (input :tfilter-input))
      (cond ((and (not (eq result :tfilter-res))
                  (not (eq input :tfilter-input)))
             (if (funcall pred input)
                 (funcall reducer result input)
                 result))
            ((and result (eq input :tfilter-input))
             (funcall reducer result))
            (t (funcall reducer))))))

(defun rcons ()
  "A transducer-friendly consing reducer with '() as the identity."
  (lambda (&optional (acc :rcons-acc) (input :rcons-input))
    (cond ((and (not (eq acc :rcons-acc))
                (not (eq input :rcons-input))) (cons input acc))
          ((and (not (eq acc :rcons-acc))
                (eq input :rcons-input)) (reverse acc))
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

;; (list-transduce (alexandria:compose (tmap #'1+)
;;                                     (tfilter #'evenp))
;;                 (rcons)
;;                 '(1 2 3 4 5))
