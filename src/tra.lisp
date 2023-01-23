(defpackage tra
  (:use :cl)
  (:shadow #:map #:remove #:concatenate #:cons))

(in-package :tra)

;; TODO
;; tfilter-map
;; treplace
;; ttake-while
;; tappend-map
;; tdelete-*

;; --- Transducers --- ;;

(defun map (f)
  "Map an F across all elements of the transduction."
  (lambda (reducer)
    (lambda (&optional (result nil r-p) (input nil i-p))
      (cond ((and r-p i-p) (funcall reducer result (funcall f input)))
            ((and r-p (not i-p)) (funcall reducer result))
            (t (funcall reducer))))))

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

(defun remove (pred)
  "Remove elements from the transduction that satisfy PRED."
  (lambda (reducer)
    (lambda (&optional (result nil r-p) (input nil i-p))
      (cond ((and r-p i-p)
             (if (not (funcall pred input))
                 (funcall reducer result input)
                 result))
            ((and r-p (not i-p)) (funcall reducer result))
            (t (funcall reducer))))))

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

(defun concatenate (reducer)
  "Concatenates all the sublists in the transduction."
  (let ((preserving-reducer (preserving-reduced reducer)))
    (lambda (&optional (result nil r-p) (input nil i-p))
      (cond ((and r-p i-p) (list-reduce preserving-reducer result input))
            ((and r-p (not i-p)) (funcall reducer result))
            (t (funcall reducer))))))

(defun flatten (reducer)
  "Entirely flattens any list passed through it."
  (lambda (&optional (result nil r-p) (input nil i-p))
    (cond ((and r-p i-p)
           (if (listp input)
               (list-reduce (preserving-reduced (flatten reducer)) result input)
               (funcall reducer result input)))
          ((and r-p (not i-p)) (funcall reducer result))
          (t '()))))

;; TODO Rename this "interpolate"?
(defun add-between (elem)
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

;; (list-transduce (add-between 0) (rcons) '(1 2 3))

;; --- Reducers --- ;;

(defun cons ()
  "A transducer-friendly consing reducer with '() as the identity."
  (lambda (&optional (acc nil a-p) (input nil i-p))
    (cond ((and a-p i-p) (cl:cons input acc))
          ((and a-p (not i-p)) (reverse acc))
          (t '()))))

;; --- Entry Points --- ;;

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

;; --- Testing --- ;;

;; (defun do-it (items)
;;   ;; (declare (optimize (speed 3) (safety 0)))
;;   (list-transduce (alexandria:compose
;;                    (map #'1+)
;;                    (filter #'evenp)
;;                    (drop 3)
;;                    (take 3))
;;                   (cons)
;;                   items))

;; (do-it '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))

;; (list-transduce (ttake 3) (rcons) '(2 4 6 8 9 1 2))
;; (list-transduce #'concatenate (rcons) '((1 2 3) (4 5 6) (7 8 9)))
;; (list-transduce #'flatten (rcons) '((1 2 3) 1 8 (4 5 6) (7 8 9) 0))
