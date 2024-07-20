(defpackage transducers/fset
  (:use :cl)
  (:shadow #:set #:map)
  (:local-nicknames (#:t #:transducers)
                    (#:s #:fset))
  (:documentation "Fset extensions for Transducers."))

(in-package :transducers/fset)

;; --- Reducers --- ;;

(defun set (&optional (acc (s:empty-set) a-p) (input nil i-p))
  "Reducer: Collect all results as an immutable Fset set."
  (cond ((and a-p i-p) (s:with acc input))
        ((and a-p (not i-p)) acc)
        (t (s:empty-set))))

#+nil
(t:transduce #'t:pass #'set '(1 2 3 1))

(defun map (&optional (acc (s:empty-map) a-p) (input nil i-p))
  "Reducer: Collect all results as an immutable Fset map."
  (cond ((and a-p i-p) (destructuring-bind (key . val) input
                         (s:with acc key val)))
        ((and a-p (not i-p)) acc)
        (t (s:empty-map))))

#+nil
(t:transduce #'t:enumerate #'map "hello")

(defun seq (&optional (acc (s:empty-seq) a-p) (input nil i-p))
  "Reducer: Collect all results as an immutable Fset seq."
  (cond ((and a-p i-p) (s:with-last acc input))
        ((and a-p (not i-p)) acc)
        (t (s:empty-seq))))

#+nil
(t:transduce #'t:pass #'seq '(1 2 3 1))

(defun bag (&optional (acc (s:empty-bag) a-p) (input nil i-p))
  "Reducer: Collect all results as an immutable Fset bag."
  (cond ((and a-p i-p) (s:with acc input))
        ((and a-p (not i-p)) acc)
        (t (s:empty-bag))))

#+nil
(t:transduce #'t:pass #'bag '(1 2 1 3 1))
