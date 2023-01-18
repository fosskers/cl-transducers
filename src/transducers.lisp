(defpackage transducers
  (:use :cl)
  (:import-from :trivia lambda-match match))

(in-package :transducers)

(defun rcons ()
  "A transducer-friendly consing reducer with '() as the identity."
  (lambda (&optional acc input)
    (cond ((and acc input) (cons input acc))
          ((and acc (not input)) (reverse acc))
          (t '()))))

(funcall (rcons) '(1 2 3) 4)
