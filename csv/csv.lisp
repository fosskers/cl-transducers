(defpackage transducers-csv
  (:use :cl)
  (:shadow #:read)
  (:local-nicknames (#:t #:transducers))
  (:documentation "CSV extensions for Transducers."))

(in-package :transducers-csv)

(defstruct csv
  "The source of some CSV data."
  (source nil :read-only t :type (or pathname stream)))

(declaim (ftype (function ((or pathname stream)) csv) read))
(defun read (source)
  "Mark a data SOURCE as being some store of csv data."
  (make-csv :source source))

(defmethod t:transduce (xform f (source csv))
  (csv-transduce xform f (csv-source source)))

(declaim (ftype (function (t t (or pathname stream)) *) csv-transduce))
(defun csv-transduce (xform f source)
  (let* ((init   (funcall f))
         (xf     (funcall xform f)))
    (cond ((typep source 'stream) (funcall xf (csv-reduce xf init source)))
          ((typep source 'pathname)
           (with-open-file (stream source)
             (funcall xf (csv-reduce xf init stream)))))))

(declaim (ftype (function (t t stream) *) csv-reduce))
(defun csv-reduce (f identity stream)
  (labels ((recurse (acc)
             (let ((line (read-line stream nil)))
               (if (not line)
                   acc
                   (let* ((parts (str:split #\, line))
                          (acc (funcall f acc parts)))
                     (if (t:reduced-p acc)
                         (t:reduced-val acc)
                         (recurse acc)))))))
    (recurse identity)))

#+nil
(t:transduce #'t:pass #'t:cons (read #p"foo.csv"))
