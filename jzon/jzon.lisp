(defpackage transducers-jzon
  (:use :cl)
  (:shadow #:read #:write)
  (:import-from #:trivia #:match)
  (:local-nicknames (#:t #:transducers)
                    (#:j #:com.inuoe.jzon))
  (:documentation "JSON extensions for Transducers."))

(in-package :transducers-jzon)

(defstruct json
  "The source of some JSON data."
  (source nil :read-only t :type (or pathname stream string)))

(declaim (ftype (function ((or pathname stream string)) json) read))
(defun read (source)
  "Mark a data SOURCE as being some store of csv data."
  (make-json :source source))

(defmethod t:transduce (xform f (source json))
  (json-transduce xform f (json-source source)))

(declaim (ftype (function (t t (or pathname stream string)) *) json-transduce))
(defun json-transduce (xform f source)
  (let* ((init (funcall f))
         (xf   (funcall xform f)))
    (etypecase source
      (stream (funcall xf (json-reduce xf init source)))
      (pathname (with-open-file (stream source)
                  (funcall xf (json-reduce xf init stream))))
      (string (with-input-from-string (stream source)
                (funcall xf (json-reduce xf init stream)))))))

(declaim (ftype (function (t t stream) *) json-reduce))
(defun json-reduce (f identity stream)
  (j:with-parser (parser stream)
    (multiple-value-bind (event value) (j:parse-next parser)
      (declare (ignore value))
      (when (not (eq :begin-array event))
        (error "Given JSON data is not an Array."))
      (labels ((recurse (acc)
                 (match (j:parse-next-element parser :max-depth 2
                                                     :eof-error-p nil
                                                     :eof-value :done)
                   (:done acc)
                   (json (let ((acc (funcall f acc json)))
                           (if (t:reduced-p acc)
                               (t:reduced-val acc)
                               (recurse acc)))))))
        (recurse identity)))))

#+nil
(t:transduce #'t:pass #'t:cons (read "[{\"name\": \"A\"}, {\"name\": \"B\"}]"))

