(defpackage transducers/jzon
  (:use :cl)
  (:shadow #:read #:write)
  (:import-from #:trivia #:match)
  (:local-nicknames (#:t #:transducers)
                    (#:j #:com.inuoe.jzon))
  (:export #:read #:write)
  (:documentation "JSON extensions for Transducers."))

(in-package :transducers/jzon)

(defstruct (json (:copier nil) (:predicate nil))
  "The source of some JSON data."
  (source nil :read-only t :type (or pathname stream string)))

(declaim (ftype (function ((or pathname stream string)) json) read))
(defun read (source)
  "Mark a data SOURCE as being some store of JSON data."
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
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (j:with-parser (parser stream)
    (multiple-value-bind (event value) (j:parse-next parser)
      (declare (ignore value))
      (when (not (eq :begin-array event))
        (error "Given JSON data is not an Array."))
      (labels ((recurse (acc)
                 (match (j:parse-next-element parser :max-depth t
                                                     :eof-error-p nil
                                                     :eof-value :done)
                   (:done acc)
                   (json (let ((acc (t::safe-call f acc json)))
                           (if (t:reduced? acc)
                               (t:reduced-val acc)
                               (recurse acc)))))))
        (recurse identity)))))

#+nil
(t:transduce #'t:pass #'t:cons (read "[{\"name\": \"A\"}, {\"name\": \"B\"}]"))

;; FIXME 2023-05-02 I suspect I'm missing some `unwind-protect' business here!
(declaim (ftype (function (stream &key (:pretty t)) *) write))
(defun write (stream &key (pretty nil))
  "Serialize every value that passes through the transduction into JSON, and
output that JSON into the given STREAM."
  (let ((writer (j:make-writer :stream stream :pretty pretty)))
    (lambda (&optional (acc nil a-p) (input nil i-p))
      (declare (ignore acc))
      (cond ((and a-p i-p)
             (j:write-value writer (if (and (listp input)
                                            (keywordp (car input)))
                                       (t:plist input)
                                       input)))
            ((and a-p (not i-p))
             (j:end-array writer)
             (j:close-writer writer))
            (t (j:begin-array writer))))))

#+nil
(with-output-to-string (stream)
  (t:transduce #'t:pass (write stream) '((:name "Colin" :age 35) (:name "Jack" :age 10))))

#+nil
(with-output-to-string (stream)
  (t:transduce #'t:pass (write stream) (read "[{\"name\": \"A\"}, {\"name\": \"B\"}]")))

#+nil
(with-open-file (stream #p"big.json" :direction :output :if-exists :supersede)
  (t:transduce (t:take 100000000) (write stream :pretty t) (t:ints 0)))

#+nil
(time (t:transduce #'t:pass #'t:count (read #p"big.json")))

#+nil
(t:transduce (t:filter-map (lambda (ht) (gethash "age" ht)))
             #'t:average
             (read "[{\"age\": 34}, {\"age\": 25}]"))

(defmethod j:write-value ((writer j:writer) (value t:plist))
  (j:with-object writer
    (t:transduce #'t:pass
                 (t:for (lambda (pair)
                          (j:write-key writer (car pair))
                          (j:write-value writer (cdr pair))))
                 value)))
