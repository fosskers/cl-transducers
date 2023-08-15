(in-package :transducers)

;; TODO docs and examples.
(defgeneric transduce (xform f source)
  (:documentation "The entry-point for processing some data source via transductions."))

(defmethod transduce (xform f (source cl:string))
  (string-transduce xform f source))

(defmethod transduce (xform f (source list))
  (list-transduce xform f source))

(defmethod transduce (xform f (source cl:vector))
  (vector-transduce xform f source))

(defmethod transduce (xform f (source cl:hash-table))
  (hash-table-transduce xform f source))

(defmethod transduce (xform f (source pathname))
  (file-transduce xform f source))

(defmethod transduce (xform f (source generator))
  (generator-transduce xform f source))

(defmethod transduce (xform f (source stream))
  (stream-transduce xform f source))

#+nil
(transduce (map #'char-upcase) #'string "hello")
#+nil
(transduce (map #'1+) #'vector '(1 2 3 4))
#+nil
(transduce (map #'1+) #'+ #(1 2 3 4))
#+nil
(let ((hm (make-hash-table :test #'equal)))
  (setf (gethash 'a hm) 1)
  (setf (gethash 'b hm) 2)
  (setf (gethash 'c hm) 3)
  (transduce (filter #'evenp) (max 0) hm))

(declaim (ftype (function (t t list) *) list-transduce))
(defun list-transduce (xform f coll)
  (let* ((init   (funcall f))
         (xf     (funcall xform f))
         (result (list-reduce xf init coll)))
    (funcall xf result)))

(defun list-reduce (f identity lst)
  (if (null lst)
      identity
      (let ((v (funcall f identity (car lst))))
        (if (reduced-p v)
            (reduced-val v)
            (list-reduce f v (cdr lst))))))

(declaim (ftype (function (t t cl:vector) *) vector-transduce))
(defun vector-transduce (xform f coll)
  (let* ((init   (funcall f))
         (xf     (funcall xform f))
         (result (vector-reduce xf init coll)))
    (funcall xf result)))

(defun vector-reduce (f identity vec)
  (let ((len (length vec)))
    (labels ((recurse (acc i)
               (if (= i len)
                  acc
                  (let ((acc (funcall f acc (aref vec i))))
                    (if (reduced-p acc)
                        (reduced-val acc)
                        (recurse acc (1+ i)))))))
      (recurse identity 0))))

#+nil
(vector-transduce (map #'1+) #'cons #(1 2 3 4 5))

(declaim (ftype (function (t t cl:string) *) string-transduce))
(defun string-transduce (xform f coll)
  (vector-transduce xform f coll))

#+nil
(string-transduce (map #'char-upcase) #'cons "hello")

(declaim (ftype (function (t t cl:hash-table) *) hash-table-transduce))
(defun hash-table-transduce (xform f coll)
  "Transduce over the contents of a given Hash Table."
  (let* ((init   (funcall f))
         (xf     (funcall xform f))
         (result (hash-table-reduce xf init coll)))
    (funcall xf result)))

;; FIXME
;;
;; It may be more correct to pass both the key and value together via `values'.
;;
;; Fri Aug 11 21:25:02 2023
;; Here we are entirely ignoring the key.
(defun hash-table-reduce (f identity ht)
  (with-hash-table-iterator (iter ht)
    (labels ((recurse (acc)
               (multiple-value-bind (entry-p key value) (iter)
                 (if (not entry-p)
                     acc
                     (let ((acc (funcall f acc (cl:cons key value))))
                       (if (reduced-p acc)
                           (reduced-val acc)
                           (recurse acc)))))))
      (recurse identity))))

#+nil
(let ((hm (make-hash-table :test #'equal)))
  (setf (gethash 'a hm) 1)
  (setf (gethash 'b hm) 2)
  (setf (gethash 'c hm) 3)
  (hash-table-transduce (filter #'evenp) (max 0) hm))

(defun file-transduce (xform f filename)
  "Transduce over the lines of the file named by a FILENAME."
  (let* ((init   (funcall f))
         (xf     (funcall xform f))
         (result (file-reduce xf init filename)))
    (funcall xf result)))

(defun file-reduce (f identity filename)
  (with-open-file (stream filename)
    (stream-reduce f identity stream)))

#+nil
(file-transduce #'pass #'count "/home/colin/history.txt")

(defun stream-transduce (xform f stream)
  "Transduce over the lines of a given STREAM. Note: Closing the stream is the
responsiblity of the caller!"
  (let* ((init   (funcall f))
         (xf     (funcall xform f))
         (result (stream-reduce xf init stream)))
    (funcall xf result)))

(defun stream-reduce (f identity stream)
  (labels ((recurse (acc)
            (let ((line (read-line stream nil)))
              (if (not line)
                  acc
                  (let ((acc (funcall f acc line)))
                    (if (reduced-p acc)
                        (reduced-val acc)
                        (recurse acc)))))))
   (recurse identity)))

(defun generator-transduce (xform f gen)
  "Transduce over a potentially endless stream of values from a generator GEN."
  (let* ((init   (funcall f))
         (xf     (funcall xform f))
         (result (generator-reduce xf init gen)))
    (funcall xf result)))

(defun generator-reduce (f identity gen)
  (labels ((recurse (acc)
             (let ((val (funcall (generator-func gen))))
               (cond ((eq *done* val) acc)
                     (t (let ((acc (funcall f acc val)))
                          (if (reduced-p acc)
                              (reduced-val acc)
                              (recurse acc))))))))
    (recurse identity)))
