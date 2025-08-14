(in-package :transducers)

(define-condition imbalanced-plist (error)
  ((key :initarg :key :reader imbalanced-plist-key))
  (:documentation "A given `plist' source had an uneven number of keys.")
  (:report (lambda (condition stream)
             (format stream "The final key ~a had no value."
                     (imbalanced-plist-key condition)))))

(define-condition empty-transduction (error)
  ((msg :initarg :msg :reader empty-transduction-msg))
  (:documentation "A transduction was empty when it was expected not to be.")
  (:report (lambda (condition stream)
             (format stream "~a~&" (empty-transduction-msg condition)))))

(define-condition non-positive-integer (error)
  ((n  :initarg :n :reader npi-n)
   (fn :initarg :fn :reader npi-fn))
  (:documentation "A non-positive integer was passed to a function that expected one.")
  (:report (lambda (condition stream)
             (format stream "Non-positive integer passed to `~a': ~d"
                     (npi-fn condition)
                     (npi-n condition)))))

(define-condition empty-argument (error)
  ((fn :initarg :fn :reader empty-argument-fn))
  (:documentation "A non-empty sequence was expected, but that didn't stop the user.")
  (:report (lambda (condition stream)
             (format stream "Empty sequence passed to `~a'."
                     (empty-argument-fn condition)))))

(define-condition no-transduce-implementation (error)
  ((type :initarg :type :reader no-transduce-implementation-type))
  (:documentation "The user attempted to call `transduce' on an unsupported type.")
  (:report (lambda (condition stream)
             (format stream "The type ~a cannot be transduced over. Did you mean to pass a list?"
                     (no-transduce-implementation-type condition)))))

(define-condition unusable-type (error)
  ((type :initarg :type :reader unusable-type-type))
  (:documentation "The user attempted to call `concatenate' with a non-list/vector.")
  (:report (lambda (condition stream)
             (format stream "The type ~a cannot be concatenated."
                     (unusable-type-type condition)))))

(define-condition unmatched-closing-paren (error)
  ()
  (:documentation "A character stream contained an unmatched closing parenthesis.")
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Unmatched closing parenthesis in sexp stream"))))

(defun prompt-new-value (prompt)
  (format *query-io* prompt)
  (force-output *query-io*)
  (list (read *query-io*)))

(defun prompt-for-function-name ()
  "Prompt the user for a function name."
  (format *query-io* "Function name: ")
  (force-output *query-io*)
  (let ((input (read *query-io*)))
    (if (and (symbolp input) (fboundp input))
        (list input)
        (error "Not a known function: ~A" input))))
