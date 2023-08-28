(in-package :transducers)

(define-condition imbalanced-plist (error)
  ((key :initarg :key
        :accessor imbalanced-plist-key))
  (:documentation "A given `plist' source had an uneven number of keys.")
  (:report (lambda (condition stream)
             (format stream "The final key ~a had no value." (imbalanced-plist-key condition)))))

(define-condition empty-transduction (error)
  ((msg :initarg :msg
        :accessor empty-transduction-msg))
  (:documentation "A transduction was empty when it was expected not to be.")
  (:report (lambda (condition stream)
             (format stream "~a~&" (empty-transduction-msg condition)))))

(defun prompt-new-value (prompt)
  (format *query-io* prompt)
  (force-output *query-io*)
  (list (read *query-io*)))
