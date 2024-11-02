(defpackage transducers
  (:use :cl)
  (:shadow #:map #:concatenate #:log #:step #:split
           #:cons #:count #:first #:last #:max #:min #:find #:string #:vector #:hash-table
           #:random)
  ;; --- Entry Points --- ;;
  (:export #:transduce)
  ;; --- Transducers -- ;;
  (:export #:pass #:map
           #:filter #:filter-map #:unique #:dedup
           #:drop #:drop-while #:take #:take-while
           #:uncons #:concatenate #:flatten
           #:segment #:window #:group-by
           #:intersperse #:enumerate #:step #:scan
           #:log
           #:once
           #:from-csv #:into-csv)
  ;; --- Higher Order Transducers --- ;;
  (:export #:branch #:inject #:split)
  ;; --- Reducers -- ;;
  (:export #:cons #:snoc #:vector #:string #:hash-table
           #:count #:average #:median
           #:anyp #:allp #:any #:all
           #:first #:last
           #:fold #:max #:min #:find
           #:for-each)
  ;; --- Sources --- ;;
  (:export #:ints #:cycle #:repeat #:random #:shuffle
           #:plist #:reversed)
  ;; --- Conditions --- ;;
  (:export #:empty-transduction
           #:imbalanced-plist)
  ;; --- Restarts --- ;;
  (:export #:next-item
           #:retry-item)
  ;; --- Utilities --- ;;
  (:export #:comp #:const
           #:reduced #:make-reduced #:reduced-p #:reduced-val)
  (:documentation "Ergonomic, efficient data processing."))

(in-package :transducers)

;; --- Types --- ;;

(defstruct reduced
  "A wrapper that signals that reduction has completed."
  val)

(defun reduced (item)
  "Wrap a value to signal that reduction has completed."
  (make-reduced :val item))
