(in-package :transducers)

(defmacro reduced-p (item)
  `(reduced? ,item))

(defmacro any (pred)
  "Deprecated: Use `any?'."
  (warn "`any' is deprecated; use `any?' instead.")
  `(anyp ,pred))

(defmacro anyp (pred)
  `(any? ,pred))

(defmacro all (pred)
  "Deprecated: Use `all?'."
  (warn "`all' is deprecated; use `all?' instead.")
  `(allp ,pred))

(defmacro allp (pred)
  `(all? ,pred))
