# Transducers

### Unreleased

#### Added

- An `uncons` transducer for splitting up a stream of cons cells.
- A `from-csv` transducer to interpret a string stream as CSV data, splitting
  the values into a hash table.
- An `into-csv` transducer that, given some `headers`, reverses the stream of
  hash tables back into strings of CSV.
- A `snoc` reducer for reducing into a list without performing a final reverse.
- A `hash-table` reducer for reducing into a Hash Table. Requires the
  transduction items to be cons cells, with `car` being the key and `cdr` being
  the value.
- A `plist` source for transducing over key-value pairs in a Property List.
- New conditions:
  - `empty-argument`
  - `empty-transduction` (see below)
  - `imbalanced-plist`
  - `non-positive-integer`

#### Changed

- The library is now distributed under the terms of the LGPL.
- Transducing over a `hash-table` now yields the key-value pair as a cons cell,
  instead of just the value. The former behaviour is considered a bug.
- The `seed` argument of the `fold` reducer is now `&optional`. When missing,
  the first value that makes it through the transduction will be used as the
  seed. If the transduction turns out to be empty, then the condition
  `empty-transduction` will be raised.
- The `cons` reducer now uses `nreverse` internally. You better not have been
  saving the intermediate results anywhere!
- `segment`, `window`, and `step` now offer restarts when a bad initial value is
  passed.
- **BREAKING:** `average`, `first`, and `last` no longer accept a fallback
  value. Instead, a condition is raised when the transduction was empty. This
  means that calls that used to look like:

```common-lisp
(t:transduce #'t:pass (t:average 'whatever) '(1 2 3 4 5))
```

should now look like:

```common-lisp
(t:transduce #'t:pass #'t:average '(1 2 3 4 5))
```

Likewise for `first` and `last`.

#### Fixed

- List transduction fixed on Clasp. Clasp doesn't (seem to) support tail-call
  optimization, but it does support `labels`-based TCO. This is also critical
  for the `jzon` support.

#### Deprecated

- `max` and `min` have been deprecated in favour of calling `fold`. The seed
  value for `fold` is optional now, so:

```common-lisp
(t:transduce #'t:pass (t:fold #'max)    '(1 2 3)) ;; => 3
(t:transduce #'t:pass (t:fold #'max 10) '(1 2 3)) ;; => 10
(t:transduce #'t:pass (t:fold #'max 10) '())      ;; => 10
(t:transduce #'t:pass (t:fold #'max)    '())      ;; => Condition!
```

### 0.1.1 (2023-08-11)

#### Added

- A `once` transducer to inject a single value into any point in the
  transduction chain.
- A `for-each` reducer to ignore all results and just consume the stream for its
  side effects.

#### Deprecated

- `all` and `any` should now be called as `allp` and `anyp`. The old names have
  been marked deprecated and you will be warned at compile time.

### 0.1.0 (2023-03-04)

Initial version.
