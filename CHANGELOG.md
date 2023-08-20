# Transducers

### Unreleased

#### Added

- A `csv` transducer to interpret a string stream as CSV data, splitting the values into a list.
- A `snoc` reducer for reducing into a list without performing a final reverse.
- A `hash-table` reducer for reducing into a Hash Table. Requires the
  transduction items to be cons cells, with `car` being the key and `cdr` being
  the value.
- The `empty-transduction` condition (see below).

#### Changed

- Transducing over a `hash-table` now yields the key-value pair as a cons cell,
  instead of just the value. The former behaviour is considered a bug.
- The `seed` argument of the `fold` reducer is now `&optional`. When missing,
  the first value that makes it through the transduction will be used as the
  seed. If the transduction turns out to be empty, then the condition
  `empty-transduction` will be raised.

#### Fixed

- List transduction fixed on Clasp. Clasp doesn't (seem to) support tail-call
  optimization, but it does support `labels`-based TCO. This is also critical
  for the `jzon` support.

### 0.1.1 (2023-08-11)

#### Added

- A `once` transducer to inject a single value into any point in the
  transduction chain.
- A `for-each` reducer to ignore all results and just consume the stream for its
  side effects.

#### Changed

- `all` and `any` should now be called as `allp` and `anyp`. The old names have
  been marked deprecated and you will be warned at compile time.

### 0.1.0 (2023-03-04)

Initial version.
