# Transducers

### Unreleased

#### Added

- A `hash-table` reducer for reducing into a Hash Table. Requires the
  transduction items to be cons cells, with `car` being the key and `cdr` being
  the value.

#### Changed

- Transducing over a `hash-table` now yields the key-value pair as a cons cell,
  instead of just the value. The former behaviour is considered a bug.

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
