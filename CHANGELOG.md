# Transducers

### Unreleased

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
