tagged-racket
===

This is an experimental fork of [Typed Racket](https://github.com/racket/typed-racket).

The point of the experiment was to weaken Typed Racket's soundness guarantee.
Instead of enforcing types, this version enforces type constructors --- that's
all.

The code here is not maintained.
It exists in this repository to keep track of the history.


Install
---

The following instructions might not work:

1. Clone this repository, `git clone https://github.com/bennn/tagged-racket`
2. Install every directory --- except the test directory --- as a package:
  - `raco pkg install ./source-syntax`
  - `raco pkg install ./tagged-racket`
  - `raco pkg install ./tagged-racket-lib`
  - `raco pkg install ./tagged-racket-more`

If the instructions worked, this program will compile and run:

```
#lang tagged/racket

(car '(A B C))
```


Usage
---

The `#lang tagged/racket` and `#lang tagged/racket/base` languages type-check
 the contents of a module --- just like Typed Racket --- and expand to Racket
 code that enforces the type constructors of each static type at runtime.


Limitations
---

A `tagged/racket` module cannot share type definitions with a `typed/racket`
 module.

