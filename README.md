tagged-racket releases
===

This folder contains tarballs of past versions of Tagged Racket:

* tagged-racket-beta : preliminary version, for measuring naive inlining vs. naive chaperones
  - defender checks everything
  - `TAGGED-USE-CHAPERONE?` flag decides whether to chaperone or inline checks
    for calls to exported functions
  - does not protect exported objects
* tagged-racket-beta-1 : bugfix, to generate contracts for type aliases
* tagged-racket-v0.0 : tag->sc version
* tagged-racket-v0.1 : add caching, use flat-contract-predicate
* tagged-racket-v0.2 : remove flat-contract-predicate, bless more primops
* tagged-racket-v0.3 : bugfix, generate flat contracts for provided names
* tagged-racket-v0.4 : protect typed functions used as first-class values (first try)
* tagged-racket-v0.5 : ... remove keywords from FSM, bless some racket functions from protection
* tagged-racket-v0.6 : fix "blessing" bug, should make morsecode faster
* tagged-racket-v0.7 : new check strategy, doesn't bless typed, fails for case-> U functions
* tagged-racket-v0.8 : bless some typed, everything SHOULD be annotated --- oops there was a bug with extend-parameterization
* tagged-racket-v0.9 : typed functions are defensive
* tagged-racket-v0.10 : (defense totally works?) struct-accessors defended against, macros ignored, rest-arg parsing fixed
* tagged-racket-v0.11 : remove `flat-named-contract` `>=/c`, especially for numeric predicates; makes a huge difference
* tagged-racket-v0.12 : more performance tuning, remove all `flat-named-contract`, don't use `and/c` for procedure contracts
  - last thing I can think of is de-duplicating `or/c` contracts .... but not sure where's an efficient place to put the de-duplicate code 
* tagged-racket-v0.13 : major bugfix, check return types of `require/typed` functions
* tagged-racket-v0.14 : minor bugfix, `(f x) : Void` when `f` is a parameter
* tagged-racket-v0.15 : minor fix, support `natural?` and `case->` for jpeg benchmark

