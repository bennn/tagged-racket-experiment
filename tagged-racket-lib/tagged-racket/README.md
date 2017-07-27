tagged-diverge
===

TODO
- [ ] provides
  - [X] via macros
  - [X] via chaperones
  - [ ] compare
- [ ] how to map? other destructors

- [ ] LNM tagged version (plot module is trouble)
- [ ] determinance GTP

- [ ] make sure define-predicate makes a deep contract, use quadMB to test

- [ ] make contracts more efficient
  - [ ] shorter runtime code for vector/c etc.
  - [ ] make sure contract-expr is never an expression
  - [ ] lift contract defs to submodule, use a cache

- [ ] use `contract-first-order`
  - is this always what I want? (only checks class/obj names)
- [ ] are object,class,unit first-order checks "correct"?
  - revert all changes, just apply first-order at last second
- [ ] why use static-contracts at all?
  - [ ] optimizations (can't do this with contracts?)

- [ ] NEED to re-run Typed Racket, because these programs are different from GTP
      more require/typed


Functional vs. OO
---

2017-09-15 : don't worry about OO for now.

| Benchmark    | O |
|--------------+---|
| acquire      | X |
| determinance |   |
| dungeon      | X |
| forth        | X |
| fsm          |   |
| fsmoo        | X |
| gregor       |   |
| kcfa         |   |
| lnm          |   |
| mbta         | X |
| morsecode    |   |
| quadBG       | X |
| quadMB       | X |
| snake        |   |
| suffixtree   |   |
| synth        |   |
| take5        | X |
| tetris       |   |
| trie-vector  |   |
| zombie       |   |
| zordoz       |   |


Naming
---

use a keyword? `#lang typed/racket #:preserve (or/c 'types 'tags 'nothing)`

`(provide (type-out ....) (tag-out ....))`
 its weird because needs to know full type, but not trust anything but tag

