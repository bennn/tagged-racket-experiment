GO forth and compile the macro-versions.
Look out for exporting structs & classes & objects.
- [ ] PASS 1 just try to compile
- [ ] PASS 2 look at what's exported, any trouble for current configs?
- [ ] PASS 3 any trouble for ALL configs?

- - -

NOTE changed type in `lnm/main.rkt`, all keywords but `#:L` are optional.

- - -

- [X] acquire-worst
  - 1 : higher-order
- [X] determinance
- [X] dungeon-typed
- [X] dungeon-worst
  - 2 : exports classes (cell.rkt)
- [X] forth-bad
  - 2 : only exports functions to untyped
  - 3 : eventually exports classes
- [X] fsm
- [X] fsmoo
- [X] gregor-worst
  - 2 : provides structs
- [X] kcfa-worst
  - 2 ???
- [ ] lnm-worst
  - 1 : unbound id ???
- [X] mbta-worst
  - 2 : maybe unsafe? can methods get called with tag-unsafe arguments???
- [X] morsecode-worst
- [X] quadBG-worst
- [ ] quadMB-worst
  - 1 : unbound id ???!?!?!?!?!
- [ ] real-time
  - 1 : polydots !!!!!?!
- [X] snake-worst (probably okay)
  - 1 : unbound id
- [X] suffixtree (probably okay, very slow)
  - 1 : unbound id
- [X] synth
  - 1 : unbound id
- [X] take5-worst
  - 1 : higher-order
- [X] tetris-worst
  - 2 : provides structs
- [X] trie-vector
  - 1 : unbound id YES YES YES should be simpler to debug!!!!
- [X] zombie-worst
- [X] zordoz-worst
