# Dynamic

This is an experimental hack for building a static analyzer for a
subset of JavaScript language.

## Current Status

+ Coarse lattice + Concrete modeling
+ k-CFA
+ Deterministic + Top-level driving

## Language
+ [x] Closure
+ [x] Dynamic accessing
+ [x] Imperative control
+ [x] Object

## TODOs
- [ ] Clean up "TODO"s, "FIXME"s and "XXX" (if unresolvable temporarily, mark them in issue)
- [ ] Test recursive invocation and k-CFA limitation
- [ ] Refactor the code, to support experimental pluggable spec modeling

## Usage
If you are using cabal: `cabal build`, then `./dist/build/dynamic/dynamic /PATH/TO/JS`;

If you are using stack: `stack build`, then `stack exec dynamic /PATH/TO/JS`. Currently, it
is recommended to use `stack ghci` and use `main' /PATH/TO/JS ShowLog` inside the REPL to
control logging (Sorry that I haven't implemented the logging command line option yet)

Some simple JavaScript program fragments are provided in `examples` folder.
