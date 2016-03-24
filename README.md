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
- [ ] Path sensitivity
- [ ] Introduce exception flows

## Problems
1. TAJS used a *very large* lattice and it seems reasonable to me now, since we
   should define operation between *any types*, such as `"s" + 1`. Basically,
   it is a mapping from operator and two input types to output type. The
   intra-type definitions should be able to be merged into the more general
   framework, facilitating the maintainability. Also, it seems like that
   some *coercion* mechanism is used.
