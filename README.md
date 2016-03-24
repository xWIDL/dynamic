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
2. Think about path sensitivity: Can we model this by `join` the input element
   with some element in the same lattice? Or on the other hands, if our current
   lattice is *the lattice*, then can we model the branching easier? And if we
   take the possible *coercion* mechanism into consideration, can we make it simpler
   and more general?
3. About coercion: Maybe we can model it by mapping over lattice, which certainly can't
   be one-to-one. *Take a look at Galois connection and order isomorphism.*
