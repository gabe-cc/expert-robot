# Basic Setup

```sh
opam switch create . 5.1.0
eval $(opam env)
opam install dune utop ocaml-lsp-server ocamlformat
opam install ocolor ppx_deriving
dune runtest --watch
```

# Design Choices

## Recursion

Recursion concerns itself with recursive _types_ and recursive _values_.
Recursive types are types that refer to themselves. For instance: `tree = Leaf of int | Node (list tree)`.
Recursive values are values that refer to themselves. For instance, the fibonacci function.

There are recursive values with non-recursive types. eg: the fibonacci function.
There are non-recursive values with recursive types. eg: `Leaf 42` of the above-defined type `tree`.

### Recursive Types

The code implements recursive types through iso-recursion. This means that:
- Constructing / Building an expression `expr` of a recursive type `t` requires wrapping it with a `fold` (`fold expr`).
- Destructing / Using an expression `expr` of recursive type `t` requires unwrapping it through `unfold` (`unfold expr`).

This is most common with inductive types (recursive variants). In that case, you will do something like `match unfold (fold (Foo 42)) with`

### Recursive Values

The code implements recursive values through a `rec` constructor.

Consider those two examples:
```ocaml
(* Example 1 *)
let factorial = rec self -> fun x ->
if x = 0 then 1
else x * (self (x - 1))

(* Example 2 *)
let lazy_infinite_list = rec self -> 1 :: (fun () -> self)
```

Whenever `rec self -> body` is encountered during evaluation, `body` is evaluated with `self` binding to `rec self -> body` in the context.

Then, whenever `rec self -> body` is encountered when fetching a variable from the environment, it is evaluated one more time.


# TODO
- [X] check
- [X] pp
- [X] test
  - [X] eval
  - [X] synthesize
  - [X] anti-tests
- [X] remove forced annotation on constructor
- [X] rec types with fold/unfold
  - [X] eval
  - [X] type checking
  - [X] tests
    - [X] types
    - [X] eval
- [X] rec values with rec keyword
  - [X] implem
  - [X] tests
    - [X] eval
    - [X] types
- [X] static eval
  - [X] static eval (full or partial) construct
  - [X] reconstruct term when synthesizing
  - [X] tests
- [X] polymorphism
  - [X] collapse texpr and tvalues
  - [X] deep polymorphism
  - [X] remove useless cases in check
  - [X] let type in
  - [X] TCall
  - [X] parametric types
- [X] multi file preparation
  - [X] check statements
  - [X] namespace calculus basic
    - [X] variable
    - [X] struct
    - [X] value
    - [X] type
    - [X] Test
  - [X] build std lib modules
    - [X] list
    - [X] option
- misc
  - [X] add example of partial evaluation on record access (should work and return the correct field from the partial record)
  - [X] add example of typing closure or remove type checking of closure
  - [X] interweave var/tvar/nvar in ctx+tctx & share vars (split from OCaml, closer to Coq!)
    - [X] Create new ctx and tctx types
    - [X] Create context.ml for ctx helpers
    - [X] Update eval.ml to use those helpers
    - [X] Create tcontext.ml for tctx helpers
    - [X] Update bidir.ml to use those helpers
    - [X] Separate forward contexts from backward contexts
      - [X] And fix some bugs thanks to that lol
  - cleanup partial/full & strong/weak evaluation for types
    - add more examples
      - [X] id with diff name
        - [X] test equality specifically
        - [X] extend equality for alpha-equivalence
          - [X] don't perform alpha renamings (n^2 time and space)
          - [X] maintain mapping
          - [X] debug existing code
      - [X] id with higher order as param
      - [X] id applied to itself
      - [X] test synthesize should return fully evaluated types
      - (finished when cleanup is finished)
    - [X] decide on whether all types should be fully evaluated up to a point
      - [X] ~~yes.~~ nope.
        - within polymorphic functions, nothing that depends on the type parameter is fully evaluated
        - if not, how can you pattern match on types?
          - you go _as far as possible_
          - if you depend on a parametric/polymorphic type variable, you don't unfold the match until you get access to the parameter
      - [X] `teval` should return `Full` or `Partial`
      - [X] how to discriminate between strong and weak reductions? should there be any discrimination?
        - strong: reduce under parametric/polymorphic
        - partial: reduce whenever an arg is passed, doesn't wait for all the args
        - right now, all type reductions are strong & partial
        - `fun static` should be for strong & partial _value_ reductions
        - and possibly a keyword for full type reductions, or weak type reductions??
    - [X] in the future, what if arbitrary functions at type level (type-level calculus or CoC)?
      - [X] imagine infinite loop, how to debug?
        - [X] first, how do you debug infinite loops at runtime?
          - printfs.
            - so need printfs at static-time!
          - timers?
            - eee. but agreed, need timers at static-time.
          - state.
            - agreed that need state at static-time.
          - debugger.
            - yup, need type level debugger, which is easy in our case.
        - [X] timeout on type level evaluations?
          - complicated. how to make it principled?
          - consider:
            - i build library just below the time out limit
            - anyone that uses it goes over the limit
          - looks bad. no for now.
        - [X] type-level debugger?
          - yes, ideally, debugger for type interpreter, interpreter *and* compiled code
          - most realistically, just for the type interpreter and interpreter at first
        - [X] sub-class that is linear time by construction and warning outside of it?
          - how do you enforce it on non-type things?
            - define complexity checker
            - b a  s   ed
            - but more realistically: no, not a good solution
      - [X] how do you interweave static and type?
        - two typical approaches
          - 1. separate ASTs, and instead complex literal&unliteral / quote&unquote
            - Staged Metaprogramming tradition.
          - 2. mix value AST and type AST: merge func, funct and tfunc
            - CoC tradition.
        - I feel closer to 1.
          - 2 with Type:Type can be strange, who knows
          - 2 almost certainly need a kind checker, but that might be trivial with Type:Type
          - 1 needs fewest changes to current codebase lol
          - 1 is closest to actual metaprogramming, which is what we are actually doing: metaprogramming through partial evaluation and type-level evaluation
      - [X] kind checking or not?
        - no. kind checking is hell.
          - or at least, i haven't dug enough to make it non-hell.
        - means that type level calculus is somehow dynamic.
          - yes!
      - [X] type erasure?
        - yes!
          - compilation is important
          - simple type-less `eval` is important
            actual semantics to the language
        - how do you do type erasure for things that depend on _internally_ or through some function application on types?
          - make sure that the applications are complete, and that all variables closed over are complete
        - doesn't type erasure imply kind checking?
          - ie: notice which sub-terms depend on types or not
          - nope, you can use an over-approximation.
      - [X] over-approximator for type erasure
        - tag all functions as static / dynamic
        - all parameters until type parameters **must** be static
        - all functions with **static** parameters must be fully evaluated...
          - in `main`
          - but not enough. need a notion of static vs dynamic exports:
            - want to be able to exports types and some static non-fully evaluated functions 
            - want to be able to exports non-main type-erased values
          - `static export` vs `dyn export`
      - [X] in the future, boxed values
        - OCaml can compile polymorphic functions
        - this is done through _boxed_ values
        - OCaml ensures that function bodies _can not_ depend on non-instantiated types, and always interact through non-instantiated values through boxed OCaml-provided functions
          - equality provided by OCaml
          - reference/dereference provided by OCaml
        - no solution for now to replicate this, but there _should_ be an approximator
          - look at objects, basically. this is what they do.
          - or rather, closer to Rust traits, that have specific layout requirements
    - [X] think: how do you interweave type checking and other things?
      - static analyses
        - after type checking a unit
        - need to store results of type checking unit, and then post processing them
        - need to store results of static analyses, in separate locations, that still map back to AST
      - type directed transformations
        - ad-hoc at first, like deep pattern matching
        - then possibly an engine?
    - [X] should types be evaluated under their lambdas??
      - yes. at least some.
      - else, how to compare `fun a -> list a` with itself?
      - but not _full_ normalization, because of recursive types
        - this is already solved with fold/unfold, no new complexity here
    - [X] decide on when static evaluation happens
      - during typechecking.
      - smells bad.
        - this is the correct solution, but...
        - type checking sounds like a bad name, doesn't capture the concept.
        - type checking sounds like mere pass of static analysis, but it transforms term.
      - what does type checking do?
        - synthesize types
        - check types
        - reduce types
        - apply static things
        - check that static things are all applied
      - why is type and static the same pass?
        - static depends on types, so must be at same time or after
          - ad-hoc polymorphism
        - types depends on static, so must be at same time or after
          - dependent types
      - still smells some:
        - writing files statically should not happen during typechecking.
        - this pass doesn't capture _all_ metaprogramming 
    - decide on the status of polymorphic expressions and parametric types
      - find polymorphic expressions / parametric types where there are errors or not _depending on what you reduce them with_
      - semi-equality
        - "for sure yes"
        - "for sure no"
        - "unsure"
        - no reduction should be performed _within_ the equality relationship
        - converges on full equality on ground terms
        - alpha-equivalent syntactic equality
      - outside of equality, all reductions should be performed eagerly, through type-level partial evaluations
      - partial parameters / functions
        - all types are eagerly partially evaluated
        - params can be flagged as "partially evaluatable" (will be partially evaluated whenever instantiated)
      - transparent parameters / functions
        - non-full-equality when going through them should be an error
      - examples for all of them
    - move design decisions to written text
  - decide on pattern match on types vs type operators
    - type operators
      - tfield: access field from type records
      - targ: access arg from type functions
    - pattern match on type
      - reify type type and interpret it
  - tliteral for expressions
    - quotes???
  - maps for records and variants instead of lists
  - standardize test helpers
  - decide on subtyping for variants and records
    - likely no
  - decide on inference for variant constructor
    - likely generate on type definition
  - decide on row calculus for variants and records
    - joints of rows are exclusive or inclusive?
    - row polymorphism is lazy or symbolic?
  - figure out nominal vs re-occurring types vs module abstraction
    - think about functions taking an abstract module as parameter
  - params can be flagged as "must be partially evaluated" (can't be passed around as dynamic closures)
  - test shadowing (in match among other things)
  - let static
    - eval content of the let
    - change tctx for eval case as a result
  - namespace static
    - so that you can call a method from a namespace in `let static` or `eval`
  - go through TODOs
  - test mu within mu (might need closure for mu types)
  - investigate whether `tctx.TCTerm` should contain an `expr member`
    - ie: should the type checker have a view of the values of the vars or not?
    - looks like it should, and only my lazyness made it so that it did not
- CLI
  - grammar
  - link context and files
  - main logic
- advanced
  - higher order examples
  - CoC
  - effects with outside world
  - top level effects well
  - extraction
- location
- error message
- CLI
- advanced
  - compile to python
  - FFI
  - mutually recursive mu types
  - patterns
  - namespace calculus advanced
    - let namespace in
    - signature calculus
      - named signatures
      - all namespace calculus
    - functors
    - include
    - open
    - visibility??
- misc
  - minimal size closures
  - fun inline
    - partial eval??
    - suspend type checking??
    - completely lazy type checking?? (only type check when you get the params)
    - try type checking, and if not working, make lazy??
  - partial evaluate function args

