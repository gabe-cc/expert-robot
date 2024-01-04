# Basic Setup

```sh
opam switch create . 5.1.0
eval $(opam env)
opam install dune utop ocaml-lsp-server ocamlformat
opam install ocolor ppx_deriving
dune runtest --watch
```

# TODO
- V1
  - grammar
  - run file
  - check file
  - compile file
  - compile folder
    - link context and files
  - main logic
- design more type stuff
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
  - [X] ...abstract types?
    - scopes
      - scope where open, else can do nothing
      - scope where abstract
      - `let rec abstract x = (* open *) and y = (* open *) ;; (* opaque *)` ?
      - module without signature, just match to all instances of abstract type
        - bad. sometimes, you want `val make : open_version -> opaque_version`
        - very bad. not clear _what_ should be abstract when implicit
      - module signatures
        - implement type matching, important for other things too
        - very verbose
    - implem
      - paths
        - abstract type is identified by its path, like `ThatAbstractModule.t`
        - ocaml way!
        - but what about results of functors / anon modules?
          - not in the type-calculus
            - can't do `(module M)` or `(val x)` in type expressions
          - no scope extrusion
            - if bound to a local binding, can't move out to a scope where the local binding is not accessible
        - extremely restrictive.
          - don't want to have syntactic restriction of "no module in the type calculus"
          - OTOH, what is `(F (X)).t` wrt scope extrusion?
      - opaque type
        - create new `unfold` constructor, for opaque types
        - opaque types not unfolded by default
        - private types??
          - like opaque types, but only specific constructor can open it
          - `unfold` tagged with full structural types
          - opaque type tagged with a name, that should be used on `unfold`, and acts as a key?
            - nominal type
            - doesn't work
          - or opaque type tagged with a randomly generated UUID -> non-determinism!!!
        - very hacky.
        - problem of raw opaque types:
          - no guarantee, anyone can still use the `unfold` constructor
          - solution: remove it from scope!
      - dependent pairs.
        - something something like existential GADTs in OCaml, but more powerful
        - need to support existential types in top level bindings & between compilation units
        - doesn't let you have a module calculus (`include`) without solving dependent _rows_
        - on reflection: same problem as paths based
          - only get access to the abstract type when deconstructing the dependent pair, which only happens on binders (match)
    - yes. go with modules + module signatures + paths.
      - tried and tested
      - already vpowerful
      - can do more later but ye.
  - [X] subset/refinement/predicate types?
    - more powerful static analysis within type system?
      - bleh.
      - type system convenient, but should have practical way to bootstrap from it
    - if type system already deals with constrains, natural extension
      - hindley milner can say `?t = (?? * int)`, so you could write something like TS, `forall T extends (any * int) , ...`
      - or MLF's constraints
    - enforce predicate on type
      - use module abstraction instead
        - `make : true_type -> t` and `get : t -> true_type`
        - but bad developer experience
          - ok to overload _constructors_, because new constraint is enforced on construction
          - but not ok to overload _destructors_. new constraint is _granted_ on destruction
          - any obvious solution?
            - modular implicit / type classes on `get`
              - but what is `get`??
              - when are recursive values not bound to a variable reduced??
                - they are always bound to a variable, that's the semantics of `(rec x -> ...)`: bound the full expr to `x`
                - so there's a meaningful `get` operation: access the variable from the context
              - no equivalent for `abstract` types: not necessarily bound to a variable, can be created anonymously and used through a structure. like `{ foo = abstract_make x ; bar = ... }.foo`. no `var` is bound to `abstract_make`, and no natural interface for both a variable and a record access
        - does not compose
          - `{ x in T | predicate1(x) } & { x in T | predicate2(x) }` -> `{x in T | predicate1(x) && predicate2(x) }`
          - composition so bad here. you want a logic.
      - subtyping
        - `{ x in T | predicate(x) } <: T`
        - aaaaaaaaa, why would you hurt yourself
    - tl;dr: no.
      1. for basic things, abstraction + some thinking about DX for use-site
      2. for complex things, L o G i C
- convenience
  - on modules with abstract signature, expose `Raw` version automatically
  - match var names to types
  - create constructors for variants and recursive variants automatically
  - List.map : (type a b) . (a list) -> (a -> b) -> b list
    - on definition, OCaml's thing: `(lst : 'a list) (f : 'a -> 'b)`
      - no need to write `(type a b) ...` when it is top level
      - can even write `(lst : {'a} list) (f : 'a -> {'b})` to make `'a` and `'b` implicit
    - on use, List.map lst (fun x -> ...)
      - extract `{a}` from `lst`, and inject it in the following
      - extract `{b}` from `(fun x -> ...)` and inject it in the following
    - if not all args are passed: fail!
      - means that chained applications now must be first class in ast
  - optional args
  - functions defined by holes
    - examples
      - inc function `List.map lst (? + 1)`
      - swap function `(f ?2 ?1)`
    - doesn't work well with no-type-inferece
    - functions with minimal characters
  - set theoretic types
    - literals, enums, and the like
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
