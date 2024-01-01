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
    - [X] equality vs reductions
      - [X] no reduction should be performed _within_ the equality relationship
      - [X] alpha-equivalent syntactic equality
    - [X] decide on the status of polymorphic expressions and parametric types
      - [X] find polymorphic expressions / parametric types where there are errors or not _depending on what you reduce them with_
        - none in current calculus
      - [X] semi-equality?
        - "for sure yes"
        - "for sure no"
        - "unsure"
        - converges on full equality on ground terms
        - good idea, but complex
        - transparent parameters / functions
          - non-full-equality when going through them should be an error
      - [X] partial parameters / functions
        - already dealt with above
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
    - [X] type level calculus??
      - type operators
        - tfield: access field from type records
        - targ: access arg from type functions
        - more operators for other things
      - pattern match on type
        - more general than just `tfield` / `targ` custom type operators
        - `match_type ... with TRecord lst -> ...`
        - but then, to actually extract `foo : number` from `{ foo : number ; ... }`, need to have other type functions like `List.assoc` at the type level
      - quotes
        - can reify a type into a value, that can then be manipulated
        - and then, a value of that type back into a type
        - ```ocaml
            type 'a get_foo = to_type (
              match (to_value 'a) with
              | TRecord lst -> (
                List.assoc "foo" lst
              )
              | _ -> failwith "get_foo: expected type trecord"
            )
          ```
        - just based???
        - true magic of `type : type`
        - `to_value` can only exist at typing-time
          - must get type erased to hell
          - check on static stuff
        - how does `to_value` deal with incomplete types?
          - `get_foo` good example of this
            - when evaluating `get_foo`, `to_value` only get a TVar for `'a`, as `get_foo` has not been applied to an arg at start
          - what is the strategy to deal with this?
            1. only apply `to_value` to fully reduced types, make it `partial` instead
              - valid
            2. apply `to_value` to the non-fully reduced type. fail, but discard the failure because one of the vars in the env was used and not bound, and return partial instead.
              - invalid, too complicated
            3. put burden on quote definer:
              - `to_value 'a` returns `Partial | Full`, so quote definer knows if it deals with fully reduced or not
              - itself returns `Partial | Full`, and `Partial` is diff from raising an exception
              - seems best. not incompatible with `1`.
                - 1 is just `Full x -> f x | Partial x -> Partial x` 
          - this is truly based.
      - yes. quotes.
    - merge all the calculi?
      - what are the calculi?
        - term calculus
        - type calculus
        - namespace calculus
      - how is this dealt with in general?
        - most langs (TS to OCaml lol): diff calculi for each level
        - dyn langs: 1 calculus for everything, because no type, and no care to namespaces
          - python: no actual namespaces
          - JS: just objects
        - coq: 
      - 3 full diff calculi
        - invalid. already di
    - move design decisions to written text
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

