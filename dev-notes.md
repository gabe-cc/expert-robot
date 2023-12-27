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
    - decide on whether all types should be fully evaluated up to a point
      - likely yes
    - should types be evaluated under their lambdas??
    - decide on when static evaluation happens
      - likely during typechecking
    - decide on the status of polymorphic expressions and parametric types
      - find polymorphic expressions / parametric types where there are errors or not _depending on what you reduce them with_
  - decide on pattern match on types vs type operators
    - type operators
      - tfield: access field from type records
      - targ: access arg from type functions
    - pattern match on type
      - reify type type and interpret it
  - maps for records and variants instead of lists
  - standardize test helpers
  - decide on subtyping for variants and records
    - likely no
  - decide on inference for variant constructor
    - likely generate
  - decide on row calculus for variants and records
    - joints of rows are exclusive or inclusive?
    - row polymorphism is lazy or symbolic?
  - figure out nominal vs re-occurring types vs module abstraction
    - think about functions taking an abstract module as parameter
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

