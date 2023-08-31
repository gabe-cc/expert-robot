# basic setup
opam switch create . 5.0.0
opam install dune utop ocaml-lsp-server ocamlformat
opam install ocolor ppx_deriving
eval $(opam env)
dune runtest --watch

# TODO
X check
X pp
X test
  X eval
  X synthesize
  X anti-tests
X remove forced annotation on constructor
X rec types with fold/unfold
  X eval
  X type checking
  X tests
    X types
    X eval
X rec values with rec keyword
  X implem
  X tests
    X eval
    X types
X static eval
  X static eval (full or partial) construct
  X reconstruct term when synthesizing
  X tests
X polymorphism
  X collapse texpr and tvalues
  X deep polymorphism
  X remove useless cases in check
  X let type in
  X TCall
  X parametric types
- multi file
  X check statements
  - namespace calculus
  - link context and files
  - build std lib
- misc
  - add example of partial evaluation on record access (should work and return the correctfield from the partial record)
  - add example of typing closure or remove type checking of closure
  - interweave var/tvar/nvar in ctx+tctx & share vars (split from OCaml, closer to Coq!)
  - cleanup partial/full | strong/weak evaluation for types
  - tfield (access field from type records)
  - maps for records and variants instead of lists
  - add subtyping between variants
    - add default inference for constructor
  - add subtyping between records
  - figure out nominal vs re-occurring types vs module abstraction
    - think about functions taking an abstract module as parameter
  - test shadowing (in match among other things)
  - let static
    - change tctx for eval case as a result
  - namespace static
    - so that you can call a method from a namespace in `let static` or `eval`
  - go through TODOs
  - test mu within mu (might need closure for mu types)
- advanced
  - higher order examples
  - CoC
  - extraction
- grammar
- location
- error message
- CLI
- advanced
  - compile to python
  - FFI
  - mutually recursive mu types
  - patterns
- misc
  - minimal size closures
  - fun inline
    - partial eval??
    - suspend type checking??
    - completely lazy type checking?? (only type check when you get the params)
    - try type checking, and if not working, make lazy??
  - partial evaluate function args
