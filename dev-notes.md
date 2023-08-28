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
- polymorphism
  - collapse texpr and tvalues
  - simple
  - higher order
  - CoC
- multi file
- misc
  - tfield (access field from type records)
  - maps for records and variants
  - add subtyping between variants
    - add default inference for constructor
  - add subtyping between records
  - minimal size closures
  - test shadowing (in match among other things)
  - let inline
  - partial evaluate function args
  - go through TODOs
- error message
- closure for mu types (mu within mu)
- mutually recursive mu types
- CLI
- grammar
- location
- patterns
