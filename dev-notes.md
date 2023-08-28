# basic setup
opam switch create . 5.0.0
opam install dune ocolor ocaml-lsp-server ocamlformat ppx_deriving
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
- rec values with rec keyword
- error message
- misc
  - maps for records and variants
  - add subtyping between variants
    - add default inference for constructor
  - add subtyping between records
  - minimal size closures
  - test shadowing (in match among other things)
- polymorphism
  - simple
  - higher order
  - CoC
- multi file
- partial eval
  - partial evaluator
  - literal -> expr
  - inline flag
- closure for mu types (mu within mu)
- mutually recursive mu types
- CLI
- grammar
- location
- patterns
