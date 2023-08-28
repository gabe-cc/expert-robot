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
- rec types with fold/unfold
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
- partial eval
  - partial evaluator
  - literal -> expr
  - inline flag
- location
- CLI
- grammar
- patterns
