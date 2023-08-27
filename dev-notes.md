# basic setup
opam switch create . 5.0.0
opam install dune ocolor ocaml-lsp-server ocamlformat ppx_deriving
eval $(opam env)
dune runtest --watch

# TODO
X check
X pp
- test
  X eval
  X synthesize
  - anti-tests
- rec
- error message
- misc
  - maps for records and variants
  - minimal size closures
  - test shadowing (in match among other things)
- polymorphism
- partial eval
  - partial evaluator
  - literal -> expr
  - inline flag
- location