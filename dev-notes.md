# basic setup
opam switch create . 5.0.0
opam install dune ocolor ocaml-lsp-server ocamlformat
eval $(opam env)
dune runtest --watch

# TODO
X check
- test
- rec
- error message
- polymorphism
- partial eval
  - partial evaluator
  - literal -> expr
  - inline flag