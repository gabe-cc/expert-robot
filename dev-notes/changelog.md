# V0
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
  - [X] cleanup partial/full & strong/weak evaluation for types
    - [X] add more examples
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
    - [X] move design decisions to written text
    - [X] `teval` should return `Full` or `Partial`

# V1