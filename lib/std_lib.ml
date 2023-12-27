open Agaml_core.Utils
module OList = struct

  let ty_body x =
    tmu "list" @@
    tvariant [
      "nil" , tunit ;
      "cons" , trecord [
        "hd" , x ;
        "tl" , tvar "list" ;
      ] ;
    ]

  let ty = tfunc "T" @@ ty_body (tvar "T")

  let nil = funct "T" @@ func "x" tunit @@ case_fold "nil" unit @@ tcall (tvar "list") (tvar "T")
  let nil_int = callt (var "nil") tint

  let namespace = nstatements [
    stlet "list" ty ;
    slet "nil" nil ;
    slet "nil_int" nil_int ;
  ]
end
module OOption = struct

  let ty_body x = tvariant [
    "none" , tunit ;
    "some" , x ;
  ]

  let ty = tfunc "T" @@ ty_body (tvar "T")
  let none = funct "T" @@ case "none" unit @@ tcall (tvar "option") (tvar "T")
  let none_int = callt none tint
  let some = funct "T" @@ func "x" (tvar "T") @@
    case "some" (var "x") @@ tcall (tvar "option") (tvar "T")

  let namespace = nstatements [
    stlet "option" ty ;
    slet "none" none ;
    slet "none_int" none_int ;
    slet "some" some ;
  ]
end