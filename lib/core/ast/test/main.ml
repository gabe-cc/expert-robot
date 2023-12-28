open Agaml_tools.Test
open Agaml_ast
open Utils
open Equality

let test_subtype () =
  let test name a b =
    test_basic (Format.asprintf "subtype ; %s" name) @@ fun () ->
    if not @@ subtype a b then (
      print_diff_values Types.pp_texpr ~a ~b
    )
  in
  test "tint tint" tint tint ;
  test "dummy poly"
    (tfunc "T" @@ tvar "T")
    (tfunc "T" @@ tvar "T") ;
  test "dummy poly renamed"
    (tfunc "T" @@ tvar "T")
    (tfunc "U" @@ tvar "U") ;
  test "id type alpha-renamed"
    (tfunc "T" @@ tarrow (tvar "T") (tvar "T"))
    (tfunc "U" @@ tarrow (tvar "U") (tvar "U")) ;
  ()

let () =
  test_subtype ()