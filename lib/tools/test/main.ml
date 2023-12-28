module O = Ocolor_format
module T = Agaml_tools
open T.Test

let test_list_take_n () =
  let pp_ints = O.pp_list_generic ~left:"[" ~right:"]" ~sep:" ; " Format.pp_print_int in
  let test name (input_n , input_lst) expected =
    test_basic (
      O.asprintf "list_take_n ; %s" name
    ) @@ fun () ->
    let exception Diff_values in
    let got = T.list_take_n input_n input_lst in
    if got <> expected then (
      print_not_expected pp_ints ~got ~expected ;
      raise Diff_values
    )
  in
  test "empty" (0 , []) [] ;
  test "123" (2 , [1;2;3]) [1;2] ;
  ()

let test_dassoc () =
  (* use the raw version to access the internals *)
  let open T.DAssocRaw in
  let test name got expected =
    test_basic (
      O.asprintf "dassoc ; %s" name
    ) @@ fun () ->
    let exception Diff_values in
    if got <> expected then (
      raise Diff_values
    )
  in
  test "empty" empty (Forward , []) ;
  test "simple forward assoc none"
    (assoc_opt 42 (Forward , [23 , 42])) None ;
  test "simple forward assoc some"
    (assoc_opt 23 (Forward , [23 , 42 ; 23 , 12])) (Some 42) ;
  test "simple backward assoc none"
    (assoc_opt 23 (Backward , [23 , 42])) None ;
  test "simple backward assoc some"
    (assoc_opt 23 (Backward , [42 , 23 ; 12 , 23])) (Some 42) ;
  test "swap"
    (swap (Forward , [23 , 42 ; 23 , 12])) (Backward , [23 , 42 ; 23 , 12]) ;
  ()

let () =
  test_list_take_n () ;
  test_dassoc () ;
  ()
