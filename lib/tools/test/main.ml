module O = Ocolor_format
module T = Agaml_tools

let test_basic name f =
  (try (
    f () ;
    O.printf "@{<green>Pass@} [@{<it>%s@}]\n" name
  ) with e -> (
    O.eprintf "@{<red;bold>Failure@} [@{<it>%s@}]\nException:\n@{<orange>%s@}\n\n"
      name (Printexc.to_string e)
  ))

let print_diff_values pp output expected =
    O.eprintf "@[<v>Different contexts.@;Got [@{<red>%a@}]@;vs@;Expected [@{<green>%a@}]@;@]"
    pp output pp expected


let test_list_take_n () =
  let pp_ints = O.pp_list_generic ~left:"[" ~right:"]" ~sep:" ; " Format.pp_print_int in
  let test name (input_n , input_lst) expected =
    test_basic (
      O.asprintf "list_take_n ; %s" name
    ) @@ fun () ->
    let exception Diff_values in
    let output = T.list_take_n input_n input_lst in
    if output <> expected then (
      print_diff_values pp_ints output expected ;
      raise Diff_values
    )
  in
  test "empty" (0 , []) [] ;
  test "123" (2 , [1;2;3]) [1;2] ;
  ()

let () =
  test_list_take_n () ;
  ()
