module O = Ocolor_format
module AT = Agaml.Types
module AU = Agaml.Utils

let fail = ref true

let test name f =
  try (
    f () ;
    O.printf "@{<green>Pass@} [@{<it>%s@}]\n" name
  ) with e -> (
    fail := false ;
    O.eprintf "@{<red;bold>Failure@} [@{<it>%s@}]\nException:\n%s\n\n"
      name (Printexc.to_string e)
  )

let test_eval () =
  let open AU in
  let test name x y =
    test name @@ fun () ->
    assert (toplevel_eval x = y) in
  test "let in" (
    let_in "x" !+%42 @@
    !%"x"
  ) @@ vint 42 ;
  test "add" (!+%21 +% !+%40) @@ vint 61 ;
  test "app fun" (
    let_in "inc" (func "x" tint @@ !%"x" +% !+%1) @@
    !%"inc" @% !+%42
  ) @@ vint 43 ;
  test "shadowing" (
    let_in "x" !+%42 @@
    let_in "x" (!%"x" +% !+%1) @@
    !%"x"
  ) @@ vint 43 ;
  test "record" (
    let_in "foo" !+%1 @@
    let_in "bar" !+%2 @@
    record [
      "foo" , !%"foo" ;
      "bar" , !%"bar" ;
    ]  
  ) @@ vrecord ["foo" , vint 1 ; "bar" , vint 2] ;
  test "field" (
    let_in "my_record" (record [
      "a" , !+%144 ;
      "b" , !+%123 ;
    ]) @@
    !%"my_record" /% "b"
  ) @@ vint 123 ;
  test "case" (
    let_in "content" !+%42 @@
    c"lol" !%"content" (tvariant ["lol" , tint])
  ) @@ vc"lol" (vint 42) ;
  test "match-1" (
    let_in "x" (c"foo" !+%7 (tvariant ["foo" , tint])) @@
    match_ !%"x" [
      "bar" , ("y" , !%"y" +% !+%1) ;
      "foo" , ("x" , !%"x" +% !%"x" +% !%"x") ;
    ]
  ) @@ vint 21 ;
  test "match-1" (
    let_in "x" (c"bar" !+%7 (tvariant ["bar" , tint])) @@
    match_ !%"x" [
      "bar" , ("y" , !%"y" +% !+%1) ;
      "foo" , ("x" , !%"x" +% !%"x" +% !%"x") ;
    ]
  ) @@ vint 8 ;
  ()



let () =
  test_eval ()