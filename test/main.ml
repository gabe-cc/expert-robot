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
    O.eprintf "@{<red;bold>Failure@} [@{<it>%s@}]\nException:\n@{<orange>%s@}\n\n"
      name (Printexc.to_string e)
  )

let test_eval () =
  let open AU in
  let test name x y =
    test (O.asprintf "@{<it>eval@} ; %s" name) @@ fun () ->
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

let test_synthesize () =
  let open AU in
  let test name x y =
    let exception Fail_synthesis in
    test (O.asprintf "@{<it>synthesize@} ; %s" name) @@ fun () ->
    let synth = toplevel_synthesize x in
    if (synth <> y) then (
      O.eprintf "@[<v>Different types.@;[@{<green>%a@}]@;vs@;[@{<red>%a@}]@;@]"
      AT.pp_texpr synth AT.pp_texpr y ;
      raise Fail_synthesis
    )
  in
  test "int" !+%42 tint ;
  test "add" (!+%23 +% !+%11) tint ;
  test "string" !^%"lel" tstring ;
  test "annot" (annot !+%42 tint) tint ;
  test "literal" (annot !+%42 (tlint 42)) (tlint 42) ;
  test "let in" (
    let_in "x" (annot !^%"lol" (tlstring "lol")) @@
    !%"x"
  ) (tlstring "lol") ;
  test "function" (
    func "x" tint @@ !%"x" +% !%"x"
  ) (tarrow tint tint) ;
  test "app" (
    let_in "f" (
      func "x" tstring @@ !+%42
    ) @@
    !%"f" @% !^%"wee"
  ) tint ;
  test "record" (
    record [
      "foo" , !+%42 ;
      "bar" , !^%"lol" ;
    ]
  ) (trecord [
    "foo" , tint ;
    "bar" , tstring ;
  ]);
  test "field" (
    let_in "x" (record [
      "foo" , !+%42 ;
      "bar" , !^%"lol" ;
    ]) @@
    !%"x" /% "foo"
  ) tint ;
  (
    let tvar = tvariant [
      "foo" , tint ;
      "bar" , tstring ;
    ] in
    test "case-1" (
      c"foo" !+%42 tvar
    ) tvar ;
    test "case-2" (
      c"bar" !^%"answer" tvar
    ) tvar ;
    test "match" (
      let_in "x" (c"foo" !+%42 tvar) @@
      match_ !%"x" [
        "foo" , ("y" , !%"y") ;
        "bar" , ("y" , !+%23) ;
      ]
    ) tint ;
  ) ;
  ()



let () =
  test_eval () ;
  test_synthesize () ;
  ()