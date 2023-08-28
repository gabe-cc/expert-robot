module O = Ocolor_format
module AT = Agaml.Types
module AU = Agaml.Utils

(* let fail = ref false *)

let test_basic name f =
  (* O.printf "Running %s\n%!" name ; *)
  (try (
    f () ;
    O.printf "@{<green>Pass@} [@{<it>%s@}]\n" name
  ) with e -> (
    (* fail := true ; *)
    O.eprintf "@{<red;bold>Failure@} [@{<it>%s@}]\nException:\n@{<orange>%s@}\n\n"
      name (Printexc.to_string e)
  ))
  (* O.printf "Ran %s\n\n%!" name *)

let test_eval () =
  let open AU in
  let test name x y =
    let exception Fail_eval in
    test_basic (O.asprintf "@{<it>eval@} ; %s" name) @@ fun () ->
    let x' =
      match toplevel_eval x with
      | Full x -> x
      | Partial x -> (
        O.eprintf "@[<v>Partial evalaution only.@;[@{<red>%a@}@]"
          AT.pp_expr x ;
        raise Fail_eval
      )
    in
    if (x' <> y) then (
      O.eprintf "@[<v>Different values.@;[@{<green>%a@}]@;vs@;[@{<red>%a@}]@;@]"
      AT.pp_expr x' AT.pp_expr y ;
      raise Fail_eval
    )
  in
  let test_fail name x =
    let exception No_fail in
    test_basic (O.asprintf "@{<it>eval@} ; %s @{<it>(fail)@}" name) @@ fun () ->
    try (
      ignore (toplevel_eval x) ;
      raise No_fail
    ) with _ -> ()
  in
  test "let in" (
    let_in "x" !+%42 @@
    !%"x"
  ) @@ !+%42 ;
  test_fail "let in, missing var" (
    let_in "x" !+%42 @@
    !%"y"  
  ) ;
  test "add" (!+%21 +% !+%40) @@ !+%61 ;
  test_fail "add, int and string" (!+%21 +% !^%"40") ;
  test "app fun" (
    let_in "inc" (func "x" !?%tint @@ !%"x" +% !+%1) @@
    !%"inc" @% !+%42
  ) @@ !+%43 ;
  test_fail "app non-func" (
    !+%42 @% !+%42
  ) ;
  test "shadowing" (
    let_in "x" !+%42 @@
    let_in "x" (!%"x" +% !+%1) @@
    !%"x"
  ) @@ !+%43 ;
  test "record" (
    let_in "foo" !+%1 @@
    let_in "bar" !+%2 @@
    record [
      "foo" , !%"foo" ;
      "bar" , !%"bar" ;
    ]  
  ) @@ record ["foo" , !+%1 ; "bar" , !+%2] ;
  test "field" (
    let_in "my_record" (record [
      "a" , !+%144 ;
      "b" , !+%123 ;
    ]) @@
    !%"my_record" /% "b"
  ) @@ !+%123 ;
  test_fail "missing field" (
    let_in "my_record" (record [
      "a" , !+%144 ;
      "b" , !+%123 ;
    ]) @@
    !%"my_record" /% "c"
  ) ;
  test "case" (
    let_in "content" !+%42 @@
    c"lol" !%"content" !?%(tvariant ["lol" , tint])
  ) @@ c'"lol" (!+%42) ;
  test "match-1" (
    let_in "x" (c"foo" !+%7 !?%(tvariant ["foo" , tint])) @@
    match_ !%"x" [
      "bar" , ("y" , !%"y" +% !+%1) ;
      "foo" , ("x" , !%"x" +% !%"x" +% !%"x") ;
    ]
  ) @@ !+%21 ;
  test "match-2" (
    let_in "x" (c"bar" !+%7 !?%(tvariant ["bar" , tint])) @@
    match_ !%"x" [
      "bar" , ("y" , !%"y" +% !+%1) ;
      "foo" , ("x" , !%"x" +% !%"x" +% !%"x") ;
    ]
  ) @@ !+%8 ;
  test_fail "missing match branch" (
    let_in "x" (c"wee" !+%7 !?%(tvariant ["wee" , tint])) @@
    match_ !%"x" [
      "bar" , ("y" , !%"y" +% !+%1) ;
      "foo" , ("x" , !%"x" +% !%"x" +% !%"x") ;
    ]  
  ) ;
  (
    let tnat =
      tmu "self" @@ 
      tevariant [
        "zero" , teunit ;
        "succ" , tevar "self" ;
      ]
    in
    let zero = fold (c'"zero" unit) !?%tnat in
    let succ = func "x" !?%tnat @@ fold (c'"succ" !%"x") !?%tnat in
    test "pred function" (
      let_in "zero" zero @@
      let_in "succ" succ @@
      let_in "pred" (
        func "x" !?%tnat @@
        match_ (unfold !%"x") [
          "zero" , ("_" , !%"zero") ;
          "succ" , ("pred" , !%"pred") ;
        ]
      ) @@
      let_in "to_int" (
        func "x" !?%tnat @@
        match_ (unfold !%"x") [
          "zero" , ("_" , !+%0) ;
          "succ" , ("_" , !+%1) ;
        ]      
      ) @@
      record [
        "0" , !%"to_int" @% !%"zero" ;
        "1" , !%"to_int" @% !%"succ" @% !%"zero" ;
        "2" , !%"to_int" @% !%"pred" @% !%"succ" @% !%"zero" ;
      ]
    ) (record [
      "0" , !+%0 ;
      "1" , !+%1 ;
      "2" , !+%0 ; 
    ]) ;
    let to_int =
      rec_ "self" !?%(tarrow tnat tint) @@
      func "x" !?%tnat @@
      match_ (unfold !%"x") [
        "zero" , ("_" , !+%0) ;
        "succ" , ("pred" , !+%1 +% (!%"self" @% !%"pred")) ;
      ] 
    in
    test "peano to_int function"  (
      let_in "zero" zero @@
      let_in "succ" succ @@
      let_in "to_int" to_int @@
      record [
        "0" , !%"to_int" @% !%"zero" ;
        "1" , !%"to_int" @% !%"succ" @% !%"zero" ;
        "2" , !%"to_int" @% !%"succ" @% !%"succ" @% !%"zero" ;
        "3" , !%"to_int" @% !%"succ" @% !%"succ" @% !%"succ" @% !%"zero" ;
      ]
    ) (tuple @@ List.map (!+%) [0;1;2;3]) ;
    test "peano add" (
      let_in "zero" zero @@
      let_in "succ" succ @@
      let_in "to_int" to_int @@
      let_in "add" (
        rec_ "add" !?% (tarrow tnat (tarrow tnat tnat)) @@
        func "x" !?%tnat @@ func "y" !?%tnat @@
        match_ (unfold !%"y") [
          "zero" , ("_" , !%"x") ;
          "succ" , ("pred_y" , !%"add" <|% (!%"succ" @% !%"x") <|% !%"pred_y") ;
        ]
      ) @@
      tuple @@ List.map (fun x -> !%"to_int" @% x) @@ [
        !%"add" <|% !%"zero" <|% !%"zero" ;
        !%"add" <|% !%"succ" @% !%"zero" <|% !%"zero" ;
        !%"add" <|% !%"succ" @% !%"zero" <|% !%"succ" @% !%"zero" ;
        !%"add" <|% !%"zero" <|% !%"succ" @% !%"zero";
        !%"add" <|% !%"succ" @% !%"zero" <|% !%"succ" @% !%"succ" @% !%"zero" ;
      ] ;
    ) @@ tuple @@ List.map (!+%) [
      0 ;
      1 ;
      2 ;
      1 ;
      3 ;
    ] ;
  ) ;
  ()

let test_synthesize () =
  let open AU in
  let test name x y =
    let exception Fail_synthesis in
    test_basic (O.asprintf "@{<it>synthesize@} ; %s" name) @@ fun () ->
    let synth = toplevel_synthesize x in
    if (synth <> y) then (
      O.eprintf "@[<v>Different types.@;[@{<green>%a@}]@;vs@;[@{<red>%a@}]@;@]"
      AT.pp_texpr !?%synth AT.pp_texpr !?%y ;
      raise Fail_synthesis
    )
  in
  let test_fail name x =
    test_basic (O.asprintf "@{<it>synthesize@} ; %s @{<it>(fail)@}" name) @@ fun () ->
    let exception No_fail in
    try (
      ignore (toplevel_synthesize x) ;
      raise No_fail
    ) with No_fail -> raise No_fail | _ -> ()
  in
  test "int" !+%42 tint ;
  test "add" (!+%23 +% !+%11) tint ;
  test_fail "add, int and string" (!+%23 +% !^%"lel") ;
  test "string" !^%"lel" tstring ;
  test "annot" (annot !+%42 !?%tint) tint ;
  test_fail "wrong annot" (annot !+%42 !?%tstring) ;
  test "literal" (annot !+%42 !?%(tlint 42)) (tlint 42) ;
  test_fail "wrong literal" (annot !+%42 !?%(tlint 43)) ;
  test "let in" (
    let_in "x" (annot !^%"lol" !?%(tlstring "lol")) @@
    !%"x"
  ) (tlstring "lol") ;
  test_fail "let in, missing var" (
    let_in "x" (annot !^%"lol" !?%(tlstring "lol")) @@
    !%"y"
  ) ;
  test "function" (
    func "x" !?%tint @@ !%"x" +% !%"x"
  ) (tarrow tint tint) ;
  test "app" (
    let_in "f" (
      func "x" !?%tstring @@ !+%42
    ) @@
    !%"f" @% !^%"wee"
  ) tint ;
  test_fail "app wrong param" (
    let_in "f" (
      func "x" !?%tstring @@ !+%42
    ) @@
    !%"f" @% !+%42
  ) ;
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
  test_fail "wrong field" (
    let_in "x" (record [
      "foo" , !+%42 ;
      "bar" , !^%"lol" ;
    ]) @@
    !%"x" /% "wee"
  ) ;
  (
    let tvar = tvariant [
      "foo" , tint ;
      "bar" , tstring ;
    ] in
    test "case-1" (
      c"foo" !+%42 !?%tvar
    ) tvar ;
    test "case-2" (
      c"bar" !^%"answer" !?%tvar
    ) tvar ;
    test_fail "case non-matching variant" (
      c"wee" !^%"lol" !?%tvar
    ) ;
    test "match" (
      let_in "x" (c"foo" !+%42 !?%tvar) @@
      match_ !%"x" [
        "foo" , ("y" , !%"y") ;
        "bar" , ("y" , !+%23) ;
      ]
    ) tint ;
    test_fail "missing match branch" (
      let_in "x" (c"foo" !+%42 !?%tvar) @@
      match_ !%"x" [
        "bar" , ("y" , !+%23) ;
      ]
    ) ;
    test_fail "extra match branch" (
      let_in "x" (c"foo" !+%42 !?%tvar) @@
      match_ !%"x" [
        "foo" , ("y" , !%"y") ;
        "bar" , ("y" , !+%23) ;
        "wee" , ("y" , !+%23) ;
      ]
    ) ;
  ) ;
  (
    let tnat =
      tmu "self" @@ 
      tevariant [
        "zero" , teunit ;
        "succ" , tevar "self" ;
      ]
    in
    test "recursive nat 0" (
      fold (c'"zero" unit) !?%tnat
    ) tnat ;
    test "recursive nat 1" (
      let_in "x" (fold (c'"zero" unit) !?%tnat) @@
      fold (c'"succ" !%"x") !?%tnat
    ) tnat ;
    test "recursive constructor wrappers" (
      let_in "zero" (fold (c'"zero" unit) !?%tnat) @@
      let_in "succ" (func "pred" !?%tnat @@
        fold (c'"succ" !%"pred") !?%tnat
      ) @@
      !%"succ" @% !%"succ" @% !%"zero"
    ) tnat ;
    test_fail "recursive constructor wrappers, mutant 0" (
      let_in "zero" (fold (c'"zero" unit) !?%tnat) @@
      let_in "succ" (func "pred" !?%tnat @@
       (c'"succ" !%"pred")
      ) @@
      !%"succ" @% !%"succ" @% !%"zero"
    ) ;
    test_fail "recursive constructor wrappers, mutant 1" (
      let_in "zero" (fold (c'"zero" unit) !?%tnat) @@
      let_in "succ" (func "pred" !?%tnat @@
        fold (c'"succ" !%"pred") !?%tnat
      ) @@
      !%"succ" @% !%"suc" @% !%"zero"
    ) ;
    test "recursive nat matching" (
      let_in "zero" (fold (c'"zero" unit) !?%tnat) @@
      match_ (unfold !%"zero") [
        "zero" , ("_" , !^%"foo") ;
        "succ" , ("_" , !^%"bar") ;
      ]
    ) tstring ;
    test "pred function" (
      func "x" !?%tnat @@
      match_ (unfold !%"x") [
        "zero" , ("_" , (fold (c'"zero" unit) !?%tnat)) ;
        "succ" , ("pred" , !%"pred") ;
      ]
    ) (tarrow tnat tnat) ;
    test_fail "pred function, mutant 0" (
      func "x" !?%tnat @@
      match_ (unfold !%"x") [
        "zero" , ("_" , (fold (c'"zero" unit) !?%tnat)) ;
        "succ" , ("pred" , !+%42) ;
      ]
    ) ;
    let to_int =
      rec_ "self" !?%(tarrow tnat tint) @@
      func "x" !?%tnat @@
      match_ (unfold !%"x") [
        "zero" , ("_" , !+%0) ;
        "succ" , ("pred" , !+%1 +% (!%"self" @% !%"pred")) ;
      ] 
    in
    test "type peano to_int" to_int (tarrow tnat tint) ;
    test "type peano add" (
      let_in "zero" (fold (c'"zero" unit) !?%tnat) @@
      let_in "succ" (func "pred" !?%tnat @@
        fold (c'"succ" !%"pred") !?%tnat
      ) @@
      rec_ "add" !?% (tarrow tnat (tarrow tnat tnat)) @@
      func "x" !?%tnat @@ func "y" !?%tnat @@
      match_ (unfold !%"y") [
        "zero" , ("_" , !%"x") ;
        "succ" , ("pred_y" , !%"add" <|% (!%"succ" @% !%"x") <|% !%"pred_y") ;
      ]
    ) (tarrow tnat (tarrow tnat tnat)) ;
  ) ;
  test "recursive values inhabit all types: int" (
    rec_ "self" !?%tint !%"self"
  ) tint ;
  test "recursive values inhabit all types: string" (
    rec_ "self" !?%tstring !%"self"
  ) tstring ;
  ()

let () =
  test_eval () ;
  test_synthesize () ;
  ()