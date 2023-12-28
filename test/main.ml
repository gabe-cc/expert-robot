module O = Ocolor_format
module AT = Agaml_core.Types
module AU = Agaml_core.Utils

let ftctx vs ts ns : AT.ftctx =
  let open AT in TForward ((
    ns |> List.map @@ fun (name , x) ->
    name , TCNamespace x  
  ) @ (  
    ts |> List.map @@ fun (name , x) ->
    name , TCType x
  ) @ (
    vs |> List.map @@ fun (name , x) ->
    name , TCTerm x
  ))


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
  let test_namespace name x y =
    let exception Fail_eval in
    test_basic (O.asprintf "@{<it>eval@} ; %s" name) @@ fun () ->
    let x' =
      match toplevel_neval x with
      | Full x -> x
      | Partial x -> (
        O.eprintf "@[<v>Partial evalaution only.@;[@{<red>%a@}@]"
          AT.pp_nexpr x ;
        raise Fail_eval
      )
    in
    if (x' <> y) then (
      O.eprintf "@[<v>Different values.@;[@{<green>%a@}]@;vs@;[@{<red>%a@}]@;@]"
      AT.pp_nexpr x' AT.pp_nexpr y ;
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
    let_in "inc" (func "x" tint @@ !%"x" +% !+%1) @@
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
    c"lol" !%"content" (tvariant ["lol" , tint])
  ) @@ c'"lol" (!+%42) ;
  test "match-1" (
    let_in "x" (c"foo" !+%7 (tvariant ["foo" , tint])) @@
    match_ !%"x" [
      "bar" , ("y" , !%"y" +% !+%1) ;
      "foo" , ("x" , !%"x" +% !%"x" +% !%"x") ;
    ]
  ) @@ !+%21 ;
  test "match-2" (
    let_in "x" (c"bar" !+%7 (tvariant ["bar" , tint])) @@
    match_ !%"x" [
      "bar" , ("y" , !%"y" +% !+%1) ;
      "foo" , ("x" , !%"x" +% !%"x" +% !%"x") ;
    ]
  ) @@ !+%8 ;
  test_fail "missing match branch" (
    let_in "x" (c"wee" !+%7 (tvariant ["wee" , tint])) @@
    match_ !%"x" [
      "bar" , ("y" , !%"y" +% !+%1) ;
      "foo" , ("x" , !%"x" +% !%"x" +% !%"x") ;
    ]  
  ) ;
  (
    let tnat =
      tmu "self" @@ 
      tvariant [
        "zero" , tunit ;
        "succ" , tvar "self" ;
      ]
    in
    let zero = fold (c'"zero" unit) tnat in
    let succ = func "x" tnat @@ fold (c'"succ" !%"x") tnat in
    test "pred function" (
      let_in "zero" zero @@
      let_in "succ" succ @@
      let_in "pred" (
        func "x" tnat @@
        match_ (unfold !%"x") [
          "zero" , ("_" , !%"zero") ;
          "succ" , ("pred" , !%"pred") ;
        ]
      ) @@
      let_in "to_int" (
        func "x" tnat @@
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
      rec_ "self" (tarrow tnat tint) @@
      func "x" tnat @@
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
        rec_ "add"  (tarrow tnat (tarrow tnat tnat)) @@
        func "x" tnat @@ func "y" tnat @@
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
  (
    let list =
      tfunc "A" @@ tmu "list" @@
      tvariant [
        "nil" , tunit ;
        "cons" , ttuple [tvar "A" ; tvar "list"]
      ]
    in
    test "parametric lists" (
      let_type_in "list" list @@
      let_in "nil" (
        funct "X" @@
        fc"nil" unit (tcall (tvar "list") (tvar "X"))
      ) @@
      let_in "cons" (
        funct "X" @@
        func "hd" (tvar "X") @@ func "tl" (tcall (tvar "list") (tvar "X")) @@
        fc"cons" (tuple [ !%"hd" ; !%"tl"]) (!?%"list" @?% !?%"X")
      ) @@
      tuple [
        callt !%"nil" tint ;
        (callt !%"cons" tstring) <|% !^%"lol" <|% (callt !%"nil" tstring) ;
      ]
    ) (tuple [
      fc'"nil" unit ;
      fc'"cons" (tuple [!^%"lol" ; fc'"nil" unit]) ;
    ]) ;  
  ) ;
  test_namespace "namespace simple" (nstatements [
    slet "x" @@ !+%42 +% !+%1 ;
  ]) (nmap [
    "x" , !+%43
  ] []) ;
  test_namespace "namespace nested" (nstatements [
    slet_namespace "N" (nstatements [
      slet "y" @@ !+%3 +% !+%1 ;
    ]) ;
    slet "x" @@ !+%1 +% (naccess (nvar "N") "y") ;
  ]) (nmap [
    "x" , !+%5
  ] [
    "N" , nmap ["y" , !+%4] []
  ]) ;
  (* (
    test "simple namespace" (

    ) !+%42
  ) ; *)
  ()

let test_synthesize () =
  let open AU in
  let test name x y =
    let exception Fail_synthesis in
    test_basic (O.asprintf "@{<it>synthesize@} ; %s" name) @@ fun () ->
    let _ , synth = toplevel_synthesize x in
    let y' = toplevel_teval y in
    if (not @@ Agaml_ast.Equality.subtype synth y') then (
      O.eprintf "@[<v>Not subtype.@;Synthesized:[@{<green>%a@}]@;vs@;Expected:[@{<red>%a@}]@;@]"
      AT.pp_texpr synth AT.pp_texpr y ;
      raise Fail_synthesis
    )
  in
  let test_namespace name x y =
    let exception Fail_synthesis in
    test_basic (O.asprintf "@{<it>synthesize namespace@} ; %s" name) @@ fun () ->
    let _nexpr , tnexpr = toplevel_synthesize_namespace x in
    (* let y' = toplevel_teval_tctx y in *)
    if (tnexpr <> y) then (
      O.eprintf "@[<v>Different contexts.@;Got [@{<green>%a@}]@;vs@;Expected [@{<red>%a@}]@;@]"
      AT.pp_tnexpr tnexpr AT.pp_tnexpr y ;
      raise Fail_synthesis
    )
  in
  let test_statements name x y =
    let exception Fail_synthesis in
    test_basic (O.asprintf "@{<it>synthesize statements@} ; %s" name) @@ fun () ->
    let tctx , _ = toplevel_synthesize_statements x in
    (* let y' = toplevel_teval_tctx y in *)
    if (tctx <> y) then (
      O.eprintf "@[<v>Different contexts.@;Got [@{<green>%a@}]@;vs@;Expected [@{<red>%a@}]@;@]"
      AT.pp_tctx tctx AT.pp_tctx y ;
      raise Fail_synthesis
    )
  in
  let test_statements_full name x y z =
    let exception Fail_synthesis in
    test_basic (O.asprintf "@{<it>synthesize statements@} ; %s" name) @@ fun () ->
    let tctx , x' = toplevel_synthesize_statements x in
    (* let y' = toplevel_teval_tctx y in *)
    if (tctx <> y) then (
      O.eprintf "@[<v>Different contexts.@;Got [@{<green>%a@}]@;vs@;Expected [@{<red>%a@}]@;@]"
      AT.pp_tctx tctx AT.pp_tctx y ;
      raise Fail_synthesis
    ) ;
    if (x' <> z) then (
      O.eprintf "@[<v>Different statementss.@;Got [@{<green>%a@}]@;vs@;Expected [@{<red>%a@}]@;@]"
      AT.pp_statements x' AT.pp_statements z ;
      raise Fail_synthesis
    )
  in
  let test_full name x y z =
    let exception Fail_synthesis in
    test_basic (O.asprintf "@{<it>synthesize-full@} ; %s" name) @@ fun () ->
    let x' , synth = toplevel_synthesize x in
    if (x' <> y) then (
      O.eprintf "@[<v>Different exprs.@;What I got: [@{<green>%a@}]@;vs@;What I expected: [@{<red>%a@}]@;@]"
      AT.pp_expr x' AT.pp_expr y ;
      raise Fail_synthesis    
    ) ;
    let z' = toplevel_teval z in
    if (synth <> z') then (
      O.eprintf "@[<v>Different types.@;[@{<green>%a@}]@;vs@;[@{<red>%a@}]@;@]"
      AT.pp_texpr synth AT.pp_texpr z ;
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
  test "annot" (annot !+%42 tint) tint ;
  test_fail "wrong annot" (annot !+%42 tstring) ;
  test "literal" (annot !+%42 (tlint 42)) (tlint 42) ;
  test_fail "wrong literal" (annot !+%42 (tlint 43)) ;
  test "let in" (
    let_in "x" (annot !^%"lol" (tlstring "lol")) @@
    !%"x"
  ) (tlstring "lol") ;
  test_fail "let in, missing var" (
    let_in "x" (annot !^%"lol" (tlstring "lol")) @@
    !%"y"
  ) ;
  test "function" (
    func "x" tint @@ !%"x" +% !%"x"
  ) (tarrow tint tint) ;
  test "app" (
    let_in "f" (
      func "x" tstring @@ !+%42
    ) @@
    !%"f" @% !^%"wee"
  ) tint ;
  test_fail "app wrong param" (
    let_in "f" (
      func "x" tstring @@ !+%42
    ) @@
    !%"f" @% !+%42
  ) ;
  test "closure" (
    let_in "f" (
      func "x" tint @@ func "y" tint @@ !%"x" +% !%"y"
    ) @@
    !%"f" @% !+%42
  ) (tarrow tint tint) ;
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
      c"foo" !+%42 tvar
    ) tvar ;
    test "case-2" (
      c"bar" !^%"answer" tvar
    ) tvar ;
    test_fail "case non-matching variant" (
      c"wee" !^%"lol" tvar
    ) ;
    test "match" (
      let_in "x" (c"foo" !+%42 tvar) @@
      match_ !%"x" [
        "foo" , ("y" , !%"y") ;
        "bar" , ("y" , !+%23) ;
      ]
    ) tint ;
    test_fail "missing match branch" (
      let_in "x" (c"foo" !+%42 tvar) @@
      match_ !%"x" [
        "bar" , ("y" , !+%23) ;
      ]
    ) ;
    test_fail "extra match branch" (
      let_in "x" (c"foo" !+%42 tvar) @@
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
      tvariant [
        "zero" , tunit ;
        "succ" , tvar "self" ;
      ]
    in
    test "recursive nat 0" (
      fold (c'"zero" unit) tnat
    ) tnat ;
    test "recursive nat 1" (
      let_in "x" (fold (c'"zero" unit) tnat) @@
      fold (c'"succ" !%"x") tnat
    ) tnat ;
    test "recursive constructor wrappers" (
      let_in "zero" (fold (c'"zero" unit) tnat) @@
      let_in "succ" (func "pred" tnat @@
        fold (c'"succ" !%"pred") tnat
      ) @@
      !%"succ" @% !%"succ" @% !%"zero"
    ) tnat ;
    test_fail "recursive constructor wrappers, mutant 0" (
      let_in "zero" (fold (c'"zero" unit) tnat) @@
      let_in "succ" (func "pred" tnat @@
       (c'"succ" !%"pred")
      ) @@
      !%"succ" @% !%"succ" @% !%"zero"
    ) ;
    test_fail "recursive constructor wrappers, mutant 1" (
      let_in "zero" (fold (c'"zero" unit) tnat) @@
      let_in "succ" (func "pred" tnat @@
        fold (c'"succ" !%"pred") tnat
      ) @@
      !%"succ" @% !%"suc" @% !%"zero"
    ) ;
    test "recursive nat matching" (
      let_in "zero" (fold (c'"zero" unit) tnat) @@
      match_ (unfold !%"zero") [
        "zero" , ("_" , !^%"foo") ;
        "succ" , ("_" , !^%"bar") ;
      ]
    ) tstring ;
    test "pred function" (
      func "x" tnat @@
      match_ (unfold !%"x") [
        "zero" , ("_" , (fold (c'"zero" unit) tnat)) ;
        "succ" , ("pred" , !%"pred") ;
      ]
    ) (tarrow tnat tnat) ;
    test_fail "pred function, mutant 0" (
      func "x" tnat @@
      match_ (unfold !%"x") [
        "zero" , ("_" , (fold (c'"zero" unit) tnat)) ;
        "succ" , ("pred" , !+%42) ;
      ]
    ) ;
    let to_int =
      rec_ "self" (tarrow tnat tint) @@
      func "x" tnat @@
      match_ (unfold !%"x") [
        "zero" , ("_" , !+%0) ;
        "succ" , ("pred" , !+%1 +% (!%"self" @% !%"pred")) ;
      ] 
    in
    test "type peano to_int" to_int (tarrow tnat tint) ;
    test "type peano add" (
      let_in "zero" (fold (c'"zero" unit) tnat) @@
      let_in "succ" (func "pred" tnat @@
        fold (c'"succ" !%"pred") tnat
      ) @@
      rec_ "add"  (tarrow tnat (tarrow tnat tnat)) @@
      func "x" tnat @@ func "y" tnat @@
      match_ (unfold !%"y") [
        "zero" , ("_" , !%"x") ;
        "succ" , ("pred_y" , !%"add" <|% (!%"succ" @% !%"x") <|% !%"pred_y") ;
      ]
    ) (tarrow tnat (tarrow tnat tnat)) ;
  ) ;
  test "recursive values inhabit all types: int" (
    rec_ "self" tint !%"self"
  ) tint ;
  test "recursive values inhabit all types: string" (
    rec_ "self" tstring !%"self"
  ) tstring ;
  test_full "static eval full literal" (
    annot (eval_full @@ !+%13 +% !+%29) (tlint 42)
  ) (annot (!+%42) (tlint 42)) (tlint 42) ;
  test_full "static eval partial fun body" (
    func "x" tint @@
    eval_partial @@
    !%"x" +% (!+%7 +% !+%43)
  ) (func "x" tint @@ !%"x" +% !+%50) (tarrow tint tint) ;
  test_full "static eval partial fun body record" (
    func "x" tint @@
    eval_partial @@
    field (record [ "a" , !+%42 ; "b" , var "x"]) "a"
  ) (func "x" tint @@ !+%42) (tarrow tint tint) ;
  test_fail "static eval full fun body" (
    func "x" tint @@
    eval_full @@
    !%"x" +% (!+%7 +% !+%43)
  ) ;
  (
    let id = 
      funct "A" @@ func "x" (tvar "A") @@
      !%"x"
    in
    let id_ty =
      tfunc "A" @@ tarrow (tvar "A") (tvar "A")
    in
    test "polymorphic id" id id_ty ;
    test "polymorphism app" (
      let_in "id" id @@
      tuple [
        (callt !%"id" tint) @% !+%42 ;
        (callt !%"id" tstring) @% !^%"lol" ; 
      ]
    ) (ttuple [
      tint ;
      tstring ;
    ]) ;
    test_fail "polymorphism mis-application" (
      let_in "id" id @@
      (callt !%"id" tint) @% !^%"lol" ;
    ) ;
    test_fail "polymorphism no callt" (
      let_in "id" id @@
      !%"id" @% !+%42 ;
    ) ;
    test "polymorphic id apps" (
      tuple [ callt id tint ; callt id tstring ]
    ) @@ ttuple [ tarrow tint tint ; tarrow tstring tstring ] ;
    test "id . id" (
      let_in "id" id @@
      (callt !%"id" id_ty) @% !%"id"
    ) id_ty ;
  ) ;
  (* AT.Synthesize_log_steps.with_flag @@ fun () -> *)
  (
    let pair =
      funct "A" @@ funct "B" @@
      func "x" (tvar "A") @@ func "y" (tvar "B") @@
      tuple [!%"x" ; !%"y"]
    in
    test "polymorphism double" (
      let_in "pair" pair @@
      (callt (callt !%"pair" tint) tstring)
    ) (tarrow tint @@ tarrow tstring @@ ttuple [tint ; tstring]) ;
  ) ;
  (
    let bool = tvariant [ "true" , tunit ; "false" , tunit ] in
    test "let type in" (
      let_type_in "bool" bool @@
      let_in "true" (c"true" unit (tvar "bool")) @@
      let_in "false" (c"false" unit (tvar "bool")) @@
      let_type_in "foobar" (
        tvariant [ "foo" , tint ; "bar" , tstring ]
      ) @@
      let_in "foo" (func "x" tint @@ c"foo" !%"x" (tvar "foobar")) @@
      let_in "bar" (func "x" tstring @@ c"bar" !%"x" (tvar "foobar")) @@
      let_in "is_foo" (func "x" (tvar "foobar") @@
        match_ !%"x" [
          "foo" , ("_" , !%"true") ;
          "bar" , ("_" , !%"false") ;
        ]
      ) @@
      tuple [
        !%"is_foo" @% !%"foo" @% !+%42 ;
        !%"is_foo" @% !%"bar" @% !^%"lol" ;
      ]
    ) (ttuple [bool ; bool]) ;
    let list =
      tfunc "A" @@ tmu "list" @@
      tvariant [
        "nil" , tunit ;
        "cons" , ttuple [tvar "A" ; tvar "list"]
      ]
    in
    (* AT.Synthesize_log_steps.with_flag @@ fun () ->
    AT.TEval_log_steps.with_flag @@ fun () -> *)
    test "parametric lists" (
      let_type_in "list" list @@
      let_in "nil" (
        funct "X" @@
        fc"nil" unit (tcall (tvar "list") (tvar "X"))
      ) @@
      let_in "cons" (
        funct "X" @@
        func "hd" (tvar "X") @@ func "tl" (tcall (tvar "list") (tvar "X")) @@
        fc"cons" (tuple [ !%"hd" ; !%"tl"]) (!?%"list" @?% !?%"X")
      ) @@
      tuple [
        callt !%"nil" tint ;
        (callt !%"cons" tstring) <|% !^%"lol" <|% (callt !%"nil" tstring) ;
      ]
    ) (ttuple [
      tcall list tint ;
      tcall list tstring ;
    ]) ;
  ) ;
  test "apply id to 2 params" (
    let_in "id" (
      funct "T" @@
      func "x" (tvar "T") (var "x")
    ) @@
    tuple [
      call (callt (var "id") tint) !+%42 ;
      call (callt (var "id") (ttuple [tint ; tint]))
        (tuple [!+%23 ; !+%0])
    ]
  ) (ttuple [
    tint ;
    ttuple [tint ; tint] ;
  ]);
  test "id diff name" (
    funct "T" @@
    func "x" (tvar "T") (var "x")
  ) (
    (tfunc "U" @@ tarrow (tvar "U") (tvar "U") )
  ) ;  
  test "apply higher order to id" (
    let_in "id" (
      funct "T" @@
      func "x" (tvar "T") (var "x")
    ) @@
    let_in "double_apply" (
      funct "T" @@
      func "f" (tarrow (tvar "T") (tvar "T")) @@
      func "x" (tvar "T") @@
      !%"f" @% !%"f" @% !%"x"
    ) @@
    ((callt (var "double_apply") tint) @% (callt (var "id") tint)) @% !+%42
  ) (
    tint
  ) ;
  test "id self-apply" (
    let_in "id" (
      funct "T" @@
      func "x" (tvar "T") (var "x")    
    ) @@
    let_in "self_apply" (
      (callt !%"id" (
        (tfunc "U" @@ tarrow (tvar "U") (tvar "U"))
      )) @% !%"id"
    ) @@
    !%"self_apply" ;
  ) (
    (tfunc "U" @@ tarrow (tvar "U") (tvar "U"))
  ) ;
  test_statements "simple statements" [
    slet "x" !+%42 ;
    slet "y" !%"x" ;
  ] (TC.from_forward @@ ftctx [
    "x" , tint ;
    "y" , tint ;
  ] [] []) ;
  test_statements "statements + types" [
    stlet "A" @@ ttuple [tint ; tstring] ;
    slet "x" @@ annot (tuple [!+%42 ; !^%"lol"]) (tvar "A") ;
  ] (TC.from_forward @@ ftctx [
    "x" , ttuple [tint ; tstring]
  ] [
    "A" , Value (ttuple [tint ; tstring])
  ] []) ;
  test_statements_full "statements + static eval" [
    slet "x" @@ eval_full @@ !+%1 +% !+%3 ;
  ] (TC.from_forward @@ ftctx [
    "x" , tint ;
  ] [] []) [
    slet "x" !+%4 ;  
  ] ;
  test_statements_full "statements + no eval" [
    slet "x" @@ !+%1 +% !+%3 ;
  ] (TC.from_forward @@ ftctx [
    "x" , tint ;
  ] [] []) [
    slet "x" @@ !+%1 +% !+%3 ;  
  ] ;
  test_statements_full "empty namespace" [
    slet_namespace "N" @@ nstatements [    
    ] ;
    slet "x" @@ !+%1 +% !+%3 ;
  ] (TC.from_forward @@ ftctx [
    "x" , tint ;
  ] [] [
    "N" , tnnamespace @@ ftctx [] [] []
  ]) [
    slet_namespace "N" @@ nstatements [
    ] ;
    slet "x" @@ !+%1 +% !+%3 ;
  ] ;
  test_statements_full "simple namespace" [
    slet_namespace "N" @@ nstatements [
      slet "y" @@ !+%1 +% !+%42
    ] ;
    slet "x" @@ (naccess (nvar "N") "y") +% !+%3 ;
  ] (TC.from_forward @@ ftctx [
    "x" , tint ;
  ] [] [
    "N" , tnnamespace @@ ftctx [
      "y" , tint ;
    ] [] []
  ]) [
    slet_namespace "N" @@ nstatements [
      slet "y" @@ !+%1 +% !+%42
    ] ;
    slet "x" @@ (naccess (nvar "N") "y") +% !+%3 ;
  ] ;
  let () =
    let module OList = Agaml.Std_lib.OList in
    test_namespace "list" (
      OList.namespace
    ) (tnnamespace @@ ftctx [
      "nil" , tfunc "T" @@ tarrow tunit @@ OList.ty_body (tvar "T") ;
      "nil_int" , tarrow tunit (OList.ty_body tint) ;
    ] [
      "list" , AT.Value (
        tfunc "T" @@ OList.ty_body (tvar "T")) ;
    ] [])
  in
  let () =
    let module OOption = Agaml.Std_lib.OOption in
    test_namespace "option" (
      OOption.namespace
    ) (tnnamespace @@ ftctx [
      "none" , OOption.ty ;
      "none_int" , OOption.ty_body tint ;
      "some" , tfunc "T" @@ tarrow (tvar "T") @@ OOption.ty_body (tvar "T") ;
    ] [
      "option" , AT.Value OOption.ty ;
    ] [])
  in
  ()

let () =
  test_eval () ;
  test_synthesize () ;
  ()