open Agaml_ast.Types
module C = Agaml_ast.Context
module TC = Agaml_ast.TContext
module Eq = Agaml_ast.Equality

module Flag = Agaml_tools.Flag
module TEval_log_steps = Flag()
module Synthesize_log_steps = Flag()

open Eval
  
let rec teval : tctx -> texpr -> texpr eval_result = fun tctx texpr ->
  if !TEval_log_steps.flag then (
    Format.printf "@[<v>TEval step:@;%a@]\n%!" pp_texpr texpr 
  ) ;
  match texpr with
  | TType -> full TType
  | TArrow (input , output) -> (
    let+ input' = teval tctx input in
    let+ output' = teval tctx output in
    full @@ TArrow (input' , output')
  )
  | TLiteral lit -> full @@ TLiteral lit
  | TBuiltin b -> full @@ TBuiltin b
  | TRecord lst -> (
    let is_full = ref true in
    let lst' = lst |> List.map (fun (name , content) -> 
      name ,
      match teval tctx content with
      | Partial x -> (
        is_full := false ;
        x
      )
      | Full x -> x
    ) in
    (if !is_full then full else partial) @@
    TRecord lst'
  )
  | TVariant lst -> (
    let is_full = ref true in
    let lst' = lst |> List.map (fun (name , content) -> 
      name ,
      match teval tctx content with
      | Partial x -> (
        is_full := false ;
        x
      )
      | Full x -> x
    ) in
    (if !is_full then full else partial) @@
    TVariant lst'
  )
  (*
    Dealing with TMu is problematic.
    Can never fully evaluate, because it is self-referential.
    Instead, put a Hole in the variable, reduce as much as you can below, and define this to be the semantics of `TMu`.
    Foldings and unfoldings only happen with the dedicated constructors.
  *)
  | TMu (var , body) -> (
    let tctx' = TC.append_thole var tctx in
    let body' =
      match teval tctx' body with
      | Full x | Partial x -> x
    in
    full @@ TMu (var , body')
  )
  | TVar var -> (
    match TC.lookup_t var tctx with
    | Some (Value tv) -> full tv
    | Some (Expr texpr') -> partial texpr'
    | Some Hole -> partial texpr
    | None -> failwith @@ F.asprintf "missing type variable (%s)" var
  )
  | TFunction (var , body) -> (
    (*
      Should reduce under the lambdas. But this implies termination of strong normalization for type level calculus. hmmmm
      TODO: check if should reduce under the lambda or not
    *)
    let tctx' = TC.append_thole var tctx in
    let body' =
      match teval tctx' body with
      | Full x | Partial x -> x
    in
    full @@ TFunction (var , body')
  ) 
  | TCall (f , arg) -> (
    let+ f' = teval tctx f in
    let+ arg' = teval tctx arg in
    match f' with
    | TFunction (var , body) -> (
      let tctx' = TC.append_t var arg' tctx in
      teval tctx' body
    )
    (* TODO: should this be partial only on variables, or also on other cases? *)
    | _ -> partial @@ TCall (f' , arg')
    (* | _ -> failwith "type calling a non-typefunction" *)
  )
  | TNamespace_access (nexpr , name) -> (
    let _nexpr' , nty = synthesize_namespace tctx nexpr in 
    match nty with
    | TNNamespace ftctx -> (
      match TC.lookup_t name @@ TC.from_forward ftctx with
      | None -> failwith @@ F.asprintf "did not find field %s in namespace" name
      | Some (Value tvalue) -> full tvalue
      | Some (Expr texpr') -> partial texpr'
      | Some Hole -> partial texpr
    )
  )

and tneval : tctx -> tnexpr -> tnexpr eval_result = fun tctx tnexpr ->
  match tnexpr with
  | TNNamespace (TForward tctx_n) -> (
    let tctx2 : tctx ref = ref tctx in
    let is_full = ref true in
    let tctx_n2 = tctx_n |> List.map (fun (name , binding) ->
      name ,
      match binding with
      | TCTerm texpr -> (
        let tvalue =
          match teval !tctx2 texpr with
          | Full x -> x
          | Partial x -> (
            is_full := false ;
            x
          )
        in
        tctx2 := TC.append name tvalue !tctx2 ;
        TCTerm tvalue
      )
      | TCType tm -> (
        let tm' =
          match tm with
          | Value tvalue -> Value tvalue
          | Expr texpr -> (
            match teval !tctx2 texpr with
            | Full tv -> Value tv
            | Partial texpr -> (
              is_full := false ;
              Expr texpr
            )
          )
          | Hole -> (
            is_full := false ;
            Hole
          )
        in
        tctx2 := TC.append_traw name tm' !tctx2 ;
        TCType tm'
      )
      | TCNamespace tnexpr -> (
        let tnvalue =
          match tneval !tctx2 tnexpr with
          | Full x -> x
          | Partial x -> (
            is_full := false ;
            x
          )
        in
        tctx2 := TC.append_n name tnvalue !tctx2 ;
        TCNamespace tnvalue
      )
    ) in

    (if !is_full then full else partial) @@ TNNamespace (TForward tctx_n2)
  )

and check : tctx -> expr -> texpr -> expr * unit = fun tctx expr ty ->
  match expr , ty with
  | Function (var , texp , body) , TArrow (input , output) -> (
    let var_ty =
      match teval tctx texp with
      | Partial x | Full x -> x
    in
    assert (Eq.subtype var_ty input) ;
    let tctx' = TC.append var var_ty tctx in
    let body' , () = check tctx' body output in
    Function (var , texp , body') , () (* TODO: texp -> tv *)
  )
  | Function _ , _ -> failwith "function expects tarrow"
  | Literal (LInt n) , TLiteral (LInt n') -> (
    Literal (LInt n) , assert (n = n')
  )
  | Literal (LString s) , TLiteral (LString s') -> (
    Literal (LString s) , assert (s = s')
  )
  | Case (name , content) , _ -> (
    match ty with
    | TVariant lst -> (
      match List.assoc_opt name lst with
      | Some ty -> (
        let content' , () = check tctx content ty in
        Case (name , content') , ()
      )
      | None -> failwith @@ F.sprintf "when type checking case, case (%s) did not match variant type" name
    )
    | _ -> failwith "when type checking case, got a non-variant type annotation"  
  )
  | Match (matchee , branches) , _ -> (
    let matchee' , matchee_ty = synthesize tctx matchee in
    match matchee_ty with
    | TVariant cases -> (
      if branches = [] then failwith "when type checking pattern matching, was empty" ;
      let branches' = branches |> List.map (fun (name , (var , body)) ->
        match List.assoc_opt name cases with
        | Some case -> (
          let tctx' = TC.append var case tctx in
          let body' , () = check tctx' body ty in
          name , (var , body')
        )
        | None -> failwith @@ F.sprintf "when type checking pattern matching, branch (%s) that was not part of the variant" name
      ) in
      cases |> List.iter (fun (name , _) ->
        if List.assoc_opt name branches = None then
          failwith @@ F.sprintf "missing branch (%s) in match case" name
      ) ;
      Match (matchee' , branches') , ()
    )
    | _ -> failwith "when type checking pattern matching, got a non-variant as matchee"
  )
  | Fold expr , TMu (var , body) -> (
    let tctx' = TC.append_t var ty tctx in
    let body_ty =
      match teval tctx' body with
      | Partial x | Full x -> x
    in
    let expr' , () = check tctx' expr body_ty in
    Fold expr' , ()
  )
  | Fold _ , _ -> failwith "folding a non-mu type"
  | Rec (var , body) , _ -> (
    let tctx' = TC.append var ty tctx in
    let body' , () = check tctx' body ty in
    Rec (var , body') , ()
  )
  | Builtin _ , _
  | Variable _ , _
  | Call _ , _
  | LetIn _ , _
  | Annotation _ , _
  | Literal _ , _
  | Record _ , _
  | Field _ , _
  | Unfold _ , _
  | FunctionT _ , _
  | CallT _ , _
  | LetInT _ , _
  (* | Namespace _ , _ *)
  | Namespace_access _ , _
  -> (
    let expr' , inferred_ty = synthesize tctx expr in
    if not (Eq.subtype inferred_ty ty) then (
      failwith @@ Format.asprintf "@[<v>When checking expression:@;%a@;Inferred type was:@;%a@;But expected type was:@;%a@;@]"
        pp_expr expr pp_texpr inferred_ty pp_texpr ty
    ) ;
    expr' , ()
  )
  (* | Closure (var , body , ctx) , TArrow (input , output) -> (
    let ctx' =
      tctx |> List.fold_left (fun acc (var , ctxm) ->
        let expr =
          match ctxm with
          | { value = Some x ; tvalue = _ } -> x
          | { value = None ; tvalue = _ } -> failwith "can't synthesize hole in closure context"
        in
        let ty =
          match ctxm.tvalue with
          | Some ty -> ty
          | None -> (
            let tctx' = TC.closure acc tctx in
            let _ , ty = synthesize tctx' expr in
            ty
          )
        in
        TC.append_full var expr ty acc
      ) [] |> List.rev
    in
    let tctx' = TC.closure ctx' tctx in
    let tctx'' = TC.append var input tctx' in
    check tctx'' body output
  )
  | Closure _ , _ -> failwith "closure need a function type" *)
  | Closure _ , _ -> failwith "can't type check closures"
  | Eval (full , expr) , _ -> (
    (*
      With static evaluation, do you typecheck _before_ and _after_ the static evaluation? Or only after?
      - If you check only after, you get a more specific type, that could make the typechecking succeed.
        - (1 + 1 : literal 2) should not typecheck
        - (eval (1+1) : literal 2) should typecheck
      - But you might also silence error.
        - should `(if true then 42 else "lol" : int)` typecheck?
      Right now, we only check after.
      TODO: Make this better. Check against a "reasonable over-approximation" of `ty` before.
     *)
    (* let expr' , () = check tctx expr ty in
    match eval (TC.to_ctx tctx) expr' with *)
    match eval (TC.to_ctx tctx) expr with
    | Full value -> (
      let value' , () = check tctx value ty in
      (* assert (value = value') *)
      value' , ()
    )
    | Partial expr when not full -> (
      let expr' , () = check tctx expr ty in
      expr' , ()
    )
    | Partial _expr -> failwith "can only partially evaluate full-eval annotated expression"
  )

(* invariant: synthesize a `tvalue` (result of teval) *)
and synthesize : tctx -> expr -> expr * texpr = fun tctx expr ->
  let counter = Synthesize_log_steps.counter_inc () in
  if !Synthesize_log_steps.flag then (
    Format.printf "@[<v>Synthesize step start (%d):@;%a@]\n%!" counter pp_expr expr 
  ) ;
  (fun (_expr , texpr) -> (
    if !Synthesize_log_steps.flag then
      Format.printf "@[<v>Synthesize step stop (%d):@;%a@]\n%!" counter pp_texpr texpr ;
    _expr , texpr
  )) @@
  match expr with
  | Builtin x -> synthesize_builtin tctx x
  | Variable var -> (
    match TC.lookup var tctx with
    | Some tv -> expr , tv
    | None -> failwith @@ F.sprintf "when type checking, variable not found (%s)" var
  )
  | Function (var , texp , body) -> (
    let tv =
      match teval tctx texp with
      | Full x | Partial x -> x
    in
    let tctx' = TC.append var tv tctx in
    let body' , body_ty = synthesize tctx' body in
    Function (var , texp , body') , TArrow (tv , body_ty) (* TODO: texp -> tv *)
  )
  | LetIn (var , exp , body) -> (
    let exp' , tv = synthesize tctx exp in
    let tctx' = TC.append var tv tctx in
    let body' , body_ty = synthesize tctx' body in
    LetIn (var , exp' , body') , body_ty
  )
  | Call (f , arg) -> (
    let f' , f_ty = synthesize tctx f in
    match f_ty with
    | TArrow (input , output) -> (
      let arg' , () = check tctx arg input in
      Call (f' , arg') , output
    )
    | _ -> failwith @@ F.sprintf "when type checking application, left part did not have function type"
  )
  | Annotation (expr , ty) -> (
    let ty' =
      match teval tctx ty with
      | Partial x | Full x -> x
    in
    let expr' , () = check tctx expr ty' in
    Annotation (expr' , ty) , ty'
  )
  | Literal lit -> (
    match lit with
    | LInt _ -> Literal lit , TBuiltin (TInt)
    | LString _ -> Literal lit , TBuiltin (TString)
  )
  | Record lst -> (
    let lst' =
      List.map
        (fun (name , content) -> name , synthesize tctx content)
        lst
    in
    let lst_expr = List.map (fun (name , (expr , _ty)) -> name , expr) lst' in
    let lst_ty = List.map (fun (name , (_expr , ty)) -> name , ty) lst' in
    Record lst_expr , TRecord lst_ty
  )
  | Field (record , name) -> (
    let record' , record_ty = synthesize tctx record in
    match record_ty with
    | TRecord lst -> (
      match List.assoc_opt name lst with
      | Some ty -> Field (record' , name) , ty
      | None -> failwith @@ F.sprintf "when type checking field, field (%s) was missing" name
    )
    | _ -> failwith "when type checking field, didn't get a record"
  )
  | Case _ -> failwith "can not synthesize case, need a type annotation"
  | Match (matchee , branches) -> (
    let matchee' , matchee_ty = synthesize tctx matchee in
    match matchee_ty with
    | TVariant cases -> (
      match branches with
      | [] -> failwith "when type checking pattern matching, was empty"
      | hd_branch :: tl_branches -> (
        let hd_branch' , hd_branch_ty =
          let (name , (var , body)) = hd_branch in
          match List.assoc_opt name cases with
          | Some case -> (
            let tctx' = TC.append var case tctx in
            let body' , body_ty = synthesize tctx' body in
            (name , (var , body')) , body_ty
          )
          | None -> failwith @@ F.sprintf "when type checking pattern matching, branch (%s) that was not part of the variant" name
        in
        let tl_branches' =
          tl_branches |> List.map @@ fun (name , (var , body)) ->
          match List.assoc_opt name cases with
          | Some case -> (
            let tctx' = TC.append var case tctx in
            let body' , () = check tctx' body hd_branch_ty in
            (name , (var , body'))
          )
          | None -> failwith @@ F.sprintf "when type checking pattern matching, branch (%s) that was not part of the variant" name
        in
        let () =
          cases |> List.iter (fun (name , _) ->
            if List.assoc_opt name branches = None then
              failwith @@ F.sprintf "missing branch (%s) in match case" name
          )
        in
        let branches' = hd_branch' :: tl_branches' in
        Match (matchee' , branches') , hd_branch_ty
      )
    )
    | _ -> failwith "when type checking pattern matching, got a non-variant as matchee"
  )
  | Fold _ -> failwith "can not synthesize fold, need a type annotation"
  | Unfold body -> (
    let body' , body_ty = synthesize tctx body in
    match body_ty with
    | TMu (var , tbody) -> (
      let tctx' = TC.append_t var body_ty tctx in
      let tbody' =
        match teval tctx' tbody with
        | Full x -> x
        | Partial _ -> failwith "unfolding mu should be full"
      in
      Unfold body' , tbody'
    )
    | _ -> failwith "unfolding a non-mu type"
  )
  | Rec (_ , _) -> failwith "can not synthesize rec, need a type annotation"
  | Closure _ -> failwith "can not synthesize closure, need a type annotation"
  | Eval (full , expr) -> (
    let expr' , ty = synthesize tctx expr in
    match eval (TC.to_ctx tctx) expr' with
    | Full value -> (
      let value' , () = check tctx value ty in
      (* assert (value = value') *)
      value' , ty
    )
    | Partial expr when not full -> (
      let expr' , () = check tctx expr ty in
      expr' , ty
    )
    | Partial _expr -> failwith "can only partially evaluate full-eval annotated expression"
  )
  | FunctionT (var , body) -> (
    let tctx' = TC.append_thole var tctx in
    let body' , body_ty = synthesize tctx' body in
    FunctionT (var , body') , TFunction (var , body_ty)
  )
  | CallT (f , arg) -> (
    let f' , f_ty = synthesize tctx f in
    let arg' =
      match teval tctx arg with
      | Partial x | Full x -> x
    in
    match f_ty with
    | TFunction (var , body) -> (
      let tctx' = TC.append_t var arg' tctx in
      let body' =
        match teval tctx' body with
        | Partial x | Full x -> x
      in
      CallT (f' , arg) , body'
    )
    | _ -> failwith "callt on non-tfunction type"
  )
  | LetInT (var , texpr , body) -> (
    let tv =
      match teval tctx texpr with
      | Partial x | Full x -> x
    in
    (* if !Synthesize_log_steps.flag then
      Format.printf "@[<v>Let In:@;%a@;@]"
      pp_texpr tv ; *)
    let tctx' = TC.append_t var tv tctx in
    synthesize tctx' body
  )
  | Namespace_access (nexpr , name) -> (
    let nexpr' , nty = synthesize_namespace tctx nexpr in
    match nty with
    | TNNamespace ns_tctx -> (
      match TC.lookup name @@ TC.from_forward ns_tctx with
      | None -> failwith @@ F.asprintf "missing field %s when typechecking namespace access" name
      | Some tv -> Namespace_access (nexpr' , name) , tv
    )
    (* | _ -> failwith @@ F.asprintf "when typechecking namespace access, got a non namespace" *)
  )

and synthesize_builtin : tctx -> builtin -> expr * texpr = fun tctx b ->
  match b with
  | BAdd (e1 , e2) -> (
    let e1' , () = check tctx e1 (TBuiltin TInt) in
    let e2' , () = check tctx e2 (TBuiltin TInt) in
    Builtin (BAdd (e1' , e2')) , TBuiltin TInt
  )

and synthesize_namespace : tctx -> nexpr -> nexpr * tnexpr = fun tctx nexpr ->
  match nexpr with
  | NStatements statements -> (
    let tctx' , statements' = synthesize_statements tctx statements in
    let diff = TC.diff tctx' tctx in
    NStatements statements' , TNNamespace (TC.to_forward diff)
  )
  | NNamespace_access (nexpr , name) -> (
    let nexpr' , nty = synthesize_namespace tctx nexpr in
    match nty with
    | TNNamespace ntctx -> (
      match TC.lookup_n name @@ TC.from_forward ntctx with
      | None -> failwith @@ F.asprintf "Missing subnamespace %s in namespace" name
      | Some tnvalue -> NNamespace_access (nexpr' , name) , tnvalue
    )
  )
  | NVariable nvar -> (
    match TC.lookup_n nvar tctx with
    | None -> failwith @@ F.asprintf "Namespace %s not found" nvar
    | Some tnvalue -> NVariable nvar , tnvalue
  )
  | NMap { vars ; nvars } -> (
    let tctx' = ref tctx in
    let vars' =
      vars |> List.map @@ fun (var , expr) -> (
        let expr' , ty = synthesize !tctx' expr in
        tctx' := TC.append var ty !tctx' ;
        var , expr'
      )
    in
    let nvars' =
      nvars |> List.map @@ fun (nvar , nexpr) -> (
        let nexpr' , nty = synthesize_namespace !tctx' nexpr in
        tctx' := TC.append_n nvar nty !tctx' ;
        nvar , nexpr'
      )
    in
    let diff = TC.diff !tctx' tctx in
    NMap { vars = vars' ; nvars = nvars' } , TNNamespace (TC.to_forward diff)
  )

and synthesize_statement : tctx -> statement -> tctx * statement =
  fun tctx stm ->
  match stm with
  | SLet (var , expr) -> (
    let expr' , tv = synthesize tctx expr in
    let tctx' = TC.append var tv tctx in
    tctx' , SLet (var , expr')
  )
  | SLetNamespace (var , nexpr) -> (
    let nexpr' , tv = synthesize_namespace tctx nexpr in
    let tctx' = TC.append_n var tv tctx in
    tctx' , SLetNamespace (var , nexpr')
  )
  | SLetType (var , texpr) -> (
    let tv =
      match teval tctx texpr with
      | Partial x | Full x -> x
    in
    let tctx' = TC.append_t var tv tctx in
    tctx' , SLetType (var , texpr)
  )
  | SExpr expr -> (
    let expr' , _ = synthesize tctx expr in
    tctx , SExpr (expr')
  )

and synthesize_statements : tctx -> statement list -> tctx * statement list =
  fun tctx stms ->
  let tctx' , rev_statements = stms |> List.fold_left (
    fun (tctx , stms) stm -> (
    let (tctx' , stm') = synthesize_statement tctx stm in
    tctx' , stm' :: stms
  )) (tctx , []) in
  tctx' , List.rev rev_statements