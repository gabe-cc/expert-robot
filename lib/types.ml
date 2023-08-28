type expr =
| Builtin of builtin
| Variable of var
| Function of var * texpr * expr (* fun (x : ty) -> body *)
| Call of expr * expr
| LetIn of var * expr * expr (* let x = expr in body *)
| Annotation of expr * texpr
| Literal of literal
| Record of (string * expr) list
| Field of expr * string
| Case of string * expr
| Match of expr * (string * (string * expr)) list
| Fold of expr
| Unfold of expr
| Rec of string * expr (* rec self -> body *)
| Closure of var * expr * ctx
| Eval of bool * expr (* static_eval (full?) expr *)
| FunctionT of var * expr (* T : Type -> body *)
| CallT of expr * texpr (* expr ty *)
[@@deriving show { with_path = false }]

and builtin =
| BAdd of expr * expr

and literal =
| LInt of int
| LString of string

and tbuiltin =
| TInt
| TString

and texpr =
| TType
| TArrow of texpr * texpr
| TLiteral of literal
| TBuiltin of tbuiltin
| TRecord of (string * texpr) list
| TVariant of (string * texpr) list
| TMu of string * texpr (* mu var -> body *)
| TVar of string
| TFunction of string * texpr (* TFUN var -> body *)

and var = string

and statement =
| SLet of var * expr
| SExpr of expr

and ctx = (string * ctx_member) list
and ctx_member = {
  value : expr option ;
  tvalue : texpr option ;
}

(*
  (var context * type var context)
  var context used for type of regular variables
  type var context used for value of type variables
*)
and tctx = ctx * (string * texpr option) list (* `texpr` should be tvalue *)

let ctx_append x value : ctx -> ctx =
  fun ctx -> (x , { value = Some value ; tvalue = None }) :: ctx
let ctx_append_type x tvalue : ctx -> ctx =
  fun ctx -> (x , { value = None ; tvalue = Some tvalue }) :: ctx
let ctx_append_full x value tvalue : ctx -> ctx =
  fun ctx -> (x , { value = Some value ; tvalue = Some tvalue }) :: ctx
let tctx_append_var x tval : tctx -> tctx =
  fun (vars , tvars) -> ctx_append_type x tval vars , tvars
let tctx_lookup_var x : tctx -> texpr option =
  fun (vars , _tvars) -> (
  Option.bind (List.assoc_opt x vars) @@ fun x ->
  x.tvalue
)
let tctx_append_tvar x tval : tctx -> tctx =
  fun (vars , tvars) -> vars , (x , Some tval) :: tvars
let tctx_append_tvar_hole x : tctx -> tctx =
  fun (vars , tvars) -> vars , (x , None) :: tvars
let tctx_lookup_tvar x : tctx -> texpr option option =
  fun (_vars , tvars) -> List.assoc_opt x tvars
let tctx_closure (new_vars : ctx) : tctx -> tctx =
  fun (_old_vars , _tvars) -> new_vars , []
let tctx_to_ctx : tctx -> ctx = fst

module F = Format

(*
  Only reason for something to be partial is if reducing if a variable is missing its value.
  
  Examples:
  - Typechecking and not inlining
  - Reducing under a lambda

  When something is not fully evaluated, it is hard to know if it is valid or not yet. For instance:
  - `x + 4` is valid, but you need to see that `x` is a variable.
  - `((fun y -> ...) x) + 4` is valid too, but you need to see that `x` is partial.
  - `{foo = (* .. some partial expression *)} + 4` is not valid, but you need to check that it is a record (even though it is partial) to see that there is no way to recover.

  For now, we are being overly approximative, and do not fail in the case one of the members is partial. As such `{ foo = (* partial *) }` can be added to, or applied to.

  TODO: Exclude impossible cases, like `{foo = (* partial *)} + 4`
*)
type eval_result =
| Full of expr (* should be a value *)
| Partial of expr (* for strong norm purposes *)
let (let*) x f =
  match x with
  | Full x -> Full (f x)
  | Partial x -> Partial (f x)
let (let+) x f =
  match x with
  | Full x -> f x
  | Partial x -> (
    match f x with
    | Full x | Partial x -> Partial x
  )
let full x = Full x
let partial x = Partial x

module Flag() = struct
  let flag = ref false
  let with_flag f =
    flag := true ;
    f () ;
    flag := false
  let counter = ref 0
  let counter_inc () = counter := !counter + 1 ; !counter - 1
end

module Eval_log_steps = Flag()
module Synthesize_log_steps = Flag()

(* subject reduction should hold *)
let rec eval : ctx -> expr -> eval_result = fun ctx expr ->
  if !Eval_log_steps.flag then (
    Format.printf "@[<v>Eval step:@;%a@]\n%!" pp_expr expr 
  ) ;
  match expr with
  | Builtin x -> eval_builtin ctx x
  | Variable x -> (
    match List.assoc_opt x ctx with
    | Some { value = Some (Rec (_ , body)) ; tvalue = _ } -> eval ctx body
    | Some { value = Some y ; tvalue = _ } -> full y
    | Some { value = None ; tvalue = _ } -> partial expr
    | None -> failwith @@ F.sprintf "when evaluating, variable not found (%s)" x
  )
  | Function (var , _ty , body) -> full @@ Closure (var , body , ctx)
  | LetIn (var , expr , body) -> (
    let+ value = eval ctx expr in
    let ctx' = ctx_append var value ctx in
    eval ctx' body
  )
  | Call (f , arg) -> (
    let+ f' = eval ctx f in
    let+ arg' = eval ctx arg in
    match f' with
    | Closure (v , body , ctx') -> (
      let ctx'' = ctx_append v arg' ctx' in
      eval ctx'' body
    )
    | _ -> failwith "when evaluating call, got a non functional value"
  )
  | Annotation (expr , _) -> eval ctx expr
  | Literal l -> full @@ Literal l
  | Record lst -> (
    let is_full = ref true in
    let lst' =
      lst |> List.map (fun (name , content) -> name ,
      match eval ctx content with
      | Partial x -> (is_full := false ; x)
      | Full x -> x
      )
    in
    (if !is_full then full else partial) @@
    Record lst'
  )
  | Field (expr , name) -> (
    let* value = eval ctx expr in
    match value with
    | Record lst -> (
      match List.assoc_opt name lst with
      | Some value' -> value'
      | None -> failwith @@ F.sprintf "when evaluating field, did not find field %s" name
    )
    | _ -> failwith "when evaluating field, got a non record"
  )
  | Case (name , expr) -> (
    let* value = eval ctx expr in
    Case (name , value)
  )
  | Match (matchee , branches) -> (
    let+ matchee' = eval ctx matchee in
    match matchee' with
    | Case (name , content) -> (
      match List.assoc_opt name branches with
      | Some (var , body) -> (
        let ctx' = ctx_append var content ctx in
        eval ctx' body
      )
      | None -> failwith @@ F.sprintf "when evaluating match, branch (%s) was missing" name
    )
    | _ -> failwith "when evaluating match, got a non-case"
  )
  | Fold expr -> (
    let* value = eval ctx expr in
    Fold value
  )
  | Unfold expr -> (
    let* value = eval ctx expr in
    match value with
    | Fold value' -> value'
    | _ -> failwith "unfolding a non-fold"
  )
  | Rec (var , body) -> (
    let ctx' = ctx_append var (Rec (var , body)) ctx in
    eval ctx' body
  )
  | Closure _ -> full expr
  | Eval (_full , expr) -> eval ctx expr
  | FunctionT (tvar , body) -> full @@ FunctionT (tvar , body)
  | CallT (expr , texpr) -> (
    match eval ctx expr with
    | Full (FunctionT (_ , body)) -> eval ctx body
    | Full _ -> failwith "callt on non functiont"
    | Partial expr' -> partial @@ CallT (expr' , texpr)
  )

and eval_builtin : ctx -> builtin -> 'a = fun ctx b ->
  match b with
  | BAdd (e1 , e2) -> (
    (* TODO: Clean this up lol. *)
    match eval ctx e1 , eval ctx e2 with
    | Full (Literal (LInt x)) , Full (Literal (LInt y))
      -> full @@ Literal (LInt (x + y))
    | Partial (_ as x) , Full (Literal (LInt _) as y)
    | Full (Literal (LInt _) as x) , Partial (_ as y)
    | Partial (_ as x) , Partial (_ as y)
      -> Partial (Builtin (BAdd (x , y)))
    | Partial (Literal (LInt _)) , _
    | _ , Partial (Literal (LInt _))
      -> failwith "dev: should not have partially evaluated literal"
    | Full _ , Full (Literal (LInt _))
    | Full _ , Partial _
      -> failwith "when evaluating add, left arg was not an int"
    | Full (Literal (LInt _)) , Full _
    | Partial _ , Full _
      -> failwith "when evaluating add, right arg was not an int"
    | Full _ , Full _ -> failwith "when evaluating add, no arg was an int"
  )


let syntactic_teeq : texpr -> texpr -> bool = fun ty1 ty2 -> ty1 = ty2

(* For now, this is mostly equality, aside from literals *)
(* This should be only on values. *)
let rec subtype : tctx -> texpr -> texpr -> bool = fun tctx ty1 ty2 ->
  match ty1 , ty2 with
  | TType , TType -> true
  | TType , _ | _ , TType -> false
  | TArrow (a,b) , TArrow (a' , b') -> (
    subtype tctx a' a &&
    subtype tctx b b'
  )
  | TArrow _ , _ | _ , TArrow _ -> false
  | TLiteral lit1 , TLiteral lit2 -> (
    lit1 = lit2
  )
  | TLiteral (LInt _) , TBuiltin TInt -> true
  | TLiteral (LString _) , TBuiltin TString -> true
  | TLiteral _ , _ | _ , TLiteral _ -> false
  | TBuiltin b1 , TBuiltin b2 -> b1 = b2
  | _ , TBuiltin _ | TBuiltin _ , _ -> false
  | TRecord lst1 , TRecord lst2 -> (
    List.length lst1 = List.length lst2 &&
    List.fold_left (fun acc (name , content1) -> acc && (
      match List.assoc_opt name lst2 with
      | Some content2 -> subtype tctx content1 content2
      | None -> false
    )) true lst1
  )
  | TRecord _ , _ | _ , TRecord _ -> false
  | TVariant lst1 , TVariant lst2 -> (
    List.length lst1 = List.length lst2 &&
    List.fold_left (fun acc (name , content1) -> acc && (
      match List.assoc_opt name lst2 with
      | Some content2 -> subtype tctx content1 content2
      | None -> false
    )) true lst1
  )
  | TVariant _ , _ | _ , TVariant _ -> false
  | TMu (x1 , body1) , TMu (x2 , body2) -> (
    (* no alpha equivalence for recursive types. pure syntactic equality.
      might even have to be nominal tbf lol. *)
    x1 = x2 &&
    syntactic_teeq body1 body2
  )
  | TMu _ , _ | _ , TMu _ -> false
  | TVar x , TVar x' when x = x' -> true
  | TVar _ , _
  | _ , TVar _
    -> failwith "should not try to subtype variables"
  | TFunction (x1 , body1) , TFunction (x2 , body2) -> (
    (* no alpha equivalence for higher order types. pure syntactic equality. *)
    x1 = x2 &&
    syntactic_teeq body1 body2
  )
  (* | TFunction _ , _ | _ , TFunction _ -> false *)
  
let rec teval : tctx -> texpr -> texpr = fun tctx texp ->
  match texp with
  | TType -> TType
  | TArrow (input , output) -> (
    let input' = teval tctx input in
    let output' = teval tctx output in
    TArrow (input' , output')
  )
  | TLiteral lit -> TLiteral lit
  | TBuiltin b -> TBuiltin b
  | TRecord lst -> (
    let lst' = lst |> List.map (fun (name , content) -> 
      name , teval tctx content    
    ) in
    TRecord lst'
  )
  | TVariant lst -> (
    let lst' = lst |> List.map (fun (name , content) -> 
      name , teval tctx content    
    ) in
    TVariant lst'
  )
  | TMu (var , body) -> TMu (var , body)
  | TVar var -> (
    match tctx_lookup_tvar var tctx with
    | Some (Some tv) -> tv
    | Some None -> TVar var
    | None -> failwith @@ F.asprintf "missing type variable (%s)" var
  )
  | TFunction (var , body) -> (
    (*
      Should reduce under the lambdas. But this implies termination of strong normalization for type level calculus. hmmmm
      TODO: check if should reduce under the lambda or not
    *)
    let tctx' = tctx_append_tvar_hole var tctx in
    let body' = teval tctx' body in
    TFunction (var , body')
  ) 
  (* | TEFunction (var , body) -> TFunction (var , body) *)
  (* | TECall (f , arg) -> (
    let f' = teval tctx f in
    let arg' = teval tctx arg in
    match f' with
    | TFunction (var , body) -> (
      let tctx' = tctx_append_tvar var arg' tctx in
      teval tctx' body
    )
    | _ -> failwith "type calling a non-typefunction"
  ) *)


let rec check : tctx -> expr -> texpr -> expr * unit = fun tctx expr ty ->
  match expr , ty with
  | Builtin x , _ -> check_builtin tctx x ty
  | Variable var , _ -> (
    match tctx_lookup_var var tctx with
    | Some var_ty -> (
      Variable var , assert (subtype tctx var_ty ty)
    )
    | None -> failwith @@ F.sprintf "when type checking, variable not found (%s)" var
  )
  | Function (var , texp , body) , TArrow (input , output) -> (
    let var_ty = teval tctx texp in
    assert (subtype tctx var_ty input) ;
    let tv = teval tctx texp in
    let tctx' = tctx_append_var var tv tctx in
    let body' , () = check tctx' body output in
    Function (var , texp , body') , () (* TODO: texp -> tv *)
  )
  | Function _ , _ -> failwith "function expects tarrow"
  | LetIn (var , exp , body) , _ -> (
    let exp' , tv = synthesize tctx exp in
    let tctx' = tctx_append_var var tv tctx in
    let body' , () = check tctx' body ty in
    LetIn (var , exp' , body') , ()
  )
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
          let tctx' = tctx_append_var var case tctx in
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
    let tctx' = tctx_append_tvar var ty tctx in
    let body_ty = teval tctx' body in
    let expr' , () = check tctx' expr body_ty in
    Fold expr' , ()
  )
  | Fold _ , _ -> failwith "folding a non-mu type"
  | Rec (var , body) , _ -> (
    let tctx' = tctx_append_var var ty tctx in
    let body' , () = check tctx' body ty in
    Rec (var , body') , ()
  )
  (* | Builtin _ , _ *)
  | Call _ , _
  | Annotation _ , _
  | Literal _ , _
  | Record _ , _
  | Field _ , _
  | Unfold _ , _
  | FunctionT _ , _
  | CallT _ , _
  -> (
    let expr' , inferred_ty = synthesize tctx expr in
    if not (subtype tctx inferred_ty ty) then (
      failwith @@ Format.asprintf "@[<v>When checking expression:@;%a@;Inferred type was:@;%a@;But expected type was:@;%a@;@]"
        pp_expr expr pp_texpr inferred_ty pp_texpr ty
    ) ;
    expr' , ()
  )
  | Closure (var , body , ctx) , TArrow (input , output) -> (
    let ctx' =
      ctx |> List.fold_left (fun acc (var , ctxm) ->
        let expr =
          match ctxm with
          | { value = Some x ; tvalue = _ } -> x
          | { value = None ; tvalue = _ } -> failwith "can't synthesize hole in closure context"
        in
        let ty =
          match ctxm.tvalue with
          | Some ty -> ty
          | None -> (
            let tctx' = tctx_closure acc tctx in
            let _ , ty = synthesize tctx' expr in
            ty
          )
        in
        ctx_append_full var expr ty acc
      ) [] |> List.rev
    in
    let tctx' = tctx_closure ctx' tctx in
    let tctx'' = tctx_append_var var input tctx' in
    check tctx'' body output
  )
  | Closure _ , _ -> failwith "closure need a function type"
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
    match eval (tctx_to_ctx tctx) expr' with *)
    match eval (tctx_to_ctx tctx) expr with
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


and check_builtin : tctx -> builtin -> texpr -> expr * unit = fun tctx b ty ->
  match b , ty with
  | BAdd (e1 , e2) , TBuiltin TInt -> (
    let e1' , () = check tctx e1 (TBuiltin TInt) in
    let e2' , () = check tctx e2 (TBuiltin TInt) in
    Builtin (BAdd (e1' , e2')) , ()
  )
  | BAdd _ , _ -> failwith "got bad type on add"

(* should synthesize a tvalue *)
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
    match tctx_lookup_var var tctx with
    | Some tv -> expr , tv
    | None -> failwith @@ F.sprintf "when type checking, variable not found (%s)" var
  )
  | Function (var , texp , body) -> (
    let tv = teval tctx texp in
    let tctx' = tctx_append_var var tv tctx in
    let body' , body_ty = synthesize tctx' body in
    Function (var , texp , body') , TArrow (tv , body_ty) (* TODO: texp -> tv *)
  )
  | LetIn (var , exp , body) -> (
    let exp' , tv = synthesize tctx exp in
    let tctx' = tctx_append_var var tv tctx in
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
    let ty' = teval tctx ty in
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
            let tctx' = tctx_append_var var case tctx in
            let body' , body_ty = synthesize tctx' body in
            (name , (var , body')) , body_ty
          )
          | None -> failwith @@ F.sprintf "when type checking pattern matching, branch (%s) that was not part of the variant" name
        in
        let tl_branches' =
          tl_branches |> List.map @@ fun (name , (var , body)) ->
          match List.assoc_opt name cases with
          | Some case -> (
            let tctx' = tctx_append_var var case tctx in
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
      let tctx' = tctx_append_tvar var body_ty tctx in
      let tbody' = teval tctx' tbody in
      Unfold body' , tbody'
    )
    | _ -> failwith "unfolding a non-mu type"
  )
  | Rec (_ , _) -> failwith "can not synthesize rec, need a type annotation"
  | Closure _ -> failwith "can not synthesize closure, need a type annotation"
  | Eval (full , expr) -> (
    let expr' , ty = synthesize tctx expr in
    match eval (tctx_to_ctx tctx) expr' with
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
    let tctx' = tctx_append_tvar_hole var tctx in
    let body' , body_ty = synthesize tctx' body in
    FunctionT (var , body') , TFunction (var , body_ty)
  )
  | CallT (f , arg) -> (
    let f' , f_ty = synthesize tctx f in
    let arg' = teval tctx arg in
    match f_ty with
    | TFunction (var , body) -> (
      let tctx' = tctx_append_tvar var arg' tctx in
      let body' = teval tctx' body in
      CallT (f' , arg) , body'
    )
    | _ -> failwith "callt on non-tfunction type"
  )

and synthesize_builtin : tctx -> builtin -> expr * texpr = fun tctx b ->
  match b with
  | BAdd (e1 , e2) -> (
    let e1' , () = check tctx e1 (TBuiltin TInt) in
    let e2' , () = check tctx e2 (TBuiltin TInt) in
    Builtin (BAdd (e1' , e2')) , TBuiltin TInt
  )
