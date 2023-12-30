open Agaml_ast.Types
module C = Agaml_ast.Context
module U = Agaml_ast.Utils

(*
  Evaluation Results can be partial or full. An evaluation result is said to be...
  - `full` when the whole expression has been evaluated to a value.
  - `partial` when at least one part has not been evaluated.
  
  For now, the only reason for an expression to only be partially evaluated is when it features a variable that has no value in the evaluation context.
  
  Example:
  - Evaluating in the body of a function. At static time, you can evaluate sub-expressions contained in the body of a function. If these expressions refer to any this function parameters, then you will get a partial result, as those parameters are not bound to any value.

  When an expression is not fully evaluated, it is hard to know if its evaluation will lead to an error or not yet. For instance:
  - `x + 4` is valid, but you need to see that `x` is a variable.
  - `((fun y -> ...) x) + 4` is valid too, but you need to see that `x` is partial.
  - `{foo = (* .. some partial expression *)} + 4` is not valid, but you need to check that it is a record (even though it is partial) to see that there is no way to recover.

  For now, we are being overly approximative, and do not fail in the case one of the members is partial. As such `{ foo = (* partial *) } + 4` is deemed valid.

  TODO: Exclude impossible cases, like `{foo = (* partial *)} + 4`
*)
type 'a eval_result =
| Full of 'a  (* should be a value *)
| Partial of 'a (* for strong norm purposes *)
let to_pair = function
| Full x -> true , x
| Partial x -> false , x
let of_pair b x =
  if b then Full x else Partial x
let (let*) x f =
  match x with
  | Full x -> Full (f x)
  | Partial x -> Partial (f x)
let (let*?) x f =
  match x with
  | Full x -> Full (f (true , x))
  | Partial x -> Partial (f (false , x))
let (let+) x f =
  match x with
  | Full x -> f x
  | Partial x -> (
    match f x with
    | Full x | Partial x -> Partial x
  )
let full x = Full x
let partial x = Partial x

module Flag = Agaml_tools.Flag
module Eval_log_steps = Flag()

(* subject reduction should hold *)
let rec eval : ctx -> expr -> expr eval_result = fun ctx expr ->
  if !Eval_log_steps.flag then (
    Format.printf "@[<v>Eval step:@;%a@]\n%!" pp_expr expr 
  ) ;
  match expr with
  | Builtin x -> eval_builtin ctx x
  | Variable x -> (
    match C.lookup x ctx with
    | Some (Value (Rec (_ , body))) -> eval ctx body
    | Some (Value y) -> full y
    | Some (Expr y) -> partial y
    | Some Hole -> partial expr
    | None -> failwith @@ F.sprintf "when evaluating, variable not found (%s)" x
  )
  | Function (var , _ty , body) -> full @@ Closure (var , body , ctx)
  | LetIn (var , expr , body) -> (
    let+ value = eval ctx expr in
    let ctx' = C.append var value ctx in
    eval ctx' body
  )
  | Call (f , arg) -> (
    let+ f' = eval ctx f in
    let+ arg' = eval ctx arg in
    match f' with
    | Closure (v , body , ctx') -> (
      let ctx'' = C.append v arg' ctx' in
      eval ctx'' body
    )
    (*
      TODO: should not be a partial??
      or at least on variables, but a bit too ad-hoc
    *)
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
    let*? is_full , value = eval ctx expr in
    match value with
    | Record lst -> (
      match List.assoc_opt name lst with
      | Some value' -> value'
      | None -> failwith @@ F.sprintf "when evaluating field, did not find field %s" name
    )
    | _ when is_full -> failwith "when evaluating field, got a non record"
    | _ -> value
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
        let ctx' = C.append var content ctx in
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
    let ctx' = C.append var (Rec (var , body)) ctx in
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
  | LetInT (_var , _texpr , body) -> eval ctx body
  | Namespace_access (nexpr , name) -> (
    let* value = neval ctx nexpr in
    match value with
    | NMap { vars ; nvars = _ } -> (
      match List.assoc_opt name vars with
      | Some value -> value
      | None -> failwith @@ F.asprintf "missing field %s in namespace" name
    )
    | _ -> failwith @@ F.asprintf "when evaluating namespace access, got a non-namespace value"
  )

and neval : ctx -> nexpr -> nexpr eval_result = fun ctx nexpr ->
  match nexpr with
  | NStatements statements -> (
    let ctx' = ref ctx in
    let full = ref true in
    let vars = ref [] in
    let nvars = ref [] in
    let statements' =
      statements |> List.filter_map @@ fun statement ->
      match statement with
      | SLet (var , expr) -> (
        match eval !ctx' expr with
        | Full value -> (
          ctx' := C.append var value !ctx' ;
          vars := (var , value) :: !vars ;
          Some (SLet (var , value))
        )
        | Partial expr' -> (
          full := false ;
          ctx' := C.append_hole var !ctx' ;
          Some (SLet (var , expr'))
        )
      )
      | SLetNamespace (var , nexpr) -> (
        match neval !ctx' nexpr with
        | Full value -> (
          ctx' := C.append_n var value !ctx' ;
          nvars := (var , value) :: !nvars ;
          Some (SLetNamespace (var , value))
        )
        | Partial nexpr' -> (
          full := false ;
          ctx' := C.append_hole var !ctx' ;
          Some (SLetNamespace (var , nexpr'))
        )
      )
      | SExpr expr -> (
        match eval !ctx' expr with
        | Full value -> Some (SExpr value)
        | Partial expr' -> (
          full := false ;
          Some (SExpr expr')
        )
      )
      | SLetType _ -> None
    in
    if !full
    then Full (NMap {vars = !vars ; nvars = !nvars})
    else Partial (NStatements statements')
  )
  | NVariable nvar -> (
    match C.lookup_n nvar ctx with
    | Some (Value nval) -> full nval
    | Some (Expr nexpr) -> partial nexpr
    | Some Hole -> failwith "should never be a namespace hole in context: no namespace function"
    | None -> failwith @@ F.asprintf "didn't find namespace %s in context" nvar
  )
  | NMap { vars ; nvars } -> (
    let full = ref true in
    let ctx' = ref ctx in
    let vars' =
      vars |> List.map @@ fun (var , expr) ->
      let is_full , expr' = to_pair @@ eval !ctx' expr in
      if is_full then (
        ctx' := C.append var expr' !ctx' ;
      ) else (
        ctx' := C.append_hole var !ctx' ;
        full := false ;
      ) ;
      var , expr'
    in
    let nvars' =
      nvars |> List.map @@ fun (var , nexpr) ->
      let is_full , nexpr' = to_pair @@ neval !ctx' nexpr in
      if is_full then (
        ctx' := C.append_n var nexpr' !ctx' ;
      ) else (
        ctx' := C.append_nexpr var nexpr' !ctx' ;
        full := false ;
      ) ;
      var , nexpr'
    in
    of_pair !full @@ NMap { vars = vars' ; nvars = nvars' }
  )
  | NNamespace_access (nexpr , name) -> (
    let* nvalue = neval ctx nexpr in
    match nvalue with
    | NMap { vars = _ ; nvars } -> (
      match List.assoc_opt name nvars with
      | Some nvalue' -> nvalue'
      | None -> failwith @@ F.asprintf "didn't find subnamespace %s in namespace" name
    )
    | _ -> failwith "expected nmap on nnamespace access"
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
