type expr =
| Builtin of builtin
| Variable of var
| Function of function_ 
| LetIn of var * expr * expr (* let x = expr in body *)
| Call of expr * expr
| Annotation of expr * texpr
| Literal of literal
| Record of (string * expr) list
| Field of expr * string
| Case of string * expr
| Match of expr * (string * (string * expr)) list
(* | Fold of expr * texpr *)
[@@deriving show { with_path = false }]

and function_ = var * texpr * expr (* fun (x : ty) -> body *)

and builtin =
| BAdd of expr * expr

and literal =
| LInt of int
| LString of string

and value =
| VLiteral of literal
| VRecord of (string * value) list
| VCase of string * value
| VClosure of var * expr * (var * value) list

and texpr = tvalue

and tbuiltin =
| TInt
| TString

and tvalue =
| TType
| TArrow of tvalue * tvalue
| TLiteral of literal
| TBuiltin of tbuiltin
| TRecord of (string * tvalue) list
| TVariant of (string * tvalue) list
(* | Mu of tvalue *)

and var = string

and statement =
| SLet of var * expr
| SExpr of expr

type ctx = (string * value) list

(*
  Used in diff places!
  1. (variable -> type value) when type checking
  2. (type variable -> type value) when evaluating type expressions
*)
type tctx = (string * tvalue) list

module F = Format

let rec eval : ctx -> expr -> value = fun ctx expr ->
  match expr with
  | Builtin x -> eval_builtin ctx x
  | Variable x -> (
    match List.assoc_opt x ctx with
    | Some y -> y
    | None -> failwith @@ F.sprintf "when evaluating, variable not found (%s)" x
  )
  | Function (var , _ty , body) -> VClosure (var , body , ctx)
  | LetIn (var , expr , body) -> (
    let value = eval ctx expr in
    let ctx' = (var , value) :: ctx in
    eval ctx' body
  )
  | Call (f , arg) -> (
    let f' = eval ctx f in
    let arg' = eval ctx arg in
    match f' with
    | VClosure (v , body , ctx') -> (
      let ctx'' = (v , arg') :: ctx' in
      eval ctx'' body
    )
    | _ -> failwith "when evaluating call, got a non functional value"
  )
  | Annotation (expr , _) -> eval ctx expr
  | Literal l -> VLiteral l
  | Record lst -> (
    let lst' =
      List.map
        (fun (name , content) -> name , eval ctx content)
        lst
    in
    VRecord lst'
  )
  | Field (expr , name) -> (
    let value = eval ctx expr in
    match value with
    | VRecord lst -> (
      match List.assoc_opt name lst with
      | Some value' -> value'
      | None -> failwith @@ F.sprintf "when evaluating field, did not find field %s" name
    )
    | _ -> failwith "when evaluating field, got a non record"
  )
  | Case (name , expr) -> (
    let value = eval ctx expr in
    VCase (name , value)
  )
  | Match (matchee , branches) -> (
    let matchee' = eval ctx matchee in
    match matchee' with
    | VCase (name , content) -> (
      match List.assoc_opt name branches with
      | Some (var , body) -> (
        let ctx' = (var , content) :: ctx in
        eval ctx' body
      )
      | None -> failwith @@ F.sprintf "when evaluating match, branch (%s) was missing" name
    )
    | _ -> failwith "when evaluating match, got a non-case"
  )


and eval_builtin : ctx -> builtin -> 'a = fun ctx b ->
  match b with
  | BAdd (e1 , e2) -> (
    let value1 = eval ctx e1 in
    let value2 = eval ctx e2 in
    match value1 , value2 with
    | VLiteral (LInt x) , VLiteral (LInt y) -> VLiteral (LInt (x + y))
    | _ , VLiteral (LInt _) -> failwith "when evaluating add, left arg was not an int"
    | VLiteral (LInt _) , _ -> failwith "when evaluating add, right arg was not an int"
    | _ , _ -> failwith "when evaluating add, no arg was an int"
  )

let teval : tctx -> texpr -> tvalue = fun tctx texp ->
  ignore tctx ;
  texp

(* For now, this is mostly equality, aside from literals *)
let rec subtype : tctx -> tvalue -> tvalue -> bool = fun tctx ty1 ty2 ->
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
  (* | TVariant _ , _ | _ , TVariant _ -> false *)
  

let rec check : tctx -> expr -> tvalue -> unit = fun tctx expr ty ->
  match expr , ty with
  | Builtin x , _ -> check_builtin tctx x ty
  | Variable var , _ -> (
    match List.assoc_opt var tctx with
    | Some var_ty -> assert (subtype tctx var_ty ty)
    | None -> failwith @@ F.sprintf "when type checking, variable not found (%s)" var
  )
  | Function (var , texp , body) , TArrow (input , output) -> (
    assert (subtype tctx texp input) ;
    let tv = teval tctx texp in
    let tctx' = (var , tv) :: tctx in
    check tctx' body output
  )
  | Function _ , _ -> failwith "function expects tarrow"
  | LetIn (var , exp , body) , _ -> (
    let tv = synthesize tctx exp in
    let tctx' = (var , tv) :: tctx in
    check tctx' body ty
  )
  | Literal (LInt n) , TLiteral (LInt n') -> (
    assert (n = n')
  )
  | Literal (LString s) , TLiteral (LString s') -> (
    assert (s = s')
  )
  | Case (name , content) , _ -> (
    match ty with
    | TVariant lst -> (
      match List.assoc_opt name lst with
      | Some ty -> (
        check tctx content ty
      )
      | None -> failwith @@ F.sprintf "when type checking case, case (%s) did not match variant type" name
    )
    | _ -> failwith "when type checking case, got a non-variant type annotation"  
  )
  | Call _ , _
  | Annotation _ , _
  | Literal _ , _
  | Record _ , _
  | Field _ , _
  -> (
    let inferred_ty = synthesize tctx expr in
    assert (subtype tctx inferred_ty ty)
  )
  | Match (matchee , branches) , _ -> (
    let matchee' = synthesize tctx matchee in
    match matchee' with
    | TVariant cases -> (
      if branches = [] then failwith "when type checking pattern matching, was empty" ;
      branches |> List.iter (fun (name , (var , body)) ->
        match List.assoc_opt name cases with
        | Some case -> (
          let tctx' = (var , case) :: tctx in
          check tctx' body ty
        )
        | None -> failwith @@ F.sprintf "when type checking pattern matching, branch (%s) that was not part of the variant" name
      ) ;
      cases |> List.iter (fun (name , _) ->
        if List.assoc_opt name branches = None then
          failwith @@ F.sprintf "missing branch (%s) in match case" name
      )
    )
    | _ -> failwith "when type checking pattern matching, got a non-variant as matchee"
  )

and check_builtin : tctx -> builtin -> tvalue -> unit = fun tctx b ty ->
  match b , ty with
  | BAdd (e1 , e2) , TBuiltin TInt -> (
    check tctx e1 (TBuiltin TInt) ;
    check tctx e2 (TBuiltin TInt)
  )
  | BAdd _ , _ -> failwith "got bad type on add"

and synthesize : tctx -> expr -> tvalue = fun tctx expr ->
  match expr with
  | Builtin x -> synthesize_builtin tctx x
  | Variable var -> (
    match List.assoc_opt var tctx with
    | Some tv -> tv
    | None -> failwith @@ F.sprintf "when type checking, variable not found (%s)" var
  )
  | Function (var , texp , body) -> (
    let tv = teval tctx texp in
    let tctx' = (var , tv) :: tctx in
    let body' = synthesize tctx' body in
    TArrow (tv , body')
  )
  | LetIn (var , exp , body) -> (
    let tv = synthesize tctx exp in
    let tctx' = (var , tv) :: tctx in
    synthesize tctx' body
  )
  | Call (f , arg) -> (
    let f' = synthesize tctx f in
    match f' with
    | TArrow (input , output) -> (
      check tctx arg input ;
      output
    )
    | _ -> failwith @@ F.sprintf "when type checking application, left part did not have function type"
  )
  | Annotation (expr , ty) -> (
    check tctx expr ty ;
    ty
  )
  | Literal lit -> (
    match lit with
    | LInt _ -> TBuiltin (TInt)
    | LString _ -> TBuiltin (TString)
  )
  | Record lst -> (
    let lst' =
      List.map
        (fun (name , content) -> name , synthesize tctx content)
        lst
    in
    TRecord lst'
  )
  | Field (record , name) -> (
    let record' = synthesize tctx record in
    match record' with
    | TRecord lst -> (
      match List.assoc_opt name lst with
      | Some ty -> ty
      | None -> failwith @@ F.sprintf "when type checking field, field (%s) was missing" name
    )
    | _ -> failwith "when type checking field, didn't get a record"
  )
  | Case _ -> failwith "can not synthesize case, need a type annotation"
  | Match (matchee , branches) -> (
    let matchee' = synthesize tctx matchee in
    match matchee' with
    | TVariant cases -> (
      match branches with
      | [] -> failwith "when type checking pattern matching, was empty"
      | hd_branch :: tl_branches -> (
        let ty =
          let (name , (var , body)) = hd_branch in
          match List.assoc_opt name cases with
          | Some case -> (
            let tctx' = (var , case) :: tctx in
            synthesize tctx' body
          )
          | None -> failwith @@ F.sprintf "when type checking pattern matching, branch (%s) that was not part of the variant" name
        in
        let () =
          tl_branches |> List.iter @@ fun (name , (var , body)) ->
          match List.assoc_opt name cases with
          | Some case -> (
            let tctx' = (var , case) :: tctx in
            check tctx' body ty
          )
          | None -> failwith @@ F.sprintf "when type checking pattern matching, branch (%s) that was not part of the variant" name
        in
        let () =
          cases |> List.iter (fun (name , _) ->
            if List.assoc_opt name branches = None then
              failwith @@ F.sprintf "missing branch (%s) in match case" name
          )
        in
        ty
      )
    )
    | _ -> failwith "when type checking pattern matching, got a non-variant as matchee"
  )

and synthesize_builtin : tctx -> builtin -> tvalue = fun tctx b ->
  match b with
  | BAdd (e1 , e2) -> (
    check tctx e1 (TBuiltin TInt) ;
    check tctx e2 (TBuiltin TInt) ;
    TBuiltin TInt
  )