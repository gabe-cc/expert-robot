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
| Fold of expr
| Unfold of expr
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
| VClosure of var * expr * ctx
| VFold of value

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
| TMu of string * texpr (* mu var -> body *)

and texpr =
| TEType
| TEArrow of texpr * texpr
| TELiteral of literal
| TEBuiltin of tbuiltin
| TERecord of (string * texpr) list
| TEVariant of (string * texpr) list
| TEMu of string * texpr (* mu var -> body *)
| TEVar of string

and var = string

and statement =
| SLet of var * expr
| SExpr of expr

and ctx = (string * value) list

(*
  (var context * type var context)
  var context used for type of regular variables
  type var context used for value of type variables
*)
and tctx = (string * tvalue) list * (string * tvalue) list

let tctx_append_var x tval: tctx -> tctx =
  fun (vars , tvars) -> (x , tval) :: vars , tvars
let tctx_lookup_var x : tctx -> tvalue option =
  fun (vars , _tvars) -> List.assoc_opt x vars
let tctx_append_tvar x tval : tctx -> tctx =
  fun (vars , tvars) -> vars , (x , tval) :: tvars
let tctx_lookup_tvar x : tctx -> tvalue option =
  fun (_vars , tvars) -> List.assoc_opt x tvars


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
  | Fold expr -> (
    let value = eval ctx expr in
    VFold value
  )
  | Unfold expr -> (
    let value = eval ctx expr in
    match value with
    | VFold value' -> value'
    | _ -> failwith "unfolding a non-fold"
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


let syntactic_teeq : texpr -> texpr -> bool = fun ty1 ty2 -> ty1 = ty2

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
  | TVariant _ , _ | _ , TVariant _ -> false
  | TMu (x1 , body1) , TMu (x2 , body2) -> (
    (* no alpha equivalence for recursive types. pure syntactic equality.
      might even have to be nominal tbf lol. *)
    x1 = x2 &&
    syntactic_teeq body1 body2
  )
  (* | TMu _ , _ | _ , TMu _ -> false *)
  
let rec teval : tctx -> texpr -> tvalue = fun tctx texp ->
  match texp with
  | TEType -> TType
  | TEArrow (input , output) -> (
    let input' = teval tctx input in
    let output' = teval tctx output in
    TArrow (input' , output')
  )
  | TELiteral lit -> TLiteral lit
  | TEBuiltin b -> TBuiltin b
  | TERecord lst -> (
    let lst' = lst |> List.map (fun (name , content) -> 
      name , teval tctx content    
    ) in
    TRecord lst'
  )
  | TEVariant lst -> (
    let lst' = lst |> List.map (fun (name , content) -> 
      name , teval tctx content    
    ) in
    TVariant lst'
  )
  | TEMu (var , body) -> TMu (var , body)
  | TEVar var -> (
    match tctx_lookup_tvar var tctx with
    | Some tv -> tv
    | None -> failwith "missing type variable"
  )


let rec check : tctx -> expr -> tvalue -> unit = fun tctx expr ty ->
  match expr , ty with
  | Builtin x , _ -> check_builtin tctx x ty
  | Variable var , _ -> (
    match tctx_lookup_var var tctx with
    | Some var_ty -> assert (subtype tctx var_ty ty)
    | None -> failwith @@ F.sprintf "when type checking, variable not found (%s)" var
  )
  | Function (var , texp , body) , TArrow (input , output) -> (
    let var_ty = teval tctx texp in
    assert (subtype tctx var_ty input) ;
    let tv = teval tctx texp in
    let tctx' = tctx_append_var var tv tctx in
    check tctx' body output
  )
  | Function _ , _ -> failwith "function expects tarrow"
  | LetIn (var , exp , body) , _ -> (
    let tv = synthesize tctx exp in
    let tctx' = tctx_append_var var tv tctx in
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
  | Match (matchee , branches) , _ -> (
    let matchee' = synthesize tctx matchee in
    match matchee' with
    | TVariant cases -> (
      if branches = [] then failwith "when type checking pattern matching, was empty" ;
      branches |> List.iter (fun (name , (var , body)) ->
        match List.assoc_opt name cases with
        | Some case -> (
          let tctx' = tctx_append_var var case tctx in
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
  | Fold expr , TMu (var , body) -> (
    let tctx' = tctx_append_tvar var ty tctx in
    let body' = teval tctx' body in
    check tctx' expr body'
  )
  | Fold _ , _ -> failwith "folding a non-mu type"
  | Call _ , _
  | Annotation _ , _
  | Literal _ , _
  | Record _ , _
  | Field _ , _
  | Unfold _ , _
  -> (
    let inferred_ty = synthesize tctx expr in
    assert (subtype tctx inferred_ty ty)
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
    match tctx_lookup_var var tctx with
    | Some tv -> tv
    | None -> failwith @@ F.sprintf "when type checking, variable not found (%s)" var
  )
  | Function (var , texp , body) -> (
    let tv = teval tctx texp in
    let tctx' = tctx_append_var var tv tctx in
    let body' = synthesize tctx' body in
    TArrow (tv , body')
  )
  | LetIn (var , exp , body) -> (
    let tv = synthesize tctx exp in
    let tctx' = tctx_append_var var tv tctx in
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
    let ty' = teval tctx ty in
    check tctx expr ty' ;
    ty'
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
            let tctx' = tctx_append_var var case tctx in
            synthesize tctx' body
          )
          | None -> failwith @@ F.sprintf "when type checking pattern matching, branch (%s) that was not part of the variant" name
        in
        let () =
          tl_branches |> List.iter @@ fun (name , (var , body)) ->
          match List.assoc_opt name cases with
          | Some case -> (
            let tctx' = tctx_append_var var case tctx in
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
  | Fold _ -> failwith "can not synthesize fold, need a type annotation"
  | Unfold body -> (
    let ty = synthesize tctx body in
    match ty with
    | TMu (var , body) -> (
      let tctx' = tctx_append_tvar var ty tctx in
      let body' = teval tctx' body in
      body'
    )
    | _ -> failwith "unfolding a non-mu type"
  )

and synthesize_builtin : tctx -> builtin -> tvalue = fun tctx b ->
  match b with
  | BAdd (e1 , e2) -> (
    check tctx e1 (TBuiltin TInt) ;
    check tctx e2 (TBuiltin TInt) ;
    TBuiltin TInt
  )


module Debug = struct
  let rec texpr_of_tvalue : tvalue -> texpr = fun tv ->
    let (!) = texpr_of_tvalue in
    match tv with
    | TType -> TEType
    | TArrow (a , b) -> TEArrow (!a , !b)
    | TLiteral lit -> TELiteral lit
    | TBuiltin b -> TEBuiltin b
    | TRecord lst -> TERecord (
      lst |> List.map (fun (x , y) -> x , !y)
    )
    | TVariant lst -> TEVariant (
      lst |> List.map (fun (x , y) -> x , !y)
    )
    | TMu (x , y) -> TEMu (x , y)
end