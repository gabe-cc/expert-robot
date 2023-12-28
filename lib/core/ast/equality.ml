open Types
module DAssoc = Agaml_tools.DAssoc

let syntactic_teeq : texpr -> texpr -> bool = fun ty1 ty2 -> ty1 = ty2

(*
  `subtype ty1 ty2` means `ty1 is a subtype of ty2`.
  For now, this is mostly equality, aside from literals (TLiteral 42 is a subtype of TInt).
  This should feature no reduction whatsoever (whereforehence the lack of type context).
  A mapping is maintained to map from vars from ty2 to ty1, to compute this equality moduly alpha-equivalence. Go to `DAssoc` to see more.
*)
let rec subtype (vm : string DAssoc.t): texpr -> texpr -> bool = fun ty1 ty2 ->
  match ty1 , ty2 with
  | TType , TType -> true
  | TType , _ | _ , TType -> false
  | TArrow (a,b) , TArrow (a' , b') -> (
    subtype (DAssoc.swap vm) a' a &&
    subtype vm b b'
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
      | Some content2 -> subtype vm content1 content2
      | None -> false
    )) true lst1
  )
  | TRecord _ , _ | _ , TRecord _ -> false
  | TVariant lst1 , TVariant lst2 -> (
    List.length lst1 = List.length lst2 &&
    List.fold_left (fun acc (name , content1) -> acc && (
      match List.assoc_opt name lst2 with
      | Some content2 -> subtype vm content1 content2
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
  | TVar x , TVar x' -> (
    match DAssoc.assoc_opt x' vm with
    | None -> x = x'
    | Some x' -> x = x'
  )
  | TVar _ , _
  | _ , TVar _
    -> failwith "should not try to subtype variables"
  | TFunction (x1 , body1) , TFunction (x2 , body2) -> (
    (* no alpha equivalence for higher order types. pure syntactic equality. *)
    let vm2 = DAssoc.append x2 x1 vm in
    subtype vm2 body1 body2
  )
  | TFunction _ , _ | _ , TFunction _ -> false
  | TCall _ , _
  | _ , TCall _
    -> failwith "should not try to subtype type applications"
  (* | TNamespace (vars , tvars) , TNamespace (vars' , tvars') -> (
    List.(length vars = length vars') && (
      vars |> List.fold_left (fun acc (var , tvalue) -> (&&) acc @@
      match List.assoc_opt var vars' with
      | Some tvalue' -> subtype tvalue tvalue'
      | None -> false
      ) true
    ) && List.(length tvars = length tvars') && (
      tvars |> List.fold_left (fun acc (tvar , tvalue) -> (&&) acc @@
      match tvalue , List.assoc_opt tvar tvars' with
      | _ , None -> false
      | None , Some None -> true (* both types are abstract in their tnamespace *)
      | Some _ , Some None -> true (* making a type abstract *)
      | None , Some (Some _) -> false (* can't specialize abstract type *)
      | Some tvalue , Some (Some tvalue') -> subtype tvalue tvalue'
      ) true
    )
  )
  | TNamespace _ , _ | _ , TNamespace _ -> failwith "subtyping namespace with non-namespace" *)
  | TNamespace_access _ , _
  (* | _ , TNamespace_access _ *)
    -> failwith "should not try to subtype namespace access"

let subtype_raw = subtype
let subtype = subtype_raw DAssoc.empty