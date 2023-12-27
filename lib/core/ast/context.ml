open Types

type t = ctx
type binding = ctx_binding

let empty : t = []

let term_get = function
| CTerm x -> Some x
| _ -> None
let namespace_get = function
| CNamespace x -> Some x
| _ -> None

let values_map f : t -> t = List.map (fun (name , content) ->
  name , match content with
  | CTerm x -> CTerm (f x)
  | (CNamespace _ as x) -> x
)
let namespaces_map f : t -> t = List.map (fun (name , content) ->
  name , match content with
  | CNamespace x -> CNamespace (f x)
  | (CTerm _ as x) -> x
)
let append x value : t -> t = fun ctx ->
  (x , CTerm (Value value)) :: ctx
let append_n x value : t -> t = fun ctx ->
  (x , CNamespace (Value value)) :: ctx
let append_expr x value : t -> t = fun ctx ->
  (x , CTerm (Expr value)) :: ctx
let append_hole x : t -> t = fun ctx ->
  (x , CTerm (Hole)) :: ctx
(* let append_t x value : t -> t = fun ctx ->
  (x , Type (Value value)) :: ctx
let append_traw x member : t -> t = fun ctx ->
  (x , Type member) :: ctx
let append_texpr x value : t -> t = fun ctx ->
  (x , Type (Expr value)) :: ctx
let append_thole x : t -> t = fun ctx ->
  (x , Type Hole) :: t *)
let append_nexpr x value : t -> t = fun ctx ->
  (x , CNamespace (Expr value)) :: ctx
let lookup_raw x : t -> binding option = fun ctx ->
  List.assoc_opt x ctx
let lookup x : t -> expr member option = fun ctx ->
  Option.bind (lookup_raw x ctx) term_get
let lookup_n x : t -> nexpr member option = fun ctx ->
  Option.bind (lookup_raw x ctx) namespace_get

(*
  `diff new old` returns all the newest entries from `new`, not present in `old`.
  It assumes that `new` is made from values added on top of `old`
*)
let diff : t -> t -> t = fun ctx1 ctx2 ->
  let open Agaml_tools in
  let l = List.length in
  list_take_n (l ctx1 - l ctx2) ctx1

