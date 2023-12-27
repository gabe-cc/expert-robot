open Types

type t = tctx
type binding = tctx_binding

let from_forward : ftctx -> t = fun (TForward x) -> List.rev x
let to_forward :  t -> ftctx = fun x -> TForward (List.rev x)

let empty : t = []

let type_get = function
| TCType x -> Some x
| _ -> None
let term_get = function
| TCTerm x -> Some x
| _ -> None
let namespace_get = function
| TCNamespace x -> Some x
| _ -> None

let append x v : t -> t = fun tctx ->
  (x , TCTerm v) :: tctx
let append_traw x m : t -> t = fun tctx ->
  (x , TCType m) :: tctx
let append_t x v : t -> t = fun tctx ->
  (x , TCType (Value v)) :: tctx
let append_thole x : t -> t = fun tctx ->
  (x , TCType (Hole)) :: tctx
let append_n x v : t -> t = fun tctx ->
  (x , TCNamespace v) :: tctx

let lookup_raw x : t -> binding option = fun tctx ->
  List.assoc_opt x tctx

let lookup x : t -> texpr option = fun tctx ->
  Option.bind (lookup_raw x tctx) term_get
let lookup_t x : t -> texpr member option = fun tctx ->
  Option.bind (lookup_raw x tctx) type_get
let lookup_n x : t -> tnexpr option = fun tctx ->
  Option.bind (lookup_raw x tctx) namespace_get

let to_ctx : t -> ctx = List.filter_map (fun (name , binding) ->
  match binding with
  | TCTerm (_ : texpr) -> Some (name , CTerm Hole)
  | TCType _ -> None
  | TCNamespace (_ : tnexpr) -> Some (name , CNamespace Hole)
)

(*
  `diff new old` returns all the newest entries from `new`, not present in `old`.
  It assumes that `new` is made from values added on top of `old`
*)
let diff : t -> t -> t = fun tctx1 tctx2 ->
  let open Agaml_tools in
  let l = List.length in
  list_take_n (l tctx1 - l tctx2) tctx1
