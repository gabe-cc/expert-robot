
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
| LetInT of var * texpr * expr
| Namespace_access of nexpr * string
[@@deriving show { with_path = false }]

(* namespace expression *)
and nexpr =
| NStatements of statements
| NNamespace_access of nexpr * string
| NVariable of string
| NMap of {
  vars : (string * expr) list ; (* values when full *)
  nvars : (string * nexpr) list ;
} (* this is nvalue *)

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
| TCall of texpr * texpr (* type application *)
| TNamespace_access of nexpr * string

and tnexpr =
| TNNamespace of ftctx
(* | TNVar of string *)

and var = string

and statement =
| SLet of var * expr
| SLetType of var * texpr
| SLetNamespace of var * nexpr
| SExpr of expr

and statements = statement list

and 'a member =
| Value of 'a
| Expr of 'a
| Hole

and ctx_binding =
| CTerm of expr member
| CNamespace of nexpr member

and tctx_binding =
| TCTerm of texpr
| TCType of texpr member
| TCNamespace of tnexpr

(*
  Context is backward when used as an evaluation context. The list is then a stack, where its beginning contains the latest added value.

  It is forward when used as a list of declaration. The beginning of the list contains the oldest declaration.

  By default, context is backward.

  TODO: Move to data-structure where lookups are backward, and iterations are forward.
*)
and ctx = (string * ctx_binding) list
and tctx = (string * tctx_binding) list

(* Forward contexts *)
and fctx = Forward of ctx
and ftctx = TForward of tctx

let statements_assoc_opt (x : string) : statements -> expr option =
  fun statements ->
  statements |> List.filter_map (function
    | SLet (var , expr) -> Some (var , expr)
    | SLetNamespace _ -> None
    | SExpr _ -> None
    | SLetType _ -> None
  ) |> List.assoc_opt x

module F = Format

module Debug = struct
end