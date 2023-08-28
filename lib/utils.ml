open Types

let toplevel_eval = eval []
let toplevel_synthesize = synthesize ([] , [])

let let_in var exp body = LetIn (var , exp , body)
let var x = Variable x
let (!%) = var
let int n = Literal (LInt n)
let (!+%) = int
let string s = Literal (LString s)
let (!^%) = string
let add a b = Builtin (BAdd (a , b))
let (+%) = add
let func x ty body = Function (x , ty , body)
let call a b = Call (a , b)
let (@%) = call
let (<|%) = call
let tint = TBuiltin TInt
let record lst = Record lst
let field a b = Field (a , b)
let ( /% ) = field
let tvariant lst = TVariant lst
let match_ m lst = Match (m , lst)
let annot expr ty = Annotation (expr , ty)
let case name content ty = annot (Case (name , content)) ty
let case' name content = Case (name , content)
let c = case
let c' = case'
let tlint n = TLiteral (LInt n)
let tlstring s = TLiteral (LString s)
let tstring = TBuiltin (TString)
let tarrow input output = TArrow (input , output)
let tmu var body = TMu (var , body)
let trecord lst = TRecord lst
let tunit = trecord []
let unit = record []
let fold e t = annot (Fold e) t
let unfold e = Unfold e
let rec_ var ty body = annot (Rec (var , body)) ty
let kv_of_lst lst =
  List.mapi (fun i x -> string_of_int i , x) lst
let tuple lst = record @@ kv_of_lst lst
let ttuple lst = trecord @@ kv_of_lst lst
let eval_full x = Eval (true , x)
let eval_partial x = Eval (false , x)
let tvar x = TVar x