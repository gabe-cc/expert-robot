open Types

let toplevel_eval = eval []
let toplevel_synthesize = synthesize []

let let_in var exp body = LetIn (var , exp , body)
let var x = Variable x
let (!%) = var
let int n = Literal (LInt n)
let (!+%) = int
let string s = Literal (LString s)
let (!^%) = string
let vint n = VLiteral (LInt n)
let add a b = Builtin (BAdd (a , b))
let (+%) = add
let func x ty body = Function (x , ty , body)
let call a b = Call (a , b)
let (@%) = call
let tint = TBuiltin TInt
let record lst = Record lst
let vrecord lst = VRecord lst
let field a b = Field (a , b)
let ( /% ) = field
let case name content ty = Case (name , content , ty)
let c = case
let vcase name content = VCase (name , content)
let vc = vcase
let tvariant lst = TVariant lst
let match_ m lst = Match (m , lst)
let annot expr ty = Annotation (expr , ty)

let tlint n = TLiteral (LInt n)
let tlstring s = TLiteral (LString s)
let tstring = TBuiltin (TString)
let tarrow input output = TArrow (input , output)
let trecord lst = TRecord lst