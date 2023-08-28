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
let tvariant lst = TVariant lst
let tevariant lst = TEVariant lst
let match_ m lst = Match (m , lst)
let annot expr ty = Annotation (expr , ty)
let case name content ty = annot (Case (name , content)) ty
let case' name content = Case (name , content)
let c = case
let c' = case'
let vcase name content = VCase (name , content)
let vc = vcase
let tlint n = TLiteral (LInt n)
let tlstring s = TLiteral (LString s)
let tstring = TBuiltin (TString)
let tarrow input output = TArrow (input , output)
let tmu var body = TMu (var , body)
let tevar var = TEVar var
let trecord lst = TRecord lst
let terecord lst = TERecord lst
let tunit = trecord []
let teunit = terecord []
let unit = record []
let te = Debug.texpr_of_tvalue
let (!?%) = te
let fold e t = annot (Fold e) t
let unfold e = Unfold e
let vfold e = VFold e
let vunit = vrecord []