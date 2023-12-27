open Agaml_ast
open Eval
open Bidir
module C = Context
module TC = TContext

(* Bunch of functions for quick use *)
let toplevel_eval = eval C.empty
let toplevel_teval = teval TC.empty
let toplevel_neval = neval C.empty
let toplevel_synthesize = synthesize TC.empty
let toplevel_synthesize_statements = synthesize_statements TC.empty
let toplevel_synthesize_namespace = synthesize_namespace TC.empty

