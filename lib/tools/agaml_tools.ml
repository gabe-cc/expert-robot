(*
  Strange Datastructure
  An associative list that can be used forward (fst -> snd) or backward (snd -> fst).
  This is essentially useful for subtyping context:
  - You need an associative list to remap variables from one term to the other (in the context of alpha-renaming)
  - In contravariant contexts (subtyping function parameters), the subtyping relation order is reversed.
    - A naive implementation would just reverse all the items in the associative list.
    - This is instead done through the `Forward` / `Backward` tag, instantiating in which direction the bindings work.
  - By default, bindings go forward (fst -> snd), until `swap` is called.
*)
module DAssocRaw = struct
  type direction = Forward | Backward
  type 'a t = direction * ('a * 'a) list

  let empty = Forward , []
  let swap (d , lst) = (
    match d with
    | Forward -> Backward
    | Backward -> Forward
  ) , lst

  let append k v (d , lst) =
    d ,
    match d with
    | Forward -> (k , v) :: lst
    | Backward -> (v , k) :: lst
  
  let list_assoc_opt_rev k lst =
    let rec aux = function
    | [] -> None
    | (v , k') :: _ when k' = k -> Some v 
    | _ :: tl -> aux tl
    in aux lst

  let assoc_opt k (d , lst) =
    match d with
    | Forward -> List.assoc_opt k lst
    | Backward -> list_assoc_opt_rev k lst
end

module DAssoc : sig
  type 'a t

  val empty : 'a t
  val swap : 'a t -> 'a t
  val append : 'a -> 'a -> 'a t -> 'a t
  val assoc_opt : 'a -> 'a t -> 'a option
end = DAssocRaw

type 'a dassoc = 'a DAssoc.t

let list_take_n =
  let rec aux acc lst n =
    match n , lst with
    | 0 , _ -> List.rev acc
    | _ , [] -> assert false
    | _ , hd :: tl -> aux (hd :: acc) tl (n - 1)
  in
  fun n lst ->
  assert (List.length lst >= n) ;
  aux [] lst n

module Flag() = struct
  let flag = ref false
  let with_flag f =
    flag := true ;
    f () ;
    flag := false
  let counter = ref 0
  let counter_inc () = counter := !counter + 1 ; !counter - 1
end

module SSet = Set.Make(String)

module Test = struct
  module O = Ocolor_format

  let test_basic name f =
    (try (
      f () ;
      O.printf "@{<green>Pass@} [@{<it>%s@}]\n" name
    ) with e -> (
      O.eprintf "@{<red;bold>Failure@} [@{<it>%s@}]\nException:\n@{<orange>%s@}\n\n"
        name (Printexc.to_string e)
    ))

  let print_diff_values pp ~a ~b =
    O.eprintf "@[<v>Different values.@;First:[@{<red>%a@}]@;vs@;Second:[@{<green>%a@}]@;@]%!"
    pp a pp b

  let print_not_expected pp ~got ~expected =
    O.eprintf "@[<v>Got different from expected.@;Got [@{<red>%a@}]@;vs@;Expected [@{<green>%a@}]@;@]"
    pp got pp expected ;
    ()
end