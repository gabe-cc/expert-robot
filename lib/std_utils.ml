let list_take_n =
  let rec aux acc lst n =
    match n , lst with
    | 0 , _ -> List.rev acc
    | _ , [] -> assert false
    | _ , hd :: tl -> aux (hd :: acc) tl (n - 1)
  in
  fun n lst ->
  assert (List.length lst >= n) ;
  List.rev @@ aux [] lst n