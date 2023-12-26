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