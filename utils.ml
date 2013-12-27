let find elem l =
  let rec aux = function
    | [] -> false
    | x :: xs -> if x = elem then true else aux xs
  in aux l
;;

let rec has_key k = function
  | [] -> false
  | (k', v') :: xs -> if k = k' then true else has_key k xs
;;

let rec get_val k = function
  | [] -> failwith ("Error not found " ^ k ^ " key.")
  | (k', v') :: xs -> if k = k' then v' else get_val k xs
;;

let update env key value =
  let rec aux = function
    | [] -> [(key, value)]
    | (k', v') :: xs ->
      if key = k' then (k', value) :: xs
      else (k', v') :: aux xs
  in aux env
;;

let uniq l =
  let rec aux l2 = function
    | [] -> l2
    | x :: xs ->
      if List.exists (fun a -> a = x) l2 then aux l2 xs
      else aux (x :: l2) xs
  in aux [] l
;;
