open Ast

let num_to_church n =
  let rec aux i nb =
    if i = n then Abs ("f", Abs ("x", nb))
    else aux (i + 1) (App (Var "f", nb))
  in (aux 0 (Var "x"))
;;

let church_to_num = function
  | Abs ("f", Abs ("x", a)) ->
    let rec aux i = function
      | Var "x" -> i
      | App (Var "f", b) -> aux (i + 1) b
      | _ -> failwith "Church numeral bad format."
    in aux 0 a
  | _ -> failwith "Church numeral bad format."
;;
