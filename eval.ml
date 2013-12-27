open Ast
open Primitive
open Church
open Utils

let fv e =
  let rec aux l = function
    | Var x -> x :: l
    | Abs (x, t) -> List.filter (fun a -> a <> x) (aux l t) 
    | App (a, b) | Seq (a, b) -> uniq ((aux l a) @ (aux l b))
    | Affect (_, t) -> aux l t
    | _ -> l
  in aux [] e
;;

let rec substitution t x r =
  match t with
  | Var v when v = x -> r
  | App (t1, t2) -> App (substitution t1 x r, substitution t2 x r)
  | Abs (p, e) when p <> x && find p (fv r) = false -> Abs (p, substitution e x r)
  | _ -> t
;;

let rec perform_beta_reduction env e =
   match e with
   | App (Abs(x, t), s) -> (substitution t x s) (* beta_reduction *)
   | App (Var v, t) when has_key v env -> perform_beta_reduction env (App (get_val v env, t))
   | App (v, Var t) when has_key t env -> perform_beta_reduction env (App (v, get_val t env))
   | Abs (s, t) -> Abs (s, perform_beta_reduction env t)
   | App (a, b) -> App (perform_beta_reduction env a, perform_beta_reduction env b)
   | _ -> e
 ;;

let rec has_beta_redex env = function
  | App (Var v, t) ->
    if has_key v env then
      has_beta_redex env (App (get_val v env, t))
    else has_beta_redex env t
  | App (Abs(_,_), _) -> true
  | App (t1, t2) -> has_beta_redex env t1 || has_beta_redex env t2
  | Abs (s, t) -> has_beta_redex env t
  | _ -> false
;;

let rec print_expr env t =
  match t with
  | Var v ->
    if has_key v env then
      print_expr env (get_val v env)
    else failwith "Error variable " ^ v ^ "not founded."
  | _ -> Printf.printf "%d\n" (church_to_num (eval_term env t))   
;;

let rec eval_expr env e =
  match e with
  | Affect (k, v) -> update env k v
  | Seq (a, b) -> let env' = eval_expr env a in eval_expr env' b
  | Print t -> print_expr env t; env
  | _ -> env

and eval_term env e =
  if has_beta_redex env e then
    eval_term env (perform_beta_reduction env e)
  else e
;;

let () = 
  let e = (Parser.prgm Lexer.lexer (Lexing.from_channel (open_in Sys.argv.(1))))
  in (eval_expr primitive_env e)
;;
