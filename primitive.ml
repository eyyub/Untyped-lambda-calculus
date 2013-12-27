open Ast

let str_expr_to_term str_expr = 
  (Parser.prgm Lexer.lexer (Lexing.from_string (str_expr ^ ";;")));;

(* 
** Primitives issues de Wikipedia -
** Je ne les comprends pas toutes.
*)

let primitive_env =
  [
    ("succ",         str_expr_to_term "λn.λf.λx.f (n f x)"                      );
    ("add",          str_expr_to_term "λm.λn.λf.λx.m f (n f x)"                 );
    ("+",            str_expr_to_term "λm.λn.λf.λx.m f (n f x)"                 );
    ("*",            str_expr_to_term "λm.λn.λf.m (n f)"                        );
    ("mul",          str_expr_to_term "λm.λn.λf.m (n f)"                        );
    ("pred",         str_expr_to_term "λn.λf.λx.n (λg.λh.h (g f)) (λu.x) (λu.u)");
    ("-",            str_expr_to_term "λm.λn.n pred m"                          );
    ("sub",          str_expr_to_term "λm.λn.n pred m"                          );
    ("true",         str_expr_to_term "λx.λy.x"                                 );
    ("false",        str_expr_to_term "λx.λy.y"                                 );
    ("and",          str_expr_to_term "λp.λq.p q p"                             );
    ("or",           str_expr_to_term "λp.λq.p p q"                             );
    ("not",          str_expr_to_term "λp.λa.λb.p b a"                          );
    ("if-then-else", str_expr_to_term "λp.λa.λb.p a b"                          );
    ("iszero?",      str_expr_to_term "λn.n (λx.false) true"                    );
    ("leq?",         str_expr_to_term "λm.λn.iszero? (- m n)"                   );
    ("geq?",         str_expr_to_term "λm.λn.not (iszero? (- m n))"             );
    ("Y",            str_expr_to_term "(λg.(λx.g (x x)) (λx.g (x x)))"          );
    ("rec",          str_expr_to_term "Y"                                       )
  ]
;;
