type term =
  | Var    of string
  | Int    of term
  | Abs    of string * term
  | App    of term   * term
  | Affect of string * term
  | Print  of term
  | Seq    of term   * term
;;
