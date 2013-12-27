{
  open Tokens
  exception LexingError of string
}

let digit = ['0' - '9']
let alpha = ['a' - 'z' 'A' - 'Z']
let empty = ['\n' '\t' ' ']
let primitive =
  ['+'
   '-'
   '*'
   '/'
   '%'
  ]
  |"pred"
  |"succ"
  |"plus"
  |"sub"
  |"add"
  |"if-then-else"
  |"or"
  |"and"
  |"true"
  |"false"
  |"iszero?"
  |"leq?"
  |"geq?"

rule lexer = parse
  | "#" [^'\n']* '\n'?                        { lexer lexbuf                                       }
  | eof                                       { TEof                                               }
  | empty+                                    { lexer lexbuf                                       }
  | "λ"                                       { TLambda                                            }
  | "."                                       { TDot                                               }
  | "("                                       { TLPA                                               }
  | ")"                                       { TRPA                                               }
  | ":="                                      { TAffect                                            }
  | ";;"                                      { TDoubleSemiColon                                   }
  | "print"                                   { TPrint                                             }
  | digit+ as n                               { TInt (int_of_string n)                             }
  | (alpha (alpha | digit)* | primitive) as v { TVar v                               }
  | _ as error                                { raise (LexingError ("Unknown " ^ Char.escaped error ^ "token." ))}
