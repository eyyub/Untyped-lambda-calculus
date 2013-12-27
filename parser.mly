(* Grammar inspirate by http://www.cs.bham.ac.uk/~axj/pub/papers/lambda-calculus.pdf *)

%{
  open Church
%}

%token TLambda TDot
%token <string> TVar
%token <int> TInt
%token TLPA TRPA
%token TAffect
%token TPrint
%token TDoubleSemiColon TEof

%start <Ast.term> prgm

%%

prgm:
| s = sequence TEof{ s }

sequence:
| t = term TDoubleSemiColon { t }
| t = term TDoubleSemiColon s = sequence { Ast.Seq (t, s) }

term:
| atom = atomic { atom }
| app = application { app }
| abs = abstraction { abs }
| aff = affectation { aff }
| p = print { p }

atomic:
| h = headatom { h }
| TLPA a = application TRPA { a }

headatom:
| v = TVar { (Ast.Var v) }
| c = TInt { (num_to_church c) }
| TLPA a = abstraction TRPA{ a }

application:
| h = headatom a = atomic { Ast.App (h, a) }
| a = application a2 = atomic { Ast.App (a, a2) }

abstraction:
| TLambda v = TVar TDot t = term { Ast.Abs (v, t) }

affectation:
| v = TVar TAffect t = term { Ast.Affect (v, t) }

print:
| TPrint t = term { Ast.Print t }
