Untyped-lambda-calculus
=======================

First real OCaml project to discover what is Lambda Calculus.

File example :
a comment
B := (λx.x);;
A := 42;;
print B 42;;
print - 63 A;;
print if-then-else (iszero? 0) (+ 21 B) 42;;
print if-then-else (leq? 0 42) 1 0;;
foo := if-then-else (geq? 0 42) 1 0;;
print foo;;
fact :=  Y (λr.λn.if-then-else (iszero? n) 1 (* n (r (- n 1))));;
print fact 4;;
A := 3;;
B := A;;
print B;;

Primitives :
succ         := λn.λf.λx.f (n f x)
add          := λm.λn.λf.λx.m f (n f x)
+            := add
mul          := λm.λn.λf.m (n f)
*            := mul
pred         := λn.λf.λx.n (λg.λh.h (g f)) (λu.x) (λu.u)
sub          := λm.λn.n pred m
-            := sub 
true         := λx.λy.x
false        := λx.λy.y
and          := λp.λq.p q p
or           := λp.λq.p p q
not          := λp.λa.λb.p b a
if-then-else := λp.λa.λb.p a b
iszero?      := λn.n (λx.false) true
leq?         := λm.λn.iszero? (- m n)
geq?         := λm.λn.not (iszero? (- m n))
Y            := (λg.(λx.g (x x)) (λx.g (x x)))
rec          := Y
