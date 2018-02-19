open RegExp;;

print_endline "Testing RegExp";;

print_endline "Zero & isZer0 ...";;
let r = RegExp.Zero
let b = RegExp.isZero r;;
assert(b = true);;
print_endline "pass";;

print_endline "One ...";;
let r1 = RegExp.One
let b1 = RegExp.isZero r1;;
assert(b1 == false);;
print_endline "pass";;

print_endline "Lit ...";;
let r2 = RegExp.Lit 0;;
let b2 = RegExp.isZero r2;;
assert(b2 = false);;
print_endline "pass";;

print_endline "plus ...";;
let r2b = RegExp.Lit 1;;
let r3 = RegExp.Plus(r2,r2b);;
let b3 = RegExp.isZero r3;;
assert(b3 = false);;
print_endline "pass";;

print_endline "mult ...";;
let r3b = RegExp.Plus(r2b,r2);;
let r4 = RegExp.Mult(r3,r3b);;
let b4 = RegExp.isZero r4;;
assert(b4 = false);;
print_endline "pass";;

print_endline "star ...";;
let r5 = RegExp.Star(r2);;
let b5 = RegExp.isZero r5;;
assert(b5 = false);;
print_endline "pass";;

print_endline "finite and infinite ...";;
let r0 = RegExp.Lit 0;;
let r1 = RegExp.Lit 1;;
assert(RegExp.infinite (RegExp.star r1) = true);;
assert((RegExp.infinite r0) = false);;
assert((RegExp.finite (RegExp.star r1)) = false);;
assert((RegExp.finite r0) = true);;
print_endline "pass";;

print_endline "matches ...";;
let inf0 = RegExp.star r0;;
assert((RegExp.matches inf0 []) == true);;

assert((RegExp.matches (RegExp.star (RegExp.mult r0 r0)) []) = true);;
assert((RegExp.matches (RegExp.star (RegExp.mult r0 r0)) [0]) = false);;
print_endline "pass";;

print_endline "equailty & normalize ...";;
let silly = RegExp.star (RegExp.mult inf0 inf0);;
let a0 = RegExp.Lit 0;;
assert(r0 = a0);;
assert((RegExp.normalize silly) = silly);;
print_endline "pass";;

print_endline "nullable ...";;
assert((RegExp.nullable inf0) = true);;
assert((RegExp.nullable r0) = false);;
print_endline "pass";;

print_endline "reversal ...";;
let ordd = RegExp.mult r0 r1;;
assert((RegExp.matches ordd [0 ; 1]) = true);;
assert((RegExp.matches (RegExp.reversal ordd) [0 ; 1]) = false);;
assert((RegExp.matches (RegExp.reversal ordd) [1 ; 0]) = true);;
print_endline "pass";;


(* x val mult : 'a regexp -> 'a regexp -> 'a regexp
   x val plus : 'a regexp -> 'a regexp -> 'a regexp
   x val star : 'a regexp -> 'a regexp
   x val normalize : 'a regexp -> 'a regexp
   x val nullable : 'a regexp -> bool
    val constant : 'a regexp -> 'a regexp
   x val finite : 'a regexp -> bool
   x val infinite : 'a regexp -> bool
    val derivative : 'a regexp -> 'a -> 'a regexp
    val derivative' : 'a regexp -> 'a list -> 'a regexp
   x val isZero : 'a regexp -> bool
   x val matches : 'a regexp -> 'a list -> bool
    val fmap : ('a -> 'b) -> 'a regexp -> 'b regexp
   x val reversal : 'a regexp -> 'a regexp *)