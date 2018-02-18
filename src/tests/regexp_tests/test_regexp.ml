open RegExp;;

print_string "Testing RegExp"

let r = RegExp.Zero
let b = RegExp.isZero r;;
assert(b == true);;

let r1 = RegExp.One
let b1 = RegExp.isZero r1;;
assert(b1 == false);;

let r2 = RegExp.Lit 0;;
let b2 = RegExp.isZero r2;;
assert(b2 == false);;

let r2b = RegExp.Lit 1;;
let r3 = RegExp.Plus(r2,r2b);;
let b3 = RegExp.isZero r3;;
assert(b3 == false);;

let r3b = RegExp.Plus(r2b,r2);;
let r4 = RegExp.Mult(r3,r3b);;
let b4 = RegExp.isZero r4;;
assert(b4 == false);;

let r5 = RegExp.Star(r2);;
let b5 = RegExp.isZero r5;;
assert(b5 == false);;


