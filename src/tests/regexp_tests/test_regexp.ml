open RegExp;;

print_string "Testing RegExp"

let r = RegExp.Zero
let b = RegExp.isZero r;;
assert(b == true);;

let r1 = RegExp.One
let b1 = RegExp.isZero r1;;
assert(b1 == false);;


