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

let xs = List.map (fun x -> RegExp.Lit x) [1; 2; 3; 4; 5; 6; 5; 4; 3; 2; 1; 0; 1; 1;1;7;8;9];;
let ys = List.map (fun x -> RegExp.Lit x) [0; 1; 2; 3; 4; 5; 6; 7; 8; 9];;
let digit = List.fold_left (fun acc b -> RegExp.plus acc b) RegExp.Zero ys;;
(* assert the algebraic properties properly reduce
   the regex (1 | 2 | 3 | 4 | 5 | 6 | 5 | 4 | 3 | 2 | 1 | 0 | 1 | 1 | 1 | 7 | 8 | 9 )
   to        (0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9)
*)
assert (digit = List.fold_left (fun acc b -> RegExp.plus acc b) RegExp.Zero xs)
assert (digit = RegExp.Plus (RegExp.Lit 0, RegExp.Plus (RegExp.Lit 1, RegExp.Plus (RegExp.Lit 2, RegExp.Plus (RegExp.Lit 3, RegExp.Plus (RegExp.Lit 4, RegExp.Plus (RegExp.Lit 5, RegExp.Plus (RegExp.Lit 6, RegExp.Plus (RegExp.Lit 7, RegExp.Plus (RegExp.Lit 8, RegExp.Lit 9)))))))))

assert (RegExp.matches (RegExp.star digit) [1;2;3])
