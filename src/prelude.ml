module Prelude = struct
  (* sum types *)
  type ('a, 'b) either = Left of 'a | Right of 'b
  (* the constant function *)
  let const (x : 'a) (y : 'b) : 'a = x
  (* the identity function *)
  let id (x : 'a) = x
  (* function composition "f after g" *)
  let after (f : 'b -> 'c) (g : 'a -> 'b) (x : 'a) = f (g x)
  (* given a function of one argument (being a pair), turn it into a function of two arguments *)
  let curry (f : (('a * 'b) -> 'c)) (x : 'a) (y : 'b) : 'c = f (x, y)
  (* given a function of two arguments, turn it into a function of one argument (a pair) *)
  let uncurry (f : 'a -> 'b -> 'c) (p : ('a * 'b)) : 'c = f (fst p) (snd p)

  let first (triple : 'a * 'b * 'c) : 'a = match triple with
    (a , _ , _) -> a
  let second (triple : 'a * 'b * 'c) : 'b = match triple with
    (_ , b , _) -> b
  let third (triple : 'a * 'b * 'c) : 'c = match triple with
    (_ , _ , c) -> c
  exception TODO of string
  module StringMap = Map.Make(String)

  (* Implement the Natural numbers Peano style *)
  type nat = Zero | Succ of nat
  let rec nat_to_int = function
      Zero   -> 0
    | Succ n -> 1 + (nat_to_int n)
  let rec int_to_nat (i : int) : nat option = match i with
        0 -> Some Zero
      | n -> if (i < 0)
             then None
             else match int_to_nat (i - 1) with
                    None   -> None
                  | Some a -> Some (Succ a)
end
