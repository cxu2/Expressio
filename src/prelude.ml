module Prelude = struct
  type ('a, 'b) either = Left of 'a | Right of 'b
  let const (x : 'a) (y : 'b) : 'a = x
  let id (x : 'a) = x
  let third (triple : 'a * 'b * 'c) : 'c = match triple with
    (_ , _ , c) -> c
  exception TODO of string
  module StringMap = Map.Make(String)

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
