module Prelude = struct
  type ('a, 'b) either = Left of 'a | Right of 'b
  let const (x : 'a) (y : 'b) : 'a = x
  let id (x : 'a) = x
  exception TODO of string

  type nat = Zero | Succ of nat
  let rec nat_to_int = function
      Zero   -> 0
    | Succ n -> 1 + (nat_to_int n)
  let rec int_to_nat (i : int) : nat option =
      if (i < 0)
      then None
      else if (i = 0)
           then Some Zero
           else match int_to_nat (i - 1) with
                  None   -> None
                | Some a -> Some (Succ a)
end
