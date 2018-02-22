open Prelude

module DFA = struct
  (* TODO use functor and ordered type parameter if possible *)
  type q = int
  type s = char

  type t = { delta : (q * s) -> q;
             q0    : q;
             f     : q list; (* TODO use set instead *)
           }
  let make (delta : (q * s) -> q) (q0 : q) (f : q list) : t =
    { delta = delta;
      q0    = q0;
      f     = f; }
  (*
  deltaStar :: DFA q s -> q -> [s] -> q
  deltaStar = foldl . curry . delta
  *)
  let deltaStar ({ delta; q0 ; _ } : t) (start : q) (word : s list) : q = List.fold_left (Prelude.curry delta) start word
  (*
  eval :: DFA q s -> [s] -> q
  eval m@(DFA _ q₀ _) = deltaStar m q₀
  *)
  let eval ({ delta ; q0 ; _ } as m : t) (word : s list) : q = deltaStar m q0 word
  (*
  -- Take a DFA, m, and a string, w, and decide if that string is accepted/recognized
  -- m accepts a string w ∈ Σ⋆ iff δ⋆(q₀, w) ∈ F
  accepts :: (Ord q) =>                    DFA q s -> [s] -> Bool
  accepts m@(DFA _ _ f) w = eval m w ∈ f
  *)
  let accepts ({ delta ; q0 ; f } as m : t) (word : s list) : bool = List.mem (eval m word) f
end
