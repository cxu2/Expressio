open Prelude

module DFA = struct
  (* TODO use functor and ordered type parameter if possible *)
  type q = int
  type s = char

  type 'q t = { delta : ('q * s) -> 'q
              ; q0    : 'q
              ; f     : 'q list (* TODO use set instead *)
              }
  let make (delta : ('q * s) -> 'q) (q0 : 'q) (f : 'q list) : 'q t =
    { delta = delta
    ; q0    = q0
    ; f     = f }
  (*
  deltaStar :: DFA q s -> q -> [s] -> q
  deltaStar = foldl . curry . delta
  *)
  let deltaStar ({ delta; q0 ; _ } : 'q t) (start : 'q) (word : s list) : 'q = List.fold_left (Prelude.curry delta) start word
  (*
  eval :: DFA q s -> [s] -> q
  eval m@(DFA _ q₀ _) = deltaStar m q₀
  *)
  let eval ({ delta ; q0 ; _ } as m : 'q t) (word : s list) : 'q = deltaStar m q0 word
  (*
  -- Take a DFA, m, and a string, w, and decide if that string is accepted/recognized
  -- m accepts a string w ∈ Σ⋆ iff δ⋆(q₀, w) ∈ F
  accepts :: (Ord q) =>                    DFA q s -> [s] -> Bool
  accepts m@(DFA _ _ f) w = eval m w ∈ f
  *)
  let accepts ({ delta ; q0 ; f } as m : 'q t) (word : s list) : bool = List.mem (eval m word) f
  (*
  The product construction
  Essentially this runs two DFAs in parallel
  productConstruct :: DFA q s -> DFA p s -> Set (q, p) -> DFA (q, p) s
  productConstruct (DFA δ₁ q₀ _) (DFA δ₂ p₀ _) fs' = DFA { delta = δ'
                                                         , q0    = (q₀, p₀)
                                                         , fs    = fs'
                                                         } where δ' ((q, p), σ) = (δ₁ (q, σ), δ₂ (p, σ))
  *)
  (* let product ({delta1 ; q0 ; _} as m1 : t) ({delta2 ; p0 ; _} as m2 : t) (finals : q list) : t =  *)
  let product ({delta = delta1 ; q0 = q0 ; _} : 'q t) ({delta = delta2 ; q0 = p0 ; _} : 'p t) (finals : ('q * 'p) list) : ('q * 'p) t =
      (* { delta = (fun (((q : 'q), (p : 'p)), (s : s)) -> (delta1 (q, s), delta2 (p, s))) *)
      { delta = (fun ((q, p), s) -> (delta1 (q, s), delta2 (p, s)))
      ; q0    = (q0, p0)
      ; f     = finals
      }
  (*
  -- The union of two DFAs, m₁ and m₂, produces a new DFA, m₃, such that
  -- L(m₃) = L(m₁) ∪ L(m₂)
  -- F = {(q, p) | q ∈ F₁  ∨  p ∈ F₂}
  union :: (Ord q, Enum q, Ord p, Enum p) =>        DFA q s -> DFA p s -> DFA (q, p) s
  union m₁@(DFA _ _ f₁) m₂@(DFA _ _ f₂) = productConstruct m₁ m₂ ((f₁ × p) ∪ (q × f₂))
          where q = qs m₁
                p = qs m₂
                *)
  let union ({delta = delta1 ; q0 ; f = f1} as m1 : 'q t) ({delta = delta2 ; q0 ; f = f2} as m2 : 'p t) : ('q * 'p) t =
    product m1 m2 [] (* FIXME need OCaml equivalent of *)
end
