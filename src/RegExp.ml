module RegExp = struct
  type 'a regexp =
    | Zero                               (* The empty language         -- L(Zero)  = ∅              *)
    | One                                (* The empty string, epsilon  -- L(One)   = {ε}            *)
    | Lit  of 'a                         (* Literal, single symbol     -- L(σ)     = {σ}, for σ ∈ Σ *)
    | Plus of ('a regexp) * ('a regexp)  (* Concatenation              -- L(α · β) = L(α) · L(β)    *)
    | Mult of ('a regexp) * ('a regexp)  (* Plus, union, or            -- L(α | β) = L(α) ∪ L(β)    *)
    | Star of ('a regexp)                (* Kleene star, repetition    -- L(α⋆)    = L(α)⋆          *)
  let rec nullable = function
       Zero        -> false
     | One         -> true
     | Lit  a      -> false
     | Plus (a, b) -> (nullable a) || (nullable b)
     | Mult (a, b) -> (nullable a) && (nullable b)
     | Star a      -> true
  let constant (r : 'a regexp) : 'a regexp = if nullable r then One else Zero
end
