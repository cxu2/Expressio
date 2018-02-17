exception TODO of string

module RegExp = struct
  type 'a regexp =
    | Zero                               (* The empty language             -- L(Zero)  = ∅              *)
    | One                                (* The empty string, epsilon      -- L(One)   = {ε}            *)
    | Lit  of 'a                         (* Literal, single symbol         -- L(σ)     = {σ}, for σ ∈ Σ *)
    | Plus of ('a regexp) * ('a regexp)  (* Multiplication, Concatenation  -- L(α · β) = L(α) · L(β)    *)
    | Mult of ('a regexp) * ('a regexp)  (* Plus, union, or                -- L(α | β) = L(α) ∪ L(β)    *)
    | Star of ('a regexp)                (* Kleene star, repetition        -- L(α⋆)    = L(α)⋆          *)
  (*
  -- Concatentation is associative          (LM)N = L(MN)
  -- ε is identity for concatenation
  -- ∅ is annihilation for concatenation    ∅L = L∅ = ∅
  -- distributes over +     L(M+N) = LM + LN
  --                        (M+N)L = ML + NL
  *)
  let rec mult (r1 : 'a regexp) (r2 : 'a regexp) : 'a regexp = match (r1, r2) with
      (_, Zero) -> Zero                                        (* -- Annihilation for mult is ∅ *)
    | (Zero, _) -> Zero                                        (* -- Annihilation for mult is ∅ *)
    | (One, b)  -> b                                           (* -- Identity for mult is ε *)
    | (a, One)  -> a                                           (* -- Identity for mult is ε *)
    | (a, b)    -> match a with
                      Mult (a1, a2) -> Mult (a1, (mult a2 b))
                    | _             -> Mult (a, b)
  (*
  -- Union is commutative L+M = M+L
  --          associative (L+M)+N = L+(M+N)
  --          idempotent  L + L = L
  *)
  let rec plus (r1 : 'a regexp) (r2 : 'a regexp) : 'a regexp = match (r1, r2) with
      (a, Zero) -> a     (* Identity for plus is ∅ *)
    | (Zero, b) -> b     (* Identity for plus is ∅ *)
    | (_, _) -> raise (TODO "finish implementation")
  let rec star (r : 'a regexp) : 'a regexp = match r with
      Zero   -> One     (* -- ∅⋆ ≈ ε *)
    | One    -> One     (* -- ε⋆ ≈ ε *)
    | Star a -> star a  (* -- recursively apply idempotence L⋆⋆ ≈ L⋆ *)
    | a      -> Star a
  let rec normalize = function
      Zero        -> Zero
    | One         -> One
    | Lit  c      -> Lit c
    | Plus (a, b) -> plus (normalize a) (normalize b)
    | Mult (a, b) -> mult (normalize a) (normalize b)
    | Star a      -> star (normalize a)
  let rec nullable = function
      Zero        -> false
    | One         -> true
    | Lit  a      -> false
    | Plus (a, b) -> (nullable a) || (nullable b)
    | Mult (a, b) -> (nullable a) && (nullable b)
    | Star a      -> true
  let constant (r : 'a regexp) : 'a regexp = if nullable r then One else Zero
  (* Check if the the regular expression, r, produces a finite language.
     This is accomplished by finding the normal form of r
     (which removes extra Kleene star operations) and then checking if
     that normal form still has a Kleene star. *)
  let finite (r : 'a regexp) : bool =
    let rec finite' = function
        Zero        -> true
      | One         -> true
      | Lit  _      -> true
      | Plus (a, b) -> (finite' a) && (finite' b)
      | Mult (a, b) -> (finite' a) && (finite' b)
      | Star _      -> false
    in finite' (normalize r)
  let infinite (r : 'a regexp) : bool = not (finite r)
  (* Brzozowski derivative with respect to σ ∈ Σ *)
  let rec derivative (r : 'a regexp) (s : 'a) : 'a regexp = match r with
      Zero        -> Zero
    | One         -> Zero
    | Lit c       -> if c = s then One else Zero
    | Plus (a, b) -> plus (derivative a s) (derivative b s)
    | Mult (a, b) -> plus (mult (derivative a s) b) (mult (constant a) (derivative b s))
    | Star a      -> mult (derivative a s) (star a)
  let derivative' (r : 'a regexp) (word : 'a list) = List.fold_left derivative r word
  (* can be written point-free as:
   let derivative' = List.fold_left derivative
  *)
  (*
  -- Given a Regular Expression, r, decide if it produces the empty language, i.e.
  -- L(r) ≟ ∅
  *)
  let isZero (r : 'a regexp) : bool =
    let rec isZero' = function
        Zero        -> true
      | One         -> false
      | Lit  _      -> false
      | Plus (a, b) -> (isZero' a) && (isZero' b)
      | Mult (a, b) -> (isZero' a) || (isZero' b)
      | Star _      -> false
    in isZero' (normalize r)
  let rec matches (r : 'a regexp) (word : 'a list) = match r with
      Zero -> false
    | a    -> match word with
                []        -> constant a = One
              | (x :: xs) -> matches (derivative a x) xs
  let rec fmap f (r : 'a regexp) = match r with
      Zero        -> Zero
    | One         -> One
    | Lit  s      -> Lit  (f s)
    | Plus (a, b) -> Plus (fmap f a, fmap f b)
    | Mult (a, b) -> Mult (fmap f a, fmap f b)
    | Star a      -> Star (fmap f a)
  (* Regular languages are closed under reversal
     adapted from proof on slide 12
     http://infolab.stanford.edu/~ullman/ialc/spr10/slides/rs2.pdf
  *)
  let rec reversal = function
      Zero        -> Zero
    | One         -> One
    | Lit  s      -> Lit s
    | Plus (a, b) -> Plus (reversal a, reversal b)
    | Mult (a, b) -> Mult (reversal b, reversal a)
    | Star a      -> Star (reversal a)
end
