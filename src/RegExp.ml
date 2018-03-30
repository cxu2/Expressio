open Prelude
module RegExp = struct
  type 'a regexp =
    | Zero                              (* The empty language             L(Zero)  = ∅              *)
    | One                               (* The empty string, epsilon      L(One)   = {ε}            *)
    | Lit  of 'a                        (* Literal, single symbol         L(σ)     = {σ}, for σ ∈ Σ *)
    | Plus of ('a regexp) * ('a regexp) (* Multiplication, Concatenation  L(α · β) = L(α) · L(β)    *)
    | Mult of ('a regexp) * ('a regexp) (* Plus, union, or                L(α | β) = L(α) ∪ L(β)    *)
    | Star of ('a regexp)               (* Kleene star, repetition        L(α⋆)    = L(α)⋆          *)
    (* TODO better name? *)
    | And  of ('a regexp) * ('a regexp) (* Intersection (logical and)     L(α & β) = L(α) ∩ L(β)    *)
    | Comp of ('a regexp)               (* complement ¬                   L(¬α)    = Σ⋆ \ L(α)      *)


  let rec star (r : 'a regexp) : 'a regexp = match r with
      Zero   -> One     (* ∅⋆ ≈ ε *)
    | One    -> One     (* ε⋆ ≈ ε *)
    | Star a -> star a  (* recursively apply idempotence L⋆⋆ ≈ L⋆ *)
    | a      -> Star a
  (*
  Concatentation is associative          (LM)N = L(MN)
  ε is identity for concatenation         εL = Lε = L
  ∅ is annihilation for concatenation    ∅L = L∅ = ∅
  distributes over +     L(M+N) = LM + LN
                          (M+N)L = ML + NL
  *)
  let rec mult (r1 : 'a regexp) (r2 : 'a regexp) : 'a regexp = match (r1, r2) with
      (_,             Zero)         -> Zero                    (* Annihilation for mult is ∅ *)
    | (Zero,          _)            -> Zero                    (* Annihilation for mult is ∅ *)
    | (a,             One)          -> a                       (* Identity for mult is ε *)
    | (One,           b)            -> b                       (* Identity for mult is ε *)
    | (Mult (a1, a2), b)            -> Mult (a1, (mult a2 b))  (* Associativity will be to the right in normal form *)
    | (Star a,   Star b) when a = b -> star a
    | (a,             b)            -> Mult (a, b)
  (*
  Union is commutative L+M = M+L
           associative (L+M)+N = L+(M+N)
           idempotent  L + L = L
  *)
  let rec plus (r1 : 'a regexp) (r2 : 'a regexp) : 'a regexp = match (r1, r2) with
      (a,             Zero)          -> a                   (* Identity for plus is ∅ *)
    | (Zero,          b)             -> b                   (* Identity for plus is ∅ *)
    | (Plus (a1, a2), b)             -> plus a1 (plus a2 b) (* Associativity will be to the right in normal form *)
    | (a,             Plus (b1, b2)) -> if a = b1           (* Idempotent plus *)
                                        then Plus (b1, b2)
                                        else if a < b1
                                             then Plus (a,  (Plus (b1, b2)))
                                             else Plus (b1, plus a b2)
    | (a,             b)             -> if a = b            (* Idempotent plus *)
                                        then a
                                        else if a < b
                                             then Plus (a, b)
                                             else Plus (b, a)
  (* FIXME might need to add more cases for comp and intersect algebraic properties *)
  let comp (r : 'a regexp) : 'a regexp = match r with
    | Comp a -> a
    | a      -> Comp a
  let intersect (r1 : 'a regexp) (r2 : 'a regexp) : 'a regexp = match (r1, r2) with
      (Zero, _)      -> Zero
    | (Comp Zero, b) -> b
    | (a, b)         -> And (a, b)
  let rec normalize = function
      Zero        -> Zero
    | One         -> One
    | Lit  c      -> Lit c
    | Plus (a, b) -> plus (normalize a) (normalize b)
    | Mult (a, b) -> mult (normalize a) (normalize b)
    | Star a      -> star (normalize a)
    | And (_, _)  -> raise (Prelude.TODO "") (* intersect (normalize a) (normalize b) *)
    | Comp _      -> raise (Prelude.TODO "") (* comp (normalize a) *) (* should be, `comp (normalize a)` but I'll double check first *)

  (* Does the language of this RE contain the empty string? *)
  let rec nullable = function
      Zero        -> false
    | One         -> true
    | Lit  _      -> false
    | Plus (a, b) -> (nullable a) || (nullable b)
    | Mult (a, b) -> (nullable a) && (nullable b)
    | Star _      -> true
    | And _       -> raise (Prelude.TODO "")
    | Comp _      -> raise (Prelude.TODO "")
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
      | And  _      -> raise (Prelude.TODO "intersection")
      | Comp _      -> raise (Prelude.TODO "finite'")
    in finite' (normalize r)
  let infinite (r : 'a regexp) : bool = not (finite r)
  (* Brzozowski derivative with respect to s ∈ Σ *)
  let rec derivative (r : 'a regexp) (s : 'a) : 'a regexp = match r with
      Zero        -> Zero
    | One         -> Zero
    | Lit  c      -> if c = s then One else Zero
    | Plus (a, b) -> plus (derivative a s) (derivative b s)
    | Mult (a, b) -> plus (mult (derivative a s) b) (mult (constant a) (derivative b s))
    | Star a      -> mult (derivative a s) (star a)
    (* TODO double check *)
    | And (a, b)  -> intersect (derivative a s) (derivative b s)
    | Comp a      -> comp (derivative a s)
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
      | And (_, _)  -> raise (Prelude.TODO "isZero")
      | Comp _      -> raise (Prelude.TODO "isZero")
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
    | And (a, b)  -> And  (fmap f a, fmap f b)
    | Comp a      -> Comp (fmap f a)
  (* Regular languages are closed under reversal
     adapted from proof on slide 12
     http://infolab.stanford.edu/~ullman/ialc/spr10/slides/rs2.pdf
  *)
  let rec reversal = function
      Zero        -> Zero
    | One         -> One
    | Lit  s      -> Lit  s
    | Plus (a, b) -> Plus (reversal a, reversal b)
    | Mult (a, b) -> Mult (reversal b, reversal a)
    | Star a      -> Star (reversal a)
    | And (_, _)  -> raise (Prelude.TODO "reversal")
    | Comp _      -> raise (Prelude.TODO "reversal")

  let rec string_of_re = function
      Zero         -> "∅"
    | One          -> "ε"
    | Lit  c       -> "(lit " ^ (String.make 1 c) ^ ")"
    | Plus (a, b)  -> "(" ^ string_of_re a ^ "+" ^ string_of_re b ^ ")"
    | Mult (a, b)  -> "(" ^ string_of_re a ^ "." ^ string_of_re b ^ ")"
    | Star (Lit c) -> (String.make 1 c) ^ "⋆"
    | Star a       -> "(" ^ string_of_re a ^ ")⋆"
    | And  (a, b)  -> "(" ^ string_of_re a ^ "&" ^ string_of_re b ^ ")"
    | Comp a       -> "¬(" ^ string_of_re a ^ ")"
end
