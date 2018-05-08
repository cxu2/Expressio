module Prelude = struct
  (* sum types *)
  type ('a, 'b) either = Left of 'a | Right of 'b
  (* the constant function *)
  let const (x : 'a) (_ : 'b) : 'a = x
  (* the identity function *)
  let id (x : 'a) = x
  (* function composition "f after g" *)
  let after (f : 'b -> 'c) (g : 'a -> 'b) (x : 'a) : 'c = f (g x)
  (* given a function of one argument (being a pair), turn it into a function of two arguments *)
  let curry (f : (('a * 'b) -> 'c)) (x : 'a) (y : 'b) : 'c = f (x, y)
  (* given a function of two arguments, turn it into a function of one argument (a pair) *)
  let uncurry (f : 'a -> 'b -> 'c) (p : ('a * 'b)) : 'c = f (fst p) (snd p)
  (* given a function of two arguments, swap the arguments *)
  let flip (f : 'a -> 'b -> 'c) (x : 'b) (y : 'a) : 'c = f y x

  let swap ((x, y) : ('a * 'b)) : ('b * 'a) = (y, x)

  let rec map_accum_left (f : 'acc -> 'x -> ('acc * 'y)) (s : 'acc) (zs : 'x list) : 'acc * ('y list) =
    match zs with
      []        -> (s, [])
    | (x :: xs) ->    let (s',  y ) = f s x
                   in let (s'', ys) = map_accum_left f s' xs
                   in (s'', y :: ys)

  let first'  (quadruple : 'a * 'b * 'c * 'd) : 'a = match quadruple with
    (a , _ , _ , _) -> a
  let second' (quadruple : 'a * 'b * 'c * 'd) : 'b = match quadruple with
    (_ , b , _ , _) -> b
  let third'  (quadruple : 'a * 'b * 'c * 'd) : 'c = match quadruple with
    (_ , _ , c , _) -> c
  let fourth' (quadruple : 'a * 'b * 'c * 'd) : 'd = match quadruple with
    (_ , _ , _ , d) -> d

  let first  (triple : 'a * 'b * 'c) : 'a = match triple with
    (a , _ , _) -> a
  let second (triple : 'a * 'b * 'c) : 'b = match triple with
    (_ , b , _) -> b
  let third  (triple : 'a * 'b * 'c) : 'c = match triple with
    (_ , _ , c) -> c

  (* An exception to be used in places where scaffolding for unimplemented code is needed *)
  exception TODO of string
  (* An exception to be used when marking that pattern matching has reached an assumed to be impossible state *)
  exception ABSURD

  module StringMap = Map.Make(String)

  (* type alias for StringMap's type to make it more consistent with OCaml, e.g. like the `list` type *)
  type 'a string_map = 'a StringMap.t

  (* fromList :: [(String, a)] -> Map String a *)
  let fromList (xs : (string * 'a) list) : 'a StringMap.t = List.fold_left (fun acc (k, v) -> StringMap.add k v acc) StringMap.empty xs

  (* let print_string_map = StringMap.iter (Printf.printf "%s -> %d\n") *)
  (* let print_smap (string_of : 'a -> string) (m : 'a StringMap.t) = StringMap.iter (fun k v -> Printf.printf "%s -> %s\n" k (string_of v)) m *)

  let error (message : string) = raise (Failure message)

  (* Implement the Natural numbers Peano style *)
  type nat = Zero | Succ of nat
  let rec nat_to_int = function
      Zero   -> 0
    | Succ n -> 1 + (nat_to_int n)
  let rec int_to_nat (i : int) : nat option = match i with
        0 -> Some Zero
      | n -> if (n < 0)
             then None
             else match int_to_nat (n - 1) with
                    None   -> None
                  | Some a -> Some (Succ a)
end
