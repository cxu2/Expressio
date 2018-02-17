module Prelude = struct
  type ('a, 'b) either = Left of 'a | Right of 'b
  let const (x : 'a) (y : 'b) : 'a = x
  let id (x : 'a) = x
  exception TODO of string
end