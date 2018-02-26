(* Abstract Syntax Tree and functions for printing it *)

open Prelude
open RegExp
open DFA

type bop = BAdd | BSub | BMult | BDiv | BEqual | BNeq | BLess | BLeq | BGreater | BGeq |
           BAnd | BOr  | BREUnion | BREConcat | BREMatches | BCase

type uop = UNeg | UNot | ULit | UStar

type typ = TInt | TBool | TChar | TUnit | TRegexp | TString | TDFA

type bind = typ * string

type tranf = int * char * int

type expr =
    IntLit    of int
  | BoolLit   of bool
  | CharLit   of char
  | StringLit of string
  | DFALit    of int DFA.t
  | Regex     of char RegExp.regexp
  | Id        of string
  | Binop     of expr * bop * expr
  | UnopPre   of uop * expr
  | UnopPost  of expr * uop
  | Assign    of string * expr
  | Call      of string * expr list
  | DFABody   of int * char list * int * int list * tranf list
  | Noexpr

type stmt =
    Block   of stmt list
  | Expr    of expr
  | Return  of expr
  | If      of expr * stmt * stmt
  | For     of expr * expr * expr * stmt
  | Infloop of stmt
  | While   of expr * stmt
  | Continue
  | Break


type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list;
    locals : bind list;
    body : stmt list;
  }


type dfa_decl = {
    dfa_name : string;
    dfa_states : int;
    dfa_alphabet: char list;
    dfa_start: int;
    dfa_final: int list;
    dfa_tranves: tranf list;
  }

type program = bind list * dfa_decl list *  func_decl list

(* Pretty-printing functions *)
(*
let rec string_of_re = function
    RegExp.Zero                -> "∅"
  | RegExp.One                 -> "ε"
  | RegExp.Lit  c              -> "(lit " ^ (String.make 1 c) ^ ")"
  | RegExp.Plus (a, b)         -> "(" ^ string_of_re a ^ "+" ^ string_of_re b ^ ")"
  | RegExp.Mult (a, b)         -> "(" ^ string_of_re a ^ "." ^ string_of_re b ^ ")"
  | RegExp.Star (RegExp.Lit c) -> (String.make 1 c) ^ "⋆"
  | RegExp.Star a              -> "(" ^ string_of_re a ^ ")⋆"
  *)

let string_of_op = function
    BAdd       -> "+"
  | BSub       -> "-"
  | BMult      -> "*"
  | BDiv       -> "/"
  | BEqual     -> "=="
  | BNeq       -> "!="
  | BLess      -> "<"
  | BLeq       -> "<="
  | BGreater   -> ">"
  | BGeq       -> ">="
  | BAnd       -> "&&"
  | BOr        -> "||"
  | BREUnion   -> "|"
  | BREConcat  -> "^"
  | BREMatches -> "matches"
  | BCase      -> "case"

let string_of_uop = function
    UNeg  -> "-"
  | UNot  -> "!"
  | ULit  -> "lit"
  | UStar -> "**"

(*
let string_of_nop = function
    NZero -> "{.}" (* TODO "{}"? *)
  | NOne  -> "(.)" (* TODO "{{}}"? *)
  *)

let rec string_of_clist = function
  []              -> ""
  | [last]        -> "'" ^ String.make 1 last ^ "'"
  | first :: rest -> "'" ^ String.make 1 first ^ "', " ^ string_of_clist rest ^ ""

let rec string_of_intlist = function
  []              -> ""
  | [last]        -> string_of_int last
  | first :: rest -> string_of_int first ^ ", " ^string_of_intlist rest

let string_of_tranf tranf =
  let (one, two, three) = tranf in
  "( " ^ string_of_int one ^ ", " ^ String.make 1 two ^ ", " ^ string_of_int three ^ " )"

let rec string_of_tlist = function
  []              -> ""
  | [last]        -> string_of_tranf last
  | first :: rest -> string_of_tranf first ^ ", " ^ string_of_tlist rest

let rec string_of_expr = function
    IntLit l          -> string_of_int l
  | Regex r           -> RegExp.string_of_re r
  | DFALit _          -> raise (Prelude.TODO "DFALit")
  | BoolLit true      -> "true"
  | BoolLit false     -> "false"
  | CharLit c         -> String.make 1 c
  | StringLit s       -> s
  | Id s              -> s
  | Binop (e1, o, e2) -> string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | UnopPre (o, e)    -> "(" ^ string_of_uop o ^ " " ^ string_of_expr e ^ ")"
  | UnopPost (e, o)   -> "(" ^ string_of_expr e ^ " " ^ string_of_uop o ^ ")"
  | Assign (v, e)     -> v ^ " = " ^ string_of_expr e
  | Call (f, el)      -> f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | DFABody(a,b,c,d,e) -> "{\n states : " ^ string_of_int a ^
  "\n alphabet : " ^ string_of_clist b ^
  "\n start : " ^ string_of_int c ^
  "\n final : " ^ string_of_intlist d ^
  "\n transitions : " ^ string_of_tlist e ^ "\n }"
  | Noexpr            -> ""

let rec string_of_stmt = function
    Block stmts         -> "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr expr           -> string_of_expr expr ^ ";\n";
  | Return expr         -> "return " ^ string_of_expr expr ^ ";\n";
  | If (e, s, Block []) -> "if " ^ string_of_expr e ^ "\n" ^ string_of_stmt s
  | If (e, s1, s2)      -> "if " ^ string_of_expr e ^ "\n" ^ string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For (e1, e2, e3, s) -> "for " ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^ string_of_expr e3 ^ "; " ^ string_of_stmt s
  | While (e, s)        -> "for ;" ^ string_of_expr e ^ "; " ^ string_of_stmt s
  | Infloop (s)         -> "for " ^ string_of_stmt s
  | Break               -> "break;"
  | Continue            -> "continue;"

let string_of_typ = function
    TInt    -> "int"
  | TBool   -> "bool"
  | TChar   -> "char"
  | TUnit   -> "unit"
  | TRegexp -> "regexp"
  | TString -> "string"
  | TDFA    -> "dfa"

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_ddecl dfa =
  dfa.dfa_name ^
  "{\n states : " ^ string_of_int dfa.dfa_states ^
  "\n alphabet : " ^ string_of_clist dfa.dfa_alphabet ^
  "\n start : " ^ string_of_int dfa.dfa_start ^
  "\n final : " ^ string_of_intlist dfa.dfa_final ^
  "\n transitions : " ^ string_of_tlist dfa.dfa_tranves ^ "\n }"

let string_of_program (vars, dfas, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_ddecl dfas) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
