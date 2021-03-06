(* Ian Treyball      <ict2102@columbia.edu>
 * Lalka Rieger      <ler2161@columbia.edu>
 * Chengtian Xu      <cx2168@columbia.edu>
 * David Han         <dth2126@columbia.edu> *)

(* Abstract Syntax Tree and functions for printing it *)

open Prelude.Prelude
open RegExp

(* Binary operators *)
type bop = BAdd | BSub | BMult | BDiv | BEqual | BNeq | BLess | BLeq | BGreater | BGeq | BAnd | BOr
         (* | BREEqual (* Structural equality of RegExp *) *)
         | BREUnion  | BREConcat  | BREMatches  | BREIntersect
         | BDFAUnion | BDFAConcat | BDFAAccepts | BDFASimulates

(* Unary operators *)
type uop = UNeg | UNot | URELit | UREStar | UREComp (* | UREOut *)

type top = Tick

(* Types within the Expressio language *)
type typ = TInt | TBool | TChar | TUnit | TString | TDFA | TRE

type bind = typ * string

(* transition function *)
type tranf = int * char * int


type expr =
    IntLit    of int
  | BoolLit   of bool
  | CharLit   of char
  | StringLit of string
  | RE        of char RegExp.regexp
  | Id        of string
  | Binop     of expr * bop * expr
  | Unop      of uop * expr
  | Assign    of string * expr
  | Call      of string * expr list
  | DFA       of expr * char list * expr * int list * tranf list
  | StringIndex of string * expr
  | Ternary   of string * expr * expr * top
  | Noexpr

type stmt =
    Block   of stmt list
  | Expr    of expr
  | Return  of expr
  | If      of expr * stmt * stmt
  | For     of expr * expr * expr * stmt
  | Infloop of stmt
  | While   of expr * stmt
  (* | Case    of expr * ((expr * expr) list) *)
  | Case    of expr * (((expr * expr) * expr) list)
  | Continue
  | Break


type func_decl = {
    typ     : typ;
    fname   : string;
    formals : bind list;
    locals  : bind list;
    body    : stmt list;
  }


type dfa_decl = {
    dfa_name     : string;
    dfa_states   : int;
    dfa_alphabet : char list;
    dfa_start    : int;
    dfa_final    : int list;
    dfa_tranves  : tranf list;
  }

type program = bind list * dfa_decl list *  func_decl list

(* Pretty-printing functions *)

let string_of_op = function
    BAdd          -> "+"
  | BSub          -> "-"
  | BMult         -> "*"
  | BDiv          -> "/"
  | BEqual        -> "=="
  | BNeq          -> "!="
  | BLess         -> "<"
  | BLeq          -> "<="
  | BGreater      -> ">"
  | BGeq          -> ">="
  | BAnd          -> "&&"
  | BOr           -> "||"
  (* | BREEqual      -> "" (* TODO operator charactor or do string/word? *) *)
  | BREUnion      -> "|"
  | BREConcat     -> "^"
  | BREMatches    -> "matches"
  | BREIntersect  -> "&"
  | BDFAUnion     -> "union"
  | BDFAConcat    -> "concat"
  | BDFAAccepts   -> "accepts"
  | BDFASimulates -> "simulates"

let string_of_uop = function
    UNeg    -> "-"
  | UNot    -> "!"
  | URELit  -> "lit"
  | UREStar -> "**"
  | UREComp -> "'"
  (* | UREOut  -> "outer" *)

let rec string_of_clist = function
    []            -> ""
  | [last]        -> "'" ^ String.make 1 last ^ "'"
  | first :: rest -> "'" ^ String.make 1 first ^ "', " ^ string_of_clist rest ^ ""

let rec string_of_intlist = function
    []            -> ""
  | [last]        -> string_of_int last
  | first :: rest -> string_of_int first ^ ", " ^ string_of_intlist rest

let string_of_tranf (one, two, three) =
    "( " ^ string_of_int one ^ ", " ^ String.make 1 two ^ ", " ^ string_of_int three ^ " )"

let rec string_of_tlist = function
    []            -> ""
  | [last]        -> string_of_tranf last
  | first :: rest -> string_of_tranf first ^ ", " ^ string_of_tlist rest

let rec string_of_expr = function
    IntLit l            -> string_of_int l
  | RE r                -> RegExp.string_of_re r
  | BoolLit true        -> "true"
  | BoolLit false       -> "false"
  | CharLit c           -> String.make 1 c
  | StringLit s         -> s
  | Id s                -> s
  | Ternary(_,_,_,_)  -> ""
  | Binop (e1, o, e2)   -> string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  (* prefix *)
  | Unop (UNeg,    e)   -> string_of_uop UNeg    ^ string_of_expr e
  | Unop (UNot,    e)   -> string_of_uop UNot    ^ string_of_expr e
  | Unop (UREComp, e)   -> string_of_uop UREComp ^ string_of_expr e
  | Unop (URELit,  e)   -> string_of_uop URELit  ^ string_of_expr e
  (* | Unop (UREOut,  e)   -> string_of_uop UREOut  ^ string_of_expr e *)
  (* postfix *)
  | Unop (UREStar, e)   -> string_of_expr e      ^ string_of_uop UREStar
  | Assign (v, e)       -> v ^ " = " ^ string_of_expr e
  | Call (f, el)        -> f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | DFA (a, b, c, d, e) -> "{\n states : "      ^ string_of_expr a    ^
                            "\n alphabet : "    ^ string_of_clist b   ^
                            "\n start : "       ^ string_of_expr c    ^
                            "\n final : "       ^ string_of_intlist d ^
                            "\n transitions : " ^ string_of_tlist e   ^ "\n }"
  | StringIndex(_,_)      -> ""
  | Noexpr              -> ""

let rec string_of_stmt = function
    Block stmts         -> "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr expr           -> string_of_expr expr ^ ";\n";
  | Return expr         -> "return " ^ string_of_expr expr ^ ";\n";
  | If (e, s, Block []) -> "if " ^ string_of_expr e ^ "\n" ^ string_of_stmt s
  | If (e, s1, s2)      -> "if " ^ string_of_expr e ^ "\n" ^ string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For (e1, e2, e3, s) -> "for " ^ string_of_expr e1  ^ "; " ^ string_of_expr e2 ^ "; " ^ string_of_expr e3 ^ "; " ^ string_of_stmt s
  | While (e, s)        -> "for ; " ^ string_of_expr e ^ "; " ^ string_of_stmt s
  | Infloop (s)         -> "for " ^ string_of_stmt s
  (*raise (Prelude.TODO "string_of case")*)
  (* FIXME this is not correct but I'll keep it here for now while I make my own branch *)
  | Case (e, cs)        -> let cases = String.concat "\n" (List.map (fun ((x, y), z) -> "(" ^ string_of_expr x ^ ", " ^ string_of_expr y ^ ") -> " ^ string_of_expr z) cs)
                           in "case " ^ string_of_expr e ^ ":\n" ^ cases ^ (error "unfinished")
  | Break               -> "break;"
  | Continue            -> "continue;"

let string_of_typ = function
    TInt    -> "int"
  | TBool   -> "bool"
  | TChar   -> "char"
  | TUnit   -> "unit"
  | TRE     -> "regexp"
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
  "{\n states : "     ^ string_of_int     dfa.dfa_states   ^
  "\n alphabet : "    ^ string_of_clist   dfa.dfa_alphabet ^
  "\n start : "       ^ string_of_int     dfa.dfa_start    ^
  "\n final : "       ^ string_of_intlist dfa.dfa_final    ^
  "\n transitions : " ^ string_of_tlist   dfa.dfa_tranves  ^ "\n }"

let string_of_program (vars, dfas, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_ddecl dfas) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
