(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast
open Prelude.Prelude
open RegExp

type sexpr = typ * sx
and sx =
    SIntLit    of int
  | SCharLit   of char
  | SStringLit of string
  | SRE        of char RegExp.regexp
  | SBoolLit   of bool
  | SId        of string
  | SBinop     of sexpr * bop * sexpr
  | SUnop      of uop * sexpr
  | SAssign    of string * sexpr
  | SCall      of string * sexpr list
  | SDFA       of int * char list * int * int list * tranf list
  | SNoexpr

type sstmt =
    SBlock   of sstmt list
  | SExpr    of sexpr
  | SReturn  of sexpr
  | SIf      of sexpr * sstmt * sstmt
  | SCase    of expr * ((expr * expr) list) (* TODO might delete this if SIf is sufficient *)
  | SFor     of sexpr * sexpr * sexpr * sstmt
  | SWhile   of sstmt * sexpr * sstmt
  | SInfloop of sstmt
  | SContinue
  | SBreak
  | SNostmt

  (* type stmt =
      Block   of stmt list
    | Expr    of expr
    | Return  of expr
    | If      of expr * stmt * stmt
    | For     of expr * expr * expr * stmt
    | Infloop of stmt
    | While   of expr * stmt
    | Continue
    | Break *)


(*
  evaluate a semantically checked int expression by interpreting it
   N.B. this assumes the semantic checking on `s` was done correctly.
   TODO: if I were not under a time crunch, I would explore the idea of implementing this with map_accum_left :)
*)
let rec eval_sint (map : int string_map) (s : sx) : (int string_map * int) = match s with
    SIntLit i                        -> (map, i)
  | SId x                            -> (match (StringMap.find_opt x map) with
                                           Some z -> (map, z)
                                         | None -> error "internal error: var identifier look up failed for int expression")
  | SAssign (str, (_, sx))           -> let (map', rhs) = eval_sint map sx
                                        in eval_sint (StringMap.add str rhs map') sx
  | SBinop ((_, e1), BAdd,  (_, e2)) -> let    (map',  left)  = eval_sint map  e1
                                        in let (map'', right) = eval_sint map' e2
                                        in (map'', left + right)
  | SBinop ((_, e1), BSub,  (_, e2)) -> let    (map',  left)  = eval_sint map  e1
                                        in let (map'', right) = eval_sint map' e2
                                        in (map'', left - right)
  | SBinop ((_, e1), BMult, (_, e2)) -> let    (map',  left)  = eval_sint map  e1
                                        in let (map'', right) = eval_sint map' e2
                                        in (map'', left * right)
  | SBinop ((_, e1), BDiv,  (_, e2)) -> let    (map',  left)  = eval_sint map  e1
                                        in let (map'', right) = eval_sint map' e2
                                        in (map'', left / right)
  | SBinop (_,          _,        _) -> error "internal error, unknown bop used in int expr"
  | SUnop (UNeg, (_, sx))            -> let (map', x) = eval_sint map sx
                                        in (map', (-x))
  | _                                -> error "internal error" (* TODO *)
  (*
  | SCall _                          -> error "call"
  | SCharLit _                       -> raise ABSURD
  | SStringLit _                     -> raise ABSURD
  | SRE        _                     -> raise ABSURD
  | SBoolLit   _                     -> raise ABSURD
  | SDFA       _                     -> raise ABSURD
  | SNoexpr                          -> error "noexpr"
  *)


type sfunc_decl = {
  styp     : typ;
  sfname   : string;
  sformals : bind list;
  slocals  : bind list;
  sbody    : sstmt list;
}

  (* type func_decl = {
      typ : typ;
      fname : string;
      formals : bind list;
      locals : bind list;
      body : stmt list;
    } *)


type sdfa_decl = {
  sdfa_name     : string;
  sdfa_states   : int;
  sdfa_alphabet : char list;
  sdfa_start    : int;
  sdfa_final    : int list;
  sdfa_tranves  : tranf list;
}
  (* type dfa_decl = {
      dfa_name : string;
      dfa_states : int;
      dfa_alphabet: char list;
      dfa_start: int;
      dfa_final: int list;
      dfa_tranves: tranf list;
    } *)

type sprogram = bind list * sdfa_decl list * sfunc_decl list
(* type program = bind list * dfa_decl list *  func_decl list *)

(* Pretty-printing functions *)

(* let rec string_of_expr = function
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
  | Noexpr            -> "" *)


let rec string_of_sexpr (t, e) =
  "(" ^ string_of_typ t ^ " : " ^ string_of_sx e ^ ")"
and     string_of_sx = function
    SIntLit i            -> string_of_int i
  | SRE r                -> RegExp.string_of_re r
  | SBoolLit true        -> string_of_expr (BoolLit true)
  | SBoolLit false       -> string_of_expr (BoolLit false)
  | SCharLit c           -> string_of_expr (CharLit c)
  | SStringLit s         -> s
  | SId s                -> s
  | SBinop (e1, o, e2)   -> string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
  (* prefix *)
  | SUnop (UNeg,    e)   -> string_of_uop UNeg    ^ string_of_sexpr e
  | SUnop (UNot,    e)   -> string_of_uop UNot    ^ string_of_sexpr e
  | SUnop (UREComp, e)   -> string_of_uop UREComp ^ string_of_sexpr e
  | SUnop (URELit,  e)   -> string_of_uop URELit  ^ string_of_sexpr e
  (* postfix *)
  | SUnop (UREStar, e)   -> string_of_sexpr e     ^ string_of_uop UREStar
  | SAssign (v, e)       -> v ^ " = " ^ string_of_sexpr e
  | SCall (f, el)        -> f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
  | SDFA (a, b, c, d, e) -> "{\n states : "     ^ string_of_int a     ^
                            "\n alphabet : "    ^ string_of_clist b   ^
                            "\n start : "       ^ string_of_int c     ^
                            "\n final : "       ^ string_of_intlist d ^
                            "\n transitions : " ^ string_of_tlist e   ^ "\n }"
  | SNoexpr            -> ""

(* let rec string_of_stmt = function
   Block stmts         -> "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
 | Expr expr           -> string_of_expr expr ^ ";\n";
 | Return expr         -> "return " ^ string_of_expr expr ^ ";\n";
 | If (e, s, Block []) -> "if " ^ string_of_expr e ^ "\n" ^ string_of_stmt s
 | If (e, s1, s2)      -> "if " ^ string_of_expr e ^ "\n" ^ string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
 | For (e1, e2, e3, s) -> "for " ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^ string_of_expr e3 ^ "; " ^ string_of_stmt s
 | While (e, s)        -> "for ;" ^ string_of_expr e ^ "; " ^ string_of_stmt s
 | Infloop (s)         -> "for " ^ string_of_stmt s
 | Break               -> "break;"
 | Continue            -> "continue;" *)
let rec string_of_sstmt = function
    SBlock stmts          -> "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SExpr expr            -> string_of_sexpr expr ^ ";\n";
  | SReturn expr          -> "return " ^ string_of_sexpr expr ^ ";\n";
  | SIf (e, s, SBlock []) -> "if " ^ string_of_sexpr e ^ "\n" ^ string_of_sstmt s
  | SIf (e, s1, s2)       -> "if " ^ string_of_sexpr e ^ "\n" ^ string_of_sstmt s1 ^ "else\n" ^ string_of_sstmt s2
  | SFor (e1, e2, e3, s)  -> "for " ^ string_of_sexpr e1  ^ " ; " ^ string_of_sexpr e2 ^ " ; " ^ string_of_sexpr e3  ^ " " ^ string_of_sstmt s
  | SWhile (_, e, s)      -> "for " ^ string_of_sexpr e ^ " " ^ string_of_sstmt s
  | SInfloop (s)          -> "for " ^ string_of_sstmt s
  | SCase (e, cases)      -> string_of_stmt (Case (e, cases))
  | SBreak                -> string_of_stmt Break
  | SContinue             -> string_of_stmt Continue
  | SNostmt               -> ""

let string_of_sfdecl fdecl =
  string_of_typ fdecl.styp ^ " " ^
  fdecl.sfname ^ "(" ^ String.concat ", " (List.map snd fdecl.sformals) ^ ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.slocals) ^
  String.concat "" (List.map string_of_sstmt fdecl.sbody) ^
  "}\n"

let string_of_sddecl dfa =
  dfa.sdfa_name ^
  "{\n states : "     ^ string_of_int dfa.sdfa_states     ^
  "\n alphabet : "    ^ string_of_clist dfa.sdfa_alphabet ^
  "\n start : "       ^ string_of_int dfa.sdfa_start      ^
  "\n final : "       ^ string_of_intlist dfa.sdfa_final  ^
  "\n transitions : " ^ string_of_tlist dfa.sdfa_tranves  ^ "\n }"

  (* let string_of_program (vars, dfas, funcs) =
    String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
    String.concat "\n" (List.map string_of_ddecl dfas) ^ "\n" ^
    String.concat "\n" (List.map string_of_fdecl funcs) *)

let string_of_sprogram (vars, dfas, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_sddecl dfas) ^ "\n" ^
  String.concat "\n" (List.map string_of_sfdecl funcs)
