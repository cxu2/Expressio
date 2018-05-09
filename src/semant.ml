(* Semantic checking for the MicroC compiler *)

open Ast
open Sast
open Prelude.Prelude

(* module StringMap = Map.Make(String) *)
(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

(* let check (globals, dfas, functions) = *)
  let check (globals, _, functions) =

  (* Check if a certain kind of binding has void type or is a duplicate
     of another, previously checked binding *)
  let check_binds (kind : string) (to_check : bind list) : bind list =
    let check_it (checked : bind list) (binding : bind) =
      match binding with
        (* No void bindings *)
        (TUnit,  _) -> error ("illegal void " ^ kind ^ " " ^ snd binding)
      | (_,     n1) -> match checked with
                        (* No duplicate bindings *)
                        ((_, n2) :: _) when n1 = n2 -> error ("duplicate " ^ kind ^ " " ^ snd binding)
                       | _                          -> binding :: checked
    in let _ = List.fold_left check_it [] (List.sort compare to_check)
       in to_check

  (**** Checking Global Variables ****)

  in let globals' : bind list = check_binds "global" globals
  in let dfas' : sdfa_decl list = [] (* check_binds "dfa" dfas *)
  (**** Checking Functions ****)

  (* Collect function declarations for built-in functions: no bodies *)
  in let built_in_decls : func_decl string_map =
    let add_bind ((rt, ty, name)) : (string * func_decl) = (name, { typ     = rt
                                                                         ; fname   = name
                                                                         ; formals = [(ty, "x")]
                                                                         ; locals  = []
                                                                         ; body    = []
                                                                         })
    in let built_ins : (string * func_decl) list = List.map add_bind [(TUnit, TInt,    "print");
                                                                      (TUnit, TRE,     "printr"); 
                                                                      (TUnit, TDFA,    "printdfa"); 
                                                                      (TUnit, TString, "printf");
                                                                      (TChar, TRE,     "litchar");
                                                                      (TUnit, TBool,   "printb");
                                                                      (TInt,  TString, "len") ]
    in let built_ins : (string * func_decl) list = ("matches", { typ = TBool
                                                                ; fname = "matches"
                                                                ; formals = [(TString, "x") ; (TRE, "y")]
                                                                ; locals = []
                                                                ; body = []
                                                                }) :: built_ins
    in let built_ins : (string * func_decl) list = ("link", { typ = TInt
                                                            ; fname = "link"
                                                            ; formals = [(TDFA, "x") ; (TInt, "y"); (TChar, "z"); (TInt, "w")]
                                                            ; locals = []
                                                            ; body = []
                                                            }) :: built_ins
    in fromList (built_ins)


  (* Add function name to symbol table *)
  in let add_func map fd = if      StringMap.mem fd.fname built_in_decls
                           then      error ("function "           ^ fd.fname ^ " may not be defined")
                           else if StringMap.mem fd.fname map
                                then error ("duplicate function " ^ fd.fname)
                                else StringMap.add fd.fname fd map

  (* Collect all other function names into one symbol table *)
  in let function_decls : func_decl string_map = List.fold_left add_func built_in_decls functions

  (* Return a function from our symbol table *)
  in let find_func s = match StringMap.find_opt s function_decls with
        Some s' -> s'
      | None    -> error ("unrecognized function " ^ s)


  in let _ = find_func "main" (* Ensure "main" is defined *)

  in let check_function (func : func_decl) : sfunc_decl =
    (* Make sure no formals or locals are void or duplicates *)
    let formals' : bind list = check_binds "formal" func.formals
    and locals'  : bind list = check_binds "local"  func.locals
    (* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)
    in let check_assign lvaluet rvaluet err = if lvaluet = rvaluet
                                              then lvaluet
                                              else error err
    (* Build local symbol table of variables for this function *)
    in let bindings : bind list = (globals' @ formals' @ locals')
    in let symbols : Ast.typ string_map = fromList (List.map swap bindings)

    (* Return a variable from our local symbol table *)
    in let type_of_identifier s = match StringMap.find_opt s symbols with
        Some s' -> s'
      | None    -> error ("undeclared identifier " ^ s)



    (* Return a semantically-checked expression, i.e., with a type *)
    (* in let rec expr = function *)
    in let rec expr (ex : expr) : sexpr = match ex with
        IntLit  l                                 -> (TInt,    SIntLit l)
      | CharLit c                                 -> (TChar,   SCharLit c)
      | StringLit s                               -> (TString, SStringLit s)
      | BoolLit l                                 -> (TBool,   SBoolLit l)
      | DFA (states, alpha, start, final, tran)   ->
                                 (* check states is greater than final states *)
                                 let rec checkFinal maxVal = function
                                   []                       -> false
                                 | x :: _  when maxVal <= x -> true
                                 | _ :: tl                  -> checkFinal maxVal tl

                                 (* check states is greater than start/final in transition *)
                                 and checkTran maxVal = function
                                   []                                  -> false
                                 | (t1, _,  _) :: _  when maxVal <= t1 -> true
                                 | (_,  _, t3) :: _  when maxVal <= t3 -> true
                                 | (_,  _,  _) :: tl                   -> checkTran maxVal tl

                                 (* check transition table has one to one *)
                                 and oneToOne sMap = function
                                   []                 -> false
                                 | (t1, t2, t3) :: tl -> let    combo      = string_of_int t1 ^ String.make 1 t2
                                                         in let finalState = string_of_int t3
                                                         in
                                                         if StringMap.mem combo sMap then true else oneToOne (StringMap.add combo finalState sMap) tl in

                                 (* also check that states is greater than start *)
                                 if states <= start ||  checkFinal states final || checkTran states tran || oneToOne StringMap.empty tran
                                 then error "DFA invalid"
                                 else (TDFA, SDFA (states, alpha, start, final, tran))
      | RE r                  -> (* let check = raise (Prelude.TODO "implement any needed checking here")
                                 in*) (TRE, SRE r)
      | Noexpr                -> (TUnit, SNoexpr)
      | Id s                  -> (type_of_identifier s, SId s)
      | Assign (var, e) as ex -> let lt = type_of_identifier var
                                 and (rt, e') = expr e
                                 in let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^ string_of_typ rt ^ " in " ^ string_of_expr ex
                                 in (check_assign lt rt err, SAssign (var, (rt, e')))
      | Unop (UNeg,    e) when fst (expr e) = TInt  -> (TInt,  SUnop (UNeg, expr e))
      | Unop (UNeg,    e) as ex                     -> error ("illegal unary operator " ^ string_of_uop UNeg    ^ string_of_typ (fst (expr e)) ^ " in " ^ string_of_expr ex)
      | Unop (UNot,    e) when fst (expr e) = TBool -> (TBool, SUnop (UNot,    expr e))
      | Unop (UNot,    e) as ex                     -> error ("illegal unary operator " ^ string_of_uop UNot    ^ string_of_typ (fst (expr e)) ^ " in " ^ string_of_expr ex)
      | Unop (URELit,  e) when fst (expr e) = TChar -> (TRE,   SUnop (URELit,  expr e))
      | Unop (URELit,  e) as ex                     -> error ("illegal unary operator " ^ string_of_uop URELit  ^ string_of_typ (fst (expr e)) ^ " in " ^ string_of_expr ex)
      | Unop (UREStar, e) when fst (expr e) = TRE   -> (TRE,   SUnop (UREStar, expr e))
      | Unop (UREStar, e) as ex                     -> error ("illegal unary operator " ^ string_of_uop UREStar ^ string_of_typ (fst (expr e)) ^ " in " ^ string_of_expr ex)
      | Unop (UREComp, e) when fst (expr e) = TRE   -> (TRE,   SUnop (UREComp, expr e))
      | Unop (UREComp, e) as ex                     -> error ("illegal unary operator " ^ string_of_uop UREComp ^ string_of_typ (fst (expr e)) ^ " in " ^ string_of_expr ex)
      | Binop (e1, op, e2) as e ->
          let (t1, e1') = expr e1
          and (t2, e2') = expr e2
          (* All binary operators require operands of the same type *)
          in let same = t1 = t2
          (* Determine expression type based on operator and operand types *)
          in let ty = match op with
                          BAdd
                        | BSub
                        | BMult
                        | BDiv          when same      && t1 = TInt    -> TInt
                        | BEqual
                        | BNeq          when same                      -> TBool
                        | BLess
                        | BLeq
                        | BGreater
                        | BGeq          when same      && t1 = TInt    -> TBool
                        | BAnd
                        | BOr           when same      && t1 = TBool   -> TBool
                        | BREUnion
                        | BREConcat
                        | BREIntersect  when same      && t1 = TRE     -> TRE
                        | BREMatches    when t1 = TRE  && t2 = TString -> TBool
                        | BDFAAccepts   when t1 = TDFA && t2 = TString -> TBool
                        | BDFASimulates when t1 = TDFA && t2 = TString -> TInt
                        | BDFAUnion     when t1 = TDFA && t2 = TDFA    -> TDFA
                        | BDFAConcat    when t1 = TDFA && t2 = TDFA    -> TDFA
                        | BCase        -> raise (TODO "implement BCase in semant")
                        | _ -> error ("illegal binary operator " ^
                                      string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                                      string_of_typ t2 ^ " in " ^ string_of_expr e)
        in (ty, SBinop ((t1, e1'), op, (t2, e2')))
      | Call (fname, args) as call ->
          let fd              = find_func fname
          in let param_length = List.length fd.formals
          in if List.length args != param_length
             then error ("expecting " ^ string_of_int param_length ^ " arguments in " ^ string_of_expr call)
             else let check_call (ft, _) e = let (et, e') = expr e
                                             in let err = "illegal argument found " ^ string_of_typ et ^ " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
                  in (check_assign ft et err, e')
          in let args' = List.map2 check_call fd.formals args
          in (fd.typ, SCall (fname, args'))

    in let check_bool_expr e = match (expr e) with
                                (TBool, e') -> (TBool, e')
                              | _           -> error ("expected Boolean expression in " ^ string_of_expr e)


    (* Return a semantically-checked statement i.e. containing sexprs *)
    (* this function was originally a simple `stmt -> sstmt` but with adding continue/break statements it is
       not possible to take any arbitrary `stmt` without more context to determine if said statement is semantically correct,
       so here we pass along some context/state, namely a boolean value which is true if we're in a loop
       false otherwise. *)
    in let rec check_statement (x : (bool * stmt)) : (bool * sstmt) = match x with
      | (false,   Break)                                                 -> error "\'break\' is outside of loop"
      | (false,   Continue)                                              -> error "\'continue\' is outside of loop"
      | (true,    Break)                                                 -> (true,    SBreak)
      | (true,    Continue)                                              -> (true,    SContinue)
      | (looping, Expr e)                                                -> (looping, SExpr (expr e))
      | (looping, If (p, b1, b2))                                        -> (looping, SIf (check_bool_expr p, snd (check_statement (looping, b1)), snd (check_statement (looping, b2))))
      | (looping, For (e1, e2, e3, s))                                   -> (looping, SFor     (expr e1, check_bool_expr e2, expr e3, snd (check_statement (true, s))))
      | (looping, While (p, s))                                          -> (looping, SWhile   (SNostmt, check_bool_expr p,           snd (check_statement (true, s))))
      | (_,       Infloop s)                                             -> (true,    SInfloop (                                      snd (check_statement (true, s))))
      | (looping, Return e)               when (fst (expr e) = func.typ) -> (looping, SReturn (expr e))
      | (_,       Return e)                                              -> error ("return gives " ^ string_of_typ (fst (expr e)) ^ " expected " ^ string_of_typ func.typ ^ " in " ^ string_of_expr e)
      | (looping, Block (Return e :: [])) when (fst (expr e) = func.typ) -> (looping, SBlock [SReturn (expr e)])
      | (_,       Block (Return e :: []))                                -> error ("return gives " ^ string_of_typ (fst (expr e)) ^ " expected " ^ string_of_typ func.typ ^ " in " ^ string_of_expr e)
      | (_,       Block (Return _ ::  _))                                -> error "nothing may follow a return"
      | (looping, Block (Block sl :: ss))                                -> (looping, snd (check_statement (looping, (Block (sl @ ss)))))           (* Flatten blocks *)
      | (looping, Block              [])                                 -> (looping, SBlock [])
      | (looping, Block              ss)                                 -> let ss' : sstmt list = snd (map_accum_left (curry check_statement) looping ss)
                                                                            in  (looping, SBlock (ss'))
    in (* body of check_function *)
    { styp     = func.typ;
      sfname   = func.fname;
      sformals = formals';
      slocals  = locals';
      sbody    = match (snd (check_statement (false, (Block func.body)))) with (* if we ever decide to support nested functions we should update check_function to pass a boolean variable to indicate the state of looping and include it here instead of `false` *)
                	 SBlock sl -> sl
                  | _        -> error "internal error: block didn't become a block?"
    }
  in (globals', dfas', List.map check_function functions)
