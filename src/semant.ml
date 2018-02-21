(* Semantic checking for the MicroC compiler *)

open Ast
open Sast

exception TODO of string

module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

let check (globals, functions) =

  (* Check if a certain kind of binding has void type or is a duplicate
     of another, previously checked binding *)
  let check_binds (kind : string) (to_check : bind list) =
    let check_it checked binding =
      let void_err = "illegal void " ^ kind ^ " " ^ snd binding
      and dup_err = "duplicate " ^ kind ^ " " ^ snd binding
      in match binding with
        (* No void bindings *)
        (TUnit, _) -> raise (Failure void_err)
      | (_, n1) -> match checked with
                    (* No duplicate bindings *)
                      ((_, n2) :: _) when n1 = n2 -> raise (Failure dup_err)
                    | _ -> binding :: checked
    in let _ = List.fold_left check_it [] (List.sort compare to_check)
       in to_check

  (**** Checking Global Variables ****)

  in let globals' = check_binds "global" globals
  (**** Checking Functions ****)

  (* Collect function declarations for built-in functions: no bodies *)
  in let built_in_decls =
    let add_bind map (name, ty) = StringMap.add name {
      typ     = TUnit;
      fname   = name;
      formals = [(ty, "x")];
      locals  = [];
      body = []
      } map
    in List.fold_left add_bind StringMap.empty [ ("print", TInt);
			                         ("printb", TBool);
			                        (* ("printf", TFloat); *)
			                         ("printbig", TInt) ]

  (* Add function name to symbol table *)
  in let add_func map fd =
    let built_in_err = "function " ^ fd.fname ^ " may not be defined"
    and dup_err = "duplicate function " ^ fd.fname
    and make_err er = raise (Failure er)
    and n = fd.fname (* Name of the function *)
    in match fd with (* No duplicate functions or redefinitions of built-ins *)
         _ when StringMap.mem n built_in_decls -> make_err built_in_err
       | _ when StringMap.mem n map -> make_err dup_err
       | _ ->  StringMap.add n fd map

  (* Collect all other function names into one symbol table *)
  in let function_decls = List.fold_left add_func built_in_decls functions

  (* Return a function from our symbol table *)
  in let find_func s =
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))


  in let _ = find_func "main" (* Ensure "main" is defined *)

  in let check_function func =
    (* Make sure no formals or locals are void or duplicates *)
    let formals' = check_binds "formal" func.formals
    in let locals' = check_binds "local" func.locals
    (* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)
    in let check_assign lvaluet rvaluet err = if lvaluet = rvaluet
                                              then lvaluet
                                              else raise (Failure err)
    (* Build local symbol table of variables for this function *)
    in let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m) StringMap.empty (globals' @ formals' @ locals' )


    (* Return a variable from our local symbol table *)
    in let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))


    (* Return a semantically-checked expression, i.e., with a type *)
    in let rec expr = function
        IntLit  l             -> (TInt, SIntLit l)
      | CharLit c             -> (TChar, SCharLit c)
      | StringLit s           -> (TString, SStringLit s)
      | BoolLit l             -> (TBool, SBoolLit l)
      | Regex _               -> raise (TODO "implement")
      | Noexpr                -> (TUnit, SNoexpr)
      | Id s                  -> (type_of_identifier s, SId s)
      | Assign (var, e) as ex -> let lt = type_of_identifier var
                                 and (rt, e') = expr e
                                 in let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^ string_of_typ rt ^ " in " ^ string_of_expr ex
                                 in (check_assign lt rt err, SAssign(var, (rt, e')))
      | UnopPre (op, e) as ex ->
          let (t, e') = expr e
          in let ty = match op with
                        UNeg when t = TInt  -> t
                      | UNot when t = TBool -> TBool
                      | _ -> raise (Failure ("illegal unary operator " ^
                                             string_of_uop op ^ string_of_typ t ^
                                             " in " ^ string_of_expr ex))
          in (ty, SUnop (op, (t, e')))
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
                        | BDiv     when same && t1 = TInt  -> TInt
                        | BEqual
                        | BNeq     when same               -> TBool
                        | BLess
                        | BLeq
                        | BGreater
                        | BGeq     when same && t1 = TInt  -> TBool
                        | BAnd
                        | BOr      when same && t1 = TBool -> TBool
                        | _ -> raise (Failure ("illegal binary operator " ^
                                               string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                                               string_of_typ t2 ^ " in " ^ string_of_expr e))
        in (ty, SBinop ((t1, e1'), op, (t2, e2')))
      | Call (fname, args) as call ->
          let fd           = find_func fname
          in let param_length = List.length fd.formals
          in if List.length args != param_length
             then raise (Failure ("expecting " ^ string_of_int param_length ^ " arguments in " ^ string_of_expr call))
             else let check_call (ft, _) e = let (et, e') = expr e
                                             in let err = "illegal argument found " ^ string_of_typ et ^ " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
                  in (check_assign ft et err, e')
          in let args' = List.map2 check_call fd.formals args
          in (fd.typ, SCall (fname, args'))
    in let check_bool_expr e = let (t', e') = expr e
                               and err = "expected Boolean expression in " ^ string_of_expr e
                               in if t' != TBool
                                  then raise (Failure err)
                                  else (t', e')


    (* Return a semantically-checked statement i.e. containing sexprs *)
    in let rec check_stmt = function
        Expr e               -> SExpr (expr e)
      | If (p, b1, b2)       -> SIf (check_bool_expr p, check_stmt b1, check_stmt b2)
      | For (e1, e2, e3, st) -> SFor (expr e1, check_bool_expr e2, expr e3, check_stmt st)
      | While (p, s)         -> SWhile (check_bool_expr p, check_stmt s)
      | Infloop (s)          -> SInfloop (check_stmt s)
      | Return e             -> let (t, e') = expr e
                                in if t = func.typ
                                   then SReturn (t, e')
                                   else raise (Failure ("return gives " ^ string_of_typ t ^ " expected " ^ string_of_typ func.typ ^ " in " ^ string_of_expr e))
	    (* A block is correct if each statement is correct and nothing
	       follows any Return statement.  Nested blocks are flattened. *)
      | Block sl ->
          let rec check_stmt_list = function
              [Return _ as s] -> [check_stmt s]
            | Return _ :: _   -> raise (Failure "nothing may follow a return")
            | Block sl :: ss  -> check_stmt_list (sl @ ss) (* Flatten blocks *)
            | s :: ss         -> check_stmt s :: check_stmt_list ss
            | []              -> []
          in SBlock (check_stmt_list sl)

    in (* body of check_function *)
    { styp     = func.typ;
      sfname   = func.fname;
      sformals = formals';
      slocals  = locals';
      sbody    = match check_stmt (Block func.body) with
                	 SBlock sl -> sl
                  | _        -> let err = "internal error: block didn't become a block?"
                                in raise (Failure err)
    }
  in (globals', List.map check_function functions)
