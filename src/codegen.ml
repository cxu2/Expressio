(*
 * File: codegen.ml
 * Date: 2018-03-26
 *
 * PLT Spring 2018
 * Expressio Project
 * Ian Treyball      <ict2102@columbia.edu>
 * Lalka Rieger      <ler2161@columbia.edu>
 * Chengtian Xu      <cx2168@columbia.edu>
 * David Han         <dth2126@columbia.edu>
 *)


(* Code generation: translate takes a semantically checked AST and
produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/

*)

(* We'll refer to Llvm and Ast constructs with module names *)
module L = Llvm
module A = Ast
open Sast
open Prelude
(* open Exceptions *)

module StringMap = Map.Make(String)

(* Code Generation from the SAST. Returns an LLVM module if successful,
   throws an exception if something is wrong. *)

(* let translate (globals, dfas, functions) = *)
let translate (globals, _, functions) =
  let context    = L.global_context ()
   (* Add types to the context so we can use them in our LLVM code *)

  in let i32_t      = L.i32_type    context
     and i8_t       = L.i8_type     context
     and i1_t       = L.i1_type     context
     and void_t     = L.void_type   context
     and pointer_t  = L.pointer_type
    (* Create an LLVM module -- this is a "container" into which we'll
     generate actual code *)
     and the_module = L.create_module context "Expressio"

  (* Tree named struct definition for regexp *)
  in let tree_t = L.struct_type context [| i8_t; i8_t; (pointer_t i8_t); (pointer_t i8_t) |]

  in let dfa_t =
      let types = Array.of_list [i32_t; L.pointer_type i8_t; i32_t; i32_t; L.pointer_type i32_t; i32_t; L.pointer_type (L.pointer_type i32_t)] in
      L.struct_type context types


  (**************************
   * Ast type to LLVM type  *
   **************************)


  in let ltype_of_typ = function
      A.TInt    -> i32_t
    | A.TBool   -> i1_t
    | A.TChar   -> i8_t
    | A.TUnit   -> void_t
    | A.TRE -> tree_t
    | A.TString -> L.pointer_type i8_t
    | A.TDFA    -> dfa_t

  in let arr_ptr a b = L.build_in_bounds_gep a [| L.const_int i32_t 0;  L.const_int i32_t 0|] "arr" b
        (*L.build_bitcast id_ptr (pointer_t i8_t) "mat_ptr" b*)

  (* Declare each global variable; remember its value in a map *)
  in let global_vars =
    let global_var m (t, n) =
      let init = L.const_int (ltype_of_typ t) 0
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in



  (***********************
   * Built-in Functions  *
   ***********************)



  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |]
  in let printf_func = L.declare_function "printf" printf_t the_module

  in let printr_t = L.function_type i32_t [| L.pointer_type tree_t |]
  in let printr_func = L.declare_function "printr" printr_t the_module

  in let matches_t = L.function_type i1_t [| L.pointer_type i8_t; L.pointer_type tree_t |]
  in let matches_func = L.declare_function "matches" matches_t the_module

  in let printdfa_t = L.function_type i32_t [| dfa_t |]
  in let printdfa_func = L.declare_function "printdfa" printdfa_t the_module in



  (**********************
   *   Build Functions  *
   **********************)



  (* Define each function (arguments and return type) so we can
   * define it's body and call it later *)

  let function_decls =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types = Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals)
      in let ftype = L.function_type (ltype_of_typ fdecl.styp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in

  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.sfname function_decls
    in let builder = L.builder_at_end context (L.entry_block the_function)

    in let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
    in let string_format_str = L.build_global_stringptr "%s\n" "fmt" builder

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    in let local_vars =
      let add_formal m (t, n) p =
        let () = L.set_value_name n p
        in let local = L.build_alloca (ltype_of_typ t) n builder
        in let _  = L.build_store p local builder in StringMap.add n local m


      (* Allocate space for any locally declared variables and add the
       * resulting registers to our map *)
    in let add_local m (t, n) =
    	let local_var = L.build_alloca (ltype_of_typ t) n builder
    	in StringMap.add n local_var m
      in let formals = List.fold_left2 add_formal StringMap.empty fdecl.sformals
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals fdecl.slocals
    in



    (*************************
     *    Helper Functions   *
     *************************)



    (* Return the value for a variable or formal argument. First check
     * locals, then globals *)

    let lookup n = try StringMap.find n local_vars
                   with Not_found -> try StringMap.find n global_vars
                                     with Not_found -> raise(Exceptions.GlobalVarNotFound("unknown variable name: "^n))

    in let itol n = L.const_int i32_t n

    in let get_ptr v b =
      let val_ptr = L.build_alloca (L.type_of v) "val_ptr" b in
      ignore(L.build_store v val_ptr b);
      val_ptr


    in let build_lit character name b =
      (* TODO alloc or malloc? *)
      let tree_ptr = L.build_alloca tree_t name b in

      let char_ptr = L.build_in_bounds_gep tree_ptr [| itol 0; itol 1 |] "char_ptr" b in

      ignore(L.build_store character char_ptr b);

      let tree_loaded = L.build_load tree_ptr "tree_loaded" b in
      tree_loaded


    in let build_unop op regexp name b =
      let tree_ptr = L.build_alloca tree_t name b in

      let operator_ptr = L.build_in_bounds_gep tree_ptr [| itol 0; itol 0 |] "operator_ptr" b in

      let left_ptr = L.build_in_bounds_gep tree_ptr [| itol 0; itol 2 |] "left_ptr" b in
      let left_tree_ptr = L.build_bitcast left_ptr (pointer_t tree_t) "left_tree_ptr" b in

      ignore(L.build_store (L.const_int i8_t (int_of_char op)) operator_ptr b);
      ignore(L.build_store regexp left_tree_ptr b);
      let tree_loaded = L.build_load tree_ptr "tree_loaded" b in
      tree_loaded


    in let build_binop op lregexp rregexp name b =
      let tree_ptr = L.build_alloca tree_t "lit_space" b in

      let operator_ptr = L.build_in_bounds_gep tree_ptr [| itol 0; itol 0 |] "operator_ptr" b in

      let left_ptr_ptr = L.build_in_bounds_gep tree_ptr [| itol 0; itol 2 |] "left_ptr_ptr" b in
      let left_ptr = L.build_load left_ptr_ptr "left_ptr" b in
      let left_tree_ptr = get_ptr lregexp b in
      let left_tree_char_ptr = L.build_in_bounds_gep left_tree_ptr [| itol 0; itol 0 |] "left_tree_char_ptr" b in
      ignore(L.build_store (L.build_load left_tree_char_ptr "tmp" b) left_ptr b);

      let right_ptr_ptr = L.build_in_bounds_gep tree_ptr [| itol 0; itol 3 |] "right_ptr_ptr" b in
      let right_ptr = L.build_load right_ptr_ptr "right_ptr" b in
      let right_tree_ptr = get_ptr rregexp b in
      let right_tree_char_ptr = L.build_in_bounds_gep right_tree_ptr [| itol 0; itol 0 |] "right_tree_char_ptr" b in
      ignore(L.build_store (L.build_load right_tree_char_ptr "tmp" b) right_ptr b);

      (* let right_tree_ptr = L.build_bitcast right_ptr (pointer_t tree_t) "right_tree_ptr" b in
      let right_tree_ptr = L.build_alloca tree_t "rtree_ptr" b in *)

      ignore(L.build_store (L.const_int i8_t (int_of_char op)) operator_ptr b);
(*       ignore(L.build_store lregexp left_tree_ptr b);
      ignore(L.build_store rregexp right_tree_ptr b); *)
      let tree_loaded = L.build_load tree_ptr "tree_loaded" b in
      tree_loaded
    in



    (*************************
     *   Expression Builder  *
     *************************)



    (* Construct code for an expression; return its value *)
    let rec expr builder ((_, e) : sexpr) = match e with
	      SIntLit i          -> L.const_int i32_t i
      | SBoolLit b          -> L.const_int i1_t (if b then 1 else 0)
      | SCharLit c          -> L.const_int i8_t (int_of_char c)
      | SStringLit s        -> L.build_global_stringptr s "string" builder
      | SNoexpr             -> L.const_int i32_t 0
      | SId s                           -> L.build_load (lookup s) s builder
      | SRE s                           -> raise (Prelude.TODO "implement SRE")
      | SBinop (e1, A.BAdd,         e2) -> L.build_add (expr builder e1) (expr builder e2) "tmp" builder
      | SBinop (e1, A.BSub,         e2) -> L.build_sub (expr builder e1) (expr builder e2) "tmp" builder
      | SBinop (e1, A.BMult,        e2) -> L.build_mul (expr builder e1) (expr builder e2) "tmp" builder
      | SBinop (e1, A.BDiv,         e2) -> L.build_sdiv (expr builder e1) (expr builder e2) "tmp" builder
      | SBinop (e1, A.BAnd,         e2) -> L.build_and (expr builder e1) (expr builder e2) "tmp" builder
      | SBinop (e1, A.BOr,          e2) -> L.build_or (expr builder e1) (expr builder e2) "tmp" builder
      | SBinop (e1, A.BEqual,       e2) -> L.build_icmp L.Icmp.Eq (expr builder e1) (expr builder e2) "tmp" builder
      | SBinop (e1, A.BNeq,         e2) -> L.build_icmp L.Icmp.Ne (expr builder e1) (expr builder e2) "tmp" builder
      | SBinop (e1, A.BLess,        e2) -> L.build_icmp L.Icmp.Slt (expr builder e1) (expr builder e2) "tmp" builder
      | SBinop (e1, A.BLeq,         e2) -> L.build_icmp L.Icmp.Sle (expr builder e1) (expr builder e2) "tmp" builder
      | SBinop (e1, A.BGreater,     e2) -> L.build_icmp L.Icmp.Sgt (expr builder e1) (expr builder e2) "tmp" builder
      | SBinop (e1, A.BGeq,         e2) -> L.build_icmp L.Icmp.Sge (expr builder e1) (expr builder e2) "tmp" builder
      | SBinop (e1, A.BCase,        e2) -> raise (Prelude.TODO "implement")
      | SBinop (e1, A.BREUnion,     e2) -> build_binop '|' (expr builder e1) (expr builder e2) "tmp" builder
      | SBinop (e1, A.BREConcat,    e2) -> build_binop '^' (expr builder e1) (expr builder e2) "tmp" builder
      | SBinop (e1, A.BREIntersect, e2) -> build_binop '&' (expr builder e1) (expr builder e2) "tmp" builder
      | SBinop (e1, A.BREMatches,   e2) -> L.build_call matches_func [| (expr builder e1) ; (expr builder e2) |] "matches" builder
      | SUnop (A.UNeg,    e)            -> L.build_neg (expr builder e) "tmp" builder
      | SUnop (A.UNot,    e)            -> L.build_not (expr builder e) "tmp" builder
      | SUnop (A.URELit,  e)            -> build_lit (expr builder e) "tmp" builder
      | SUnop (A.UREComp, e)            -> build_unop '\\' (expr builder e)  "tmp" builder
      | SUnop (A.UREStar, e)            -> build_unop '*' (expr builder e) "tmp" builder
      | SAssign (s, e)          -> let e' = expr builder e in
                                   let _  = L.build_store e' (lookup s) builder in e'
      | SDFA (n, a, s, f, delta) ->    let ns = L.const_int i32_t n
                                          and start = L.const_int i32_t s
                                          and nsym = L.const_int i32_t (List.length a)
                                          and nfin = L.const_int i32_t (List.length f) in
                                            let alpha_t = L.array_type i8_t (List.length a)
                                            and fin_t = L.array_type i32_t (List.length f) in
                                              let alpha = L.build_array_alloca alpha_t nsym "alpha" builder
                                              and fin = L.build_array_alloca fin_t nfin "fin" builder
                                              and d = L.build_alloca (L.pointer_type i32_t) "delta" builder in
                                                let ll_of_char c  = L.const_int i8_t (int_of_char c)
                                                and ll_of_int fint = L.const_int i32_t fint in
                                                  let ll_of_char_array = List.map ll_of_char a
                                                  and ll_of_int_array =  List.map ll_of_int f in
                                                    let copy_list_to_array (arr, i) value = ((L.build_insertvalue arr value i ("loaded"^(string_of_int i)) builder), i + 1) in
                                                    let alpha_filled = List.fold_left copy_list_to_array (L.build_load alpha "alpha0" builder , 0) ll_of_char_array
                                                    and fin_filled = List.fold_left copy_list_to_array (L.build_load fin "fin0" builder, 0) ll_of_int_array in
                                          let dfa1 = L.build_alloca dfa_t "dfa" builder in
                                          let dfa_loaded = L.build_load dfa1 "dfa_loaded" builder in
                                          let dfa_loaded2 = L.build_insertvalue dfa_loaded ns 0 "dfa_loaded2" builder in
                                          let dfa_loaded3 = L.build_insertvalue dfa_loaded2 (arr_ptr alpha builder) 1 "dfa_loaded3" builder in
                                          let dfa_loaded4 = L.build_insertvalue dfa_loaded3 nsym 2 "dfa_loaded4" builder in
                                          let dfa_loaded5 = L.build_insertvalue dfa_loaded4 start 3 "dfa_loaded5" builder in
                                          let dfa_loaded6 = L.build_insertvalue dfa_loaded5 (arr_ptr fin builder) 4 "dfa_loaded6" builder in
                                          let dfa_loaded7 = L.build_insertvalue dfa_loaded6 nfin 5 "dfa_loaded7" builder in
                                          L.build_insertvalue dfa_loaded7 d 6 "dfa_loaded8" builder
      | SCall ("printb",   [e]) -> L.build_call printf_func   [| int_format_str ; (expr builder e) |]   "printf"   builder
      | SCall ("printdfa", [e]) -> L.build_call printdfa_func   [|(expr builder e) |]   "printf"   builder
      | SCall ("printf",   [e]) -> L.build_call printf_func   [| string_format_str ; (expr builder e) |] "printf"   builder
      | SCall ("printr",   [e]) -> L.build_call printr_func   [| get_ptr (expr builder e) builder |] "printr" builder
      | SCall (f,          act) -> let (fdef, fdecl) = StringMap.find f function_decls
                                   in let actuals = List.rev (List.map (expr builder) (List.rev act))
                                   in let result = (match fdecl.styp with
                                                      A.TUnit -> ""
                                                    | _       -> f ^ "_result")
                                   in L.build_call fdef (Array.of_list actuals) result builder
    in

    (* Each basic block in a program ends with a "terminator" instruction i.e.
    one that ends the basic block. By definition, these instructions must
    indicate which basic block comes next -- they typically yield "void" value
    and produce control flow, not values *)
    (* Invoke "f builder" if the current block doesn't already
       have a terminator (e.g., a branch). *)
    let add_terminal builder f =
                           (* The current block where we're inserting instr *)
      match L.block_terminator (L.insertion_block builder) with
	      Some _ -> ()
      | None   -> ignore (f builder) in



    (**************************
     *    Statement Builder   *
     **************************)



    (* Build the code for the given statement; return the builder for
       the statement's successor (i.e., the next instruction will be built
       after the one generated by this call) *)
    (* Imperative nature of statement processing entails imperative OCaml *)
    let rec stmt builder = function
	      SBlock sl -> List.fold_left stmt builder sl
        (* Generate code for this expression, return resulting builder *)
      | SExpr e   -> let _ = expr builder e in builder
      | SReturn e -> let _ = match fdecl.styp with
                                (* Special "return nothing" instr *)
                                A.TUnit -> L.build_ret_void builder
                                (* Build return statement *)
                              | _       -> L.build_ret (expr builder e) builder
                     in builder
      (* The order that we create and add the basic blocks for an If statement
      doesnt 'really' matter (seemingly). What hooks them up in the right order
      are the build_br functions used at the end of the then and else blocks (if
      they don't already have a terminator) and the build_cond_br function at
      the end, which adds jump instructions to the "then" and "else" basic blocks *)
      | SIf (predicate, then_stmt, else_stmt) ->
         let bool_val        = expr builder predicate
         (* Add "merge" basic block to our function's list of blocks *)
	       in let merge_bb     = L.append_block context "merge" the_function
         (* Partial function used to generate branch to merge block *)
         in let branch_instr = L.build_br merge_bb
         (* Same for "then" basic block *)
	       in let then_bb      = L.append_block context "then" the_function
         (* Position builder in "then" block and build the statement *)
         in let then_builder = stmt (L.builder_at_end context then_bb) then_stmt
         (* Add a branch to the "then" block (to the merge block)
           if a terminator doesn't already exist for the "then" block *)
	       in let ()           = add_terminal then_builder branch_instr
         (* Identical to stuff we did for "then" *)
	       in let else_bb      = L.append_block context "else" the_function
         in let else_builder = stmt (L.builder_at_end context else_bb) else_stmt
	       in let ()           = add_terminal else_builder branch_instr
         (* Generate initial branch instruction perform the selection of "then"
         or "else". Note we're using the builder we had access to at the start
         of this alternative. *)
	       in let _            = L.build_cond_br bool_val then_bb else_bb builder
         (* Move to the merge block for further instruction building *)
	       in L.builder_at_end context merge_bb

      | SWhile (predicate, body) ->
          (* First create basic block for condition instructions -- this will
          serve as destination in the case of a loop *)
	        let pred_bb          = L.append_block context "while" the_function
          (* In current block, branch to predicate to execute the condition *)
	        in let _             = L.build_br pred_bb builder
          (* Create the body's block, generate the code for it, and add a branch
          back to the predicate block (we always jump back at the end of a while
          loop's body, unless we returned or something) *)
	        in let body_bb       = L.append_block context "while_body" the_function
          in let while_builder = stmt (L.builder_at_end context body_bb) body
	        in let ()            = add_terminal while_builder (L.build_br pred_bb)
          (* Generate the predicate code in the predicate block *)
	        in let pred_builder  = L.builder_at_end context pred_bb
	        in let bool_val      = expr pred_builder predicate
          (* Hook everything up *)
	        in let merge_bb      = L.append_block context "merge" the_function
	        in let _             = L.build_cond_br bool_val body_bb merge_bb pred_builder
	        in L.builder_at_end context merge_bb
      | SInfloop (body) -> stmt builder ( SBlock [SWhile ((A.TBool ,SBoolLit(true)), SBlock [body]) ] )
      (* Implement for loops as while loops! *)
      | SFor (e1, e2, e3, body) -> stmt builder ( SBlock [SExpr e1 ; SWhile (e2, SBlock [body ; SExpr e3]) ] )
      | SContinue               -> raise (Prelude.TODO "implement")
      | SBreak                  -> raise (Prelude.TODO "implement")
    (* Build the code for each statement in the function *)
    in let builder = stmt builder (SBlock fdecl.sbody)
    (* Add a return if the last block falls off the end *)
    in add_terminal builder (match fdecl.styp with
        A.TUnit -> L.build_ret_void
      | t       -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in List.iter build_function_body functions;
  the_module
