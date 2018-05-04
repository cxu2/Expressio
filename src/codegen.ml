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
open Ast
open Sast
open Prelude
open RegExp.RegExp
(* open Exceptions *)

module StringMap = Map.Make(String)

(* Code Generation from the SAST. Returns an LLVM module if successful,
   throws an exception if something is wrong. *)

(* let translate (globals, dfas, functions) = *)
let translate (globals, _, functions) =
  let context : L.llcontext = L.global_context ()
   (* Add types to the context so we can use them in our LLVM code *)

  in let i32_t     :              L.lltype  = L.i32_type    context
     and i8_t      :              L.lltype  = L.i8_type     context
     and i1_t      :              L.lltype  = L.i1_type     context
     and void_t    :              L.lltype  = L.void_type   context
     and pointer_t : (L.lltype -> L.lltype) = L.pointer_type
    (* Create an LLVM module -- this is a "container" into which we'll
     generate actual code *)
     and the_module : L.llmodule = L.create_module context "Expressio"

  (* Tree named struct definition for regexp *)
  in let tree_t = L.struct_type context [| i8_t; i8_t; (pointer_t i8_t); (pointer_t i8_t) |]

  in let dfa_t =
      let types = Array.of_list [i32_t; L.pointer_type i8_t; i32_t; i32_t; L.pointer_type i32_t; i32_t; L.pointer_type i32_t] in
      L.struct_type context types


  (**************************
   * Ast type to LLVM type  *
   **************************)
  in let ltype_of_typ = function
      A.TInt    -> i32_t
    | A.TBool   -> i1_t
    | A.TChar   -> i8_t
    | A.TUnit   -> void_t
    | A.TRE     -> tree_t
    | A.TString -> L.pointer_type i8_t
    | A.TDFA    -> dfa_t


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

  in let printdfa_t = L.function_type i32_t [| L.pointer_type dfa_t |]
  in let printdfa_func = L.declare_function "printdfa" printdfa_t the_module

  in let dfaunion_t = L.function_type i32_t [| (L.pointer_type dfa_t); (L.pointer_type dfa_t); (L.pointer_type dfa_t) |]
  in let dfaunion_func = L.declare_function "dfaunion" dfaunion_t the_module

  in let accepts_t = L.function_type i1_t [|L.pointer_type dfa_t;  L.pointer_type i8_t |]
  in let accepts_func = L.declare_function "accepts" accepts_t the_module

  in let simulates_t = L.function_type i32_t [|L.pointer_type dfa_t;  L.pointer_type i8_t |]
  in let simulates_func = L.declare_function "simulates" simulates_t the_module in



  (**********************
   *   Build Functions  *
   **********************)

  (* Define each function (arguments and return type) so we can
   * define it's body and call it later *)

  let function_decls =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types = Array.of_list (List.map (fun (t, _) -> ltype_of_typ t) fdecl.sformals)
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

    in let arr_ptr a b = L.build_in_bounds_gep a [| L.const_int i32_t 0;  L.const_int i32_t 0|] "arr" b
    in let get_arr_idx a i b = L.build_in_bounds_gep a [| L.const_int i32_t 0;  L.const_int i32_t i|] "arr" b
    in let insert_elt a v i b = L.build_store v (get_arr_idx a i b) b

    in let get_struct_idx s i b = L.build_struct_gep s i "structelt" b

    in let build_lit op character b =
      let tree_ptr = L.build_alloca tree_t "tree_space" b in

      (* storing leaf node identifier operator l for lit *)
      let operator_ptr = L.build_in_bounds_gep tree_ptr [| itol 0; itol 0 |] "operator_ptr" b in
      ignore(L.build_store (L.const_int i8_t (int_of_char op)) operator_ptr b);

      (* storing the character *)
      let char_ptr = L.build_in_bounds_gep tree_ptr [| itol 0; itol 1 |] "char_ptr" b in
      ignore(L.build_store character char_ptr b);

      let tree_loaded = L.build_load tree_ptr "tree_loaded" b in
      tree_loaded


    in let build_unop op regexp b =
      let tree_ptr = L.build_alloca tree_t "tree_space" b in

      (* storing the operator *)
      let operator_ptr = L.build_in_bounds_gep tree_ptr [| itol 0; itol 0 |] "operator_ptr" b in
      ignore(L.build_store (L.const_int i8_t (int_of_char op)) operator_ptr b);

      (* storing left tree *)
      let left_ptr_ptr = L.build_in_bounds_gep tree_ptr [| itol 0; itol 2 |] "left_ptr_ptr" b in
      let left_tree_ptr = get_ptr regexp b in
      let left_tree_op_ptr = L.build_in_bounds_gep left_tree_ptr [| itol 0; itol 0 |] "left_tree_op_ptr" b in
      ignore(L.build_store left_tree_op_ptr left_ptr_ptr b);

      let tree_loaded = L.build_load tree_ptr "tree_loaded" b in
      tree_loaded


    in let build_binop op lregexp rregexp b =
      let tree_ptr : L.llvalue = L.build_alloca tree_t "tree_space" b in

      (* storing the operator *)
      let operator_ptr = L.build_in_bounds_gep tree_ptr [| itol 0; itol 0 |] "operator_ptr" b in
      ignore(L.build_store (L.const_int i8_t (int_of_char op)) operator_ptr b);

      (* storing left tree *)
      let left_ptr_ptr = L.build_in_bounds_gep tree_ptr [| itol 0; itol 2 |] "left_ptr_ptr" b in
      let left_tree_ptr = get_ptr lregexp b in
      let left_tree_op_ptr = L.build_in_bounds_gep left_tree_ptr [| itol 0; itol 0 |] "left_tree_op_ptr" b in
      ignore(L.build_store left_tree_op_ptr left_ptr_ptr b);

      (* storing right tree *)
      let right_ptr_ptr = L.build_in_bounds_gep tree_ptr [| itol 0; itol 3 |] "right_ptr_ptr" b in
      let right_tree_ptr = get_ptr rregexp b in
      let right_tree_op_ptr = L.build_in_bounds_gep right_tree_ptr [| itol 0; itol 0 |] "right_tree_op_ptr" b in
      ignore(L.build_store right_tree_op_ptr right_ptr_ptr b);

      let tree_loaded = L.build_load tree_ptr "tree_loaded" b in
      tree_loaded


    in let build_dfa n a s f d b =
      (*Getting our llvm values for array sizes, which we need for our c lib*)
      let ns = L.const_int i32_t n
      and len_a = List.length a in
      let delta_len = L.const_int i32_t (n*len_a)
      and start = L.const_int i32_t s
      and nsym = L.const_int i32_t len_a
      and nfin = L.const_int i32_t (List.length f) in

      (*Define llvm "array types"*)
      let alpha_t = L.array_type i8_t len_a
      and fin_t = L.array_type i32_t (List.length f) in
      (*and delta_row_t = L.array_type i32_t (List.length a) in*)
      let delta_t = L.array_type i32_t (n*len_a) in

      (*Allocating space and getting pointers*)
      let dfa_ptr = L.build_malloc dfa_t "dfa" b in
      let alpha_ptr = L.build_array_malloc alpha_t nsym "alpha" b in
      let fin_ptr = L.build_array_malloc fin_t nfin "fin" b in
      let delta_ptr = L.build_array_malloc delta_t delta_len "delta" b in

      (*preprocess our Ocaml lists so we can insert them into llvm arrays*)
      let ll_of_char c  = L.const_int i8_t (int_of_char c) in
      let list_of_llvm_char : L.llvalue list = List.map ll_of_char a in

      let ll_of_int fint = L.const_int i32_t fint in
      let list_of_llvm_int =  List.map ll_of_int f in

      (* copy over the values to the llvm arrays*)
      let copy_list_to_array (arr, i, localb) value = (ignore(insert_elt arr value i localb); arr, i + 1, localb) in

      ignore(List.fold_left copy_list_to_array (alpha_ptr, 0, b) list_of_llvm_char);
      ignore(List.fold_left copy_list_to_array (fin_ptr, 0, b) list_of_llvm_int);

      (*Now, to copy the delta function*)
      (*First, we obtain a mapping of characters to the appropriate index*)
      let rec get_char_index c (elts, idx) =
        if (List.hd elts) = c then idx else get_char_index c ((List.tl elts), idx+1) in

      (*fill the table with special value -1 to indicate no transition*)
      let rec build_memset len arr fill = match len with
        0 -> arr
        | _ -> build_memset (len-1) (fill::arr) fill in

      let filler = (build_memset (n*len_a) [-1] (-1)) in
      let llvm_filler = List.map ll_of_int filler in
      ignore(List.fold_left copy_list_to_array (delta_ptr, 0, b) llvm_filler);

      let copy_by_index (arr, lst, localb) (from_s, ch, to_s) =
        (ignore(insert_elt arr (L.const_int i32_t to_s) (from_s*len_a + (get_char_index ch (lst, 0))) localb); arr, lst, localb) in
      ignore(List.fold_left copy_by_index (delta_ptr, a, b) d);

      (* Stuff everything in the dfa struct *)
      ignore(L.build_store ns                    (get_struct_idx dfa_ptr 0 b) b);
      ignore(L.build_store (arr_ptr alpha_ptr b) (get_struct_idx dfa_ptr 1 b) b);
      ignore(L.build_store nsym                  (get_struct_idx dfa_ptr 2 b) b);
      ignore(L.build_store start                 (get_struct_idx dfa_ptr 3 b) b);
      ignore(L.build_store (arr_ptr fin_ptr   b) (get_struct_idx dfa_ptr 4 b) b);
      ignore(L.build_store nfin                  (get_struct_idx dfa_ptr 5 b) b);
      ignore(L.build_store (arr_ptr delta_ptr b) (get_struct_idx dfa_ptr 6 b) b);
      L.build_load dfa_ptr "dfa_loaded" b in


    let build_dfaunion d1 d2 b =
      let d1_ptr = get_ptr d1 b
      and d2_ptr = get_ptr d2 b in

      let n1   = L.build_load (get_struct_idx d1_ptr 0 b) "d1.nstates" b
      and n2   = L.build_load (get_struct_idx d2_ptr 0 b) "d2.nstates" b
      and nsym = L.build_load (get_struct_idx d2_ptr 2 b) "d2.nsym"    b
      and f1   = L.build_load (get_struct_idx d1_ptr 5 b) "d2.nsfin"   b
      and f2   = L.build_load (get_struct_idx d2_ptr 5 b) "d2.nfin"    b in

      let one = L.const_int i32_t 1 in

      let ns = (L.build_mul (L.build_add n1 one "++" b) (L.build_add n2 one "++" b) "mul" b)
      and nfin = (L.build_sub
                    (L.build_add
                        (L.build_mul f1 (L.build_add n2 one "++" b) "mult" b) (L.build_mul f2 (L.build_add n1 one "++" b) "mult" b)
                    "add" b)
                  (L.build_mul f1 f2 "mul" b) "sub" b) in

      (*Define llvm "array types"*)
      let alpha_t = L.array_type i8_t 1
      and fin_t = L.array_type i32_t 1 in
      let delta_t = L.array_type i32_t 1 in

      (*Allocating space and getting pointers*)
      let dfa_ptr = L.build_malloc dfa_t "dfa" b in
      let alpha_ptr = L.build_array_malloc alpha_t nsym "alpha" b in
      let fin_ptr = L.build_array_malloc fin_t nfin "fin" b in
      let delta_ptr = L.build_array_malloc delta_t (L.build_mul ns nsym "mul" b) "delta" b in

      ignore(L.build_store ns                    (get_struct_idx dfa_ptr 0 b) b);
      ignore(L.build_store (arr_ptr alpha_ptr b) (get_struct_idx dfa_ptr 1 b) b);
      ignore(L.build_store nsym                  (get_struct_idx dfa_ptr 2 b) b);
      ignore(L.build_store (arr_ptr fin_ptr   b) (get_struct_idx dfa_ptr 4 b) b);
      ignore(L.build_store nfin                  (get_struct_idx dfa_ptr 5 b) b);
      ignore(L.build_store (arr_ptr delta_ptr b) (get_struct_idx dfa_ptr 6 b) b);
      ignore(L.build_call dfaunion_func [| d1_ptr; d2_ptr; dfa_ptr |] "dfaunion" b);
      L.build_load dfa_ptr "dfa_loaded" b in

    (*************************
     *   Expression Builder  *
     *************************)



    (* Construct code for an expression; return its value *)
    let rec expr builder ((_, e) : sexpr) : L.llvalue = match e with
	      SIntLit i                        -> L.const_int i32_t i
      | SBoolLit b                       -> L.const_int i1_t (if b then 1 else 0)
      | SCharLit c                       -> L.const_int i8_t (int_of_char c)
      | SStringLit s                     -> L.build_global_stringptr s "string" builder
      | SNoexpr                          -> L.const_int i32_t 0
      | SId s                            -> L.build_load (lookup s) s builder
      (* TODO decide if it's better to keep this or do a nullary op constructor instead *)
      (* Convert SRE to its expression constituents *)
      | SRE Zero                         -> raise (Prelude.TODO "implement SRE Zero")
      | SRE One                          -> raise (Prelude.TODO "implement SRE One")
      | SRE (Lit c)                      -> expr builder (TRE, SUnop  (A.URELit,  (TRE, SRE (Lit c))))
      | SRE (Comp r)                     -> expr builder (TRE, SUnop  (A.UREComp, (TRE, SRE (Comp r))))
      | SRE (Star r)                     -> expr builder (TRE, SUnop  (A.UREStar, (TRE, SRE (Star r))))
      | SRE (Mult (a, b))                -> expr builder (TRE, SBinop ((TRE, SRE a), A.BREConcat,    (TRE, SRE b)))
      | SRE (And  (a, b))                -> expr builder (TRE, SBinop ((TRE, SRE a), A.BREIntersect, (TRE, SRE b)))
      | SRE (Plus (a, b))                -> expr builder (TRE, SBinop ((TRE, SRE a), A.BREUnion,     (TRE, SRE b)))
      | SBinop (e1, A.BDFAUnion,     e2) -> build_dfaunion (expr builder e1) (expr builder e2)                builder
      | SBinop (_, A.BDFAConcat,    _) -> raise (Prelude.TODO "implement codegen")
      | SBinop (e1, A.BDFAAccepts,   e2) -> L.build_call accepts_func [| (get_ptr (expr builder e1) builder); (expr builder e2) |] "accepts" builder
      | SBinop (e1, A.BDFASimulates, e2) -> L.build_call simulates_func [| (get_ptr (expr builder e1) builder); (expr builder e2) |] "accepts" builder
      | SBinop (e1, A.BREMatches,    e2) -> L.build_call matches_func [| (expr builder e1) ; (expr builder e2) |] "matches" builder
      | SBinop (e1, A.BREUnion,      e2) -> build_binop '|'         (expr builder e1) (expr builder e2)       builder
      | SBinop (e1, A.BREConcat,     e2) -> build_binop '^'         (expr builder e1) (expr builder e2)       builder
      | SBinop (e1, A.BREIntersect,  e2) -> build_binop '&'         (expr builder e1) (expr builder e2)       builder
      | SBinop (e1, A.BAdd,          e2) -> L.build_add             (expr builder e1) (expr builder e2) "tmp" builder
      | SBinop (e1, A.BSub,          e2) -> L.build_sub             (expr builder e1) (expr builder e2) "tmp" builder
      | SBinop (e1, A.BMult,         e2) -> L.build_mul             (expr builder e1) (expr builder e2) "tmp" builder
      | SBinop (e1, A.BDiv,          e2) -> L.build_sdiv            (expr builder e1) (expr builder e2) "tmp" builder
      | SBinop (e1, A.BAnd,          e2) -> L.build_and             (expr builder e1) (expr builder e2) "tmp" builder
      | SBinop (e1, A.BOr,           e2) -> L.build_or              (expr builder e1) (expr builder e2) "tmp" builder
      | SBinop (e1, A.BEqual,        e2) -> L.build_icmp L.Icmp.Eq  (expr builder e1) (expr builder e2) "tmp" builder
      | SBinop (e1, A.BNeq,          e2) -> L.build_icmp L.Icmp.Ne  (expr builder e1) (expr builder e2) "tmp" builder
      | SBinop (e1, A.BLess,         e2) -> L.build_icmp L.Icmp.Slt (expr builder e1) (expr builder e2) "tmp" builder
      | SBinop (e1, A.BLeq,          e2) -> L.build_icmp L.Icmp.Sle (expr builder e1) (expr builder e2) "tmp" builder
      | SBinop (e1, A.BGreater,      e2) -> L.build_icmp L.Icmp.Sgt (expr builder e1) (expr builder e2) "tmp" builder
      | SBinop (e1, A.BGeq,          e2) -> L.build_icmp L.Icmp.Sge (expr builder e1) (expr builder e2) "tmp" builder
      | SUnop (A.UNeg,    e)             -> L.build_neg             (expr builder e)                    "tmp" builder
      | SUnop (A.UNot,    e)             -> L.build_not             (expr builder e)                    "tmp" builder
      | SUnop (A.URELit,  e)             -> build_lit 'l'           (expr builder e)                          builder
      | SUnop (A.UREComp, e)             -> build_unop '\\'         (expr builder e)                          builder
      | SUnop (A.UREStar, e)             -> build_unop '*'          (expr builder e)                          builder
      | SAssign (s, e)                   -> let e' = expr builder e
                                            in let _  = L.build_store e' (lookup s) builder
                                            in e'
      | SDFA (n, a, s, f, delta)         -> build_dfa n a s f delta builder
      | SCase _ (*(e, cases) *)          -> raise (Prelude.TODO "codegen: expr fn, SCase") (* TODO build this multiway "if" into regular if statemants using stmt fn? (might have to use `and` for mutal recursion) *)
      | SCall ("print",    [e])          -> L.build_call printf_func   [| int_format_str ; (expr builder e)    |] "printf" builder
      | SCall ("printdfa", [e])          -> L.build_call printdfa_func [| get_ptr (expr builder e) builder     |] "printf" builder
      | SCall ("printf",   [e])          -> L.build_call printf_func   [| string_format_str ; (expr builder e) |] "printf" builder
      | SCall ("printr",   [e])          -> L.build_call printr_func   [| get_ptr (expr builder e) builder     |] "printr" builder
      | SCall (f,          act)          -> let (fdef, fdecl) = StringMap.find f function_decls
                                            and actuals = List.rev (List.map (expr builder) (List.rev act))
                                            in let result = (match fdecl.styp with
                                                              A.TUnit -> ""
                                                            | _       -> f ^ "_result")
                                            in L.build_call fdef (Array.of_list actuals) result builder


    (* Each basic block in a program ends with a "terminator" instruction i.e.
    one that ends the basic block. By definition, these instructions must
    indicate which basic block comes next -- they typically yield "void" value
    and produce control flow, not values *)
    (* Invoke "f builder" if the current block doesn't already
       have a terminator (e.g., a branch). *)
    in let add_terminal builder f =
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
    (* let rec stmt (builder : L.llbuilder) (x : sstmt) : L.llbuilder = match x with *)
    let rec stmt (builder,callStack) = function
    	      SBlock sl -> List.fold_left stmt (builder,callStack) sl
            (* Generate code for this expression, return resulting builder *)
          | SExpr e   -> let _ = expr builder e in (builder,callStack)
          | SReturn e -> let _ = match fdecl.styp with
                                    (* Special "return nothing" instr *)
                                    A.TUnit -> L.build_ret_void builder
                                    (* Build return statement *)
                                  | _       -> L.build_ret (expr builder e) builder
                         in (builder,callStack)
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
             in let (then_builder,_) = stmt ((L.builder_at_end context then_bb),callStack) then_stmt
             (* Add a branch to the "then" block (to the merge block)
               if a terminator doesn't already exist for the "then" block *)
    	       in let ()           = add_terminal then_builder branch_instr
             (* Identical to stuff we did for "then" *)
    	       in let else_bb      = L.append_block context "else" the_function
             in let (else_builder,_) = stmt ((L.builder_at_end context else_bb),callStack) else_stmt
    	       in let ()           = add_terminal else_builder branch_instr
             (* Generate initial branch instruction perform the selection of "then"
             or "else". Note we're using the builder we had access to at the start
             of this alternative. *)
    	       in let _            = L.build_cond_br bool_val then_bb else_bb builder
             (* Move to the merge block for further instruction building *)
    	       in (L.builder_at_end context merge_bb,callStack)
          | SWhile (lastInstr, predicate, body) ->
              (* Get the last instruction and revise body *)

              (* First create basic block for condition instructions -- this will
              serve as destination in the case of a loop *)
              let pred_bb          = L.append_block context "while" the_function
              (* In current block, branch to predicate to execute the condition *)
              in let _             = L.build_br pred_bb builder
              in let int_bb        = L.append_block context "int_bb" the_function
              (* Create the body's block, generate the code for it, and add a branch
              back to the predicate block (we always jump back at the end of a while
              loop's body, unless we returned or something) *)
              in let body_bb       = L.append_block context "while_body" the_function
              in let callStack = callStack @ [int_bb]
              in let (while_builder,_) = stmt ((L.builder_at_end context body_bb),callStack) body

              (* in let int_bb        = L.append_block context "int_bb" the_function  *)
              in let ()            = add_terminal while_builder (L.build_br int_bb)
              in let int_builder = L.builder_at_end context int_bb
              (* in let i3         = expr int_builder lastInstr *)
              in let (int_builder2,_) = stmt (int_builder,callStack) lastInstr
              in let ()            = add_terminal int_builder2 (L.build_br pred_bb)
              (* Generate the predicate code in the predicate block *)
              in let pred_builder  = L.builder_at_end context pred_bb
              in let bool_val      = expr pred_builder predicate
              (* Hook everything up *)
              in let merge_bb      = L.append_block context "merge" the_function
              in let _             = L.build_cond_br bool_val body_bb merge_bb pred_builder
              in (L.builder_at_end context merge_bb,callStack)
          | SInfloop (body) -> stmt (builder,callStack) ( SBlock [SWhile (SNostmt, (A.TBool, SBoolLit(true)), SBlock [body]) ] )
          (* Implement for loops as while loops! *)
          | SFor (e1, e2, e3, body) -> stmt (builder,callStack) ( SBlock [SExpr e1 ; SWhile (SExpr e3, e2, SBlock [body]) ] )
          | SContinue               ->
              if List.length callStack = 0 then (builder,callStack)
              else
              let continue_bb       = L.append_block context "continue_bb" the_function
              in let () = add_terminal builder (L.build_br continue_bb)
              in let b = L.builder_at_end context continue_bb
              (* in let _ = L.build_br continue_bb b *)

              in let _ = L.build_br (List.hd (List.rev callStack)) b
              (* in let int_bb       = L.append_block context "int_bb" the_function
              in let c = L.builder_at_end context int_bb *)

              in let callStack = List.rev (List.tl (List.rev callStack))
              in (b,callStack)
          | SBreak                  -> raise (Prelude.TODO "implement")
          | SNostmt                 -> (builder,callStack)
        (* Build the code for each statement in the function *)
        in let (builder,_) = stmt (builder,[]) (SBlock fdecl.sbody)
        (* Add a return if the last block falls off the end *)
        in add_terminal builder (match fdecl.styp with
            A.TUnit -> L.build_ret_void
          | t       -> L.build_ret (L.const_int (ltype_of_typ t) 0))
      in List.iter build_function_body functions;
      the_module
