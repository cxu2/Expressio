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
open Prelude
open Exception

 module StringMap = Map.Make(String)

(* Code Generation from the SAST. Returns an LLVM module if successful,
   throws an exception if something is wrong. *)
let translate (globals, dfas, functions) =
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

  in let tree_t = L.struct_type context [| i8_t; (pointer_t i8_t); (pointer_t i8_t) |]

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
    | A.TRegexp -> tree_t
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


  (* Define each function (arguments and return type) so we can
   * define it's body and call it later *)
  in let function_decls =
    let function_decl m fdecl =
      let name = fdecl.fname
      and formal_types = Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.formals)
      in let ftype = L.function_type (ltype_of_typ fdecl.typ) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in

  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.fname function_decls
    in let builder = L.builder_at_end context (L.entry_block the_function)

    in let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
    and float_format_str = L.build_global_stringptr "%g\n" "fmt" builder

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
      in let formals = List.fold_left2 add_formal StringMap.empty fdecl.formals
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals fdecl.locals
    in

    (* Return the value for a variable or formal argument. First check
     * locals, then globals *)
    let lookup n = try StringMap.find n local_vars
                   with Not_found -> StringMap.find n global_vars
    in

    (* Construct code for an expression; return its value *)
    let rec expr builder e = match e with
	      IntLit i          -> L.const_int i32_t i
      | BoolLit b          -> L.const_int i1_t (if b then 1 else 0)
      | CharLit c          -> L.const_int i8_t (int_of_char c)
      (* | SFliteral l         -> L.const_float_of_string float_t l *)
      | StringLit s        -> L.build_global_stringptr s "string" builder
      | Noexpr             -> L.const_int i32_t 0
      | Id s               -> L.build_load (lookup s) s builder
      | Binop (e1, op, e2) -> let e1' = expr builder e1
                              and e2' = expr builder e2
                               in (match op with
                                	    A.BAdd     -> L.build_add
                                	  | A.BSub     -> L.build_sub
                                	  | A.BMult    -> L.build_mul
                                    | A.BDiv     -> L.build_sdiv
                                	  | A.BAnd     -> L.build_and
                                	  | A.BOr      -> L.build_or
                                	  | A.BEqual   -> L.build_icmp L.Icmp.Eq
                                	  | A.BNeq     -> L.build_icmp L.Icmp.Ne
                                	  | A.BLess    -> L.build_icmp L.Icmp.Slt
                                	  | A.BLeq     -> L.build_icmp L.Icmp.Sle
                                	  | A.BGreater -> L.build_icmp L.Icmp.Sgt
                                	  | A.BGeq     -> L.build_icmp L.Icmp.Sge
                                	  ) e1' e2' "tmp" builder
      | UnopPre (op, e)           -> (* let (t, _) = e *)
                                   let e' = expr builder e
                                   in (match op with
              	                          (* A.UNeg when t = A.TFloat -> L.build_fneg *)
              	                          A.UNeg                   -> L.build_neg
                                        | A.UNot                   -> L.build_not
                                        | A.UStar                  -> raise (Prelude.TODO "implement")
                                        | A.ULit                   -> raise (Prelude.TODO "implement")
                                   ) e' "tmp" builder
      | Assign (s, e)          -> let e' = expr builder e in
                                   let _  = L.build_store e' (lookup s) builder in e'
  (*)    | DFABody (n, a, s, f, delta) ->  let alloc_dfa name sts alpha start fin transitions = 
                                          let ns = L.const_int i32_t n
                                          and a = L.build_array_malloc i8_t (Array.of_list alpha) "alpha" builder
                                          and na = L.array_length a
                                          and f = L.build_array_malloc i32_t (Array.of_list fin) "fin" builder
                                          and nf = L.array_length f
                                          and d = L.build_malloc (L.pointer_type i32_t) "delta" builder in
                                          let dfa1 = L.build_malloc dfa_t "dfa" builder in
                                          let dfa_loaded = L.build_load dfa1 "dfa_loaded" builder in
                                          let dfa_loaded2 = L.build_insertvalue dfa_loaded (i32 10) 0 "dfa_loaded2" builder
                                            let dfa_struct = L.build_alloca dfa_t "dfa" builder in 
                                            L.struct_set_body (L.named_struct_type context "dfa") (Array.of_list [ns; a; na; f; nf; d])*)
      | Call ("print",    [e])
      | Call ("printb",   [e]) -> L.build_call printf_func   [| int_format_str ; (expr builder e) |]   "printf"   builder
      | Call ("printf",   [e]) -> L.build_call printf_func   [| float_format_str ; (expr builder e) |] "printf"   builder
      | Call (f,          act) -> let (fdef, fdecl) = StringMap.find f function_decls
                                   in let actuals = List.rev (List.map (expr builder) (List.rev act))
                                   in let result = (match fdecl.typ with
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

    (* Build the code for the given statement; return the builder for
       the statement's successor (i.e., the next instruction will be built
       after the one generated by this call) *)
    (* Imperative nature of statement processing entails imperative OCaml *)
    let rec stmt builder = function
	      Block sl -> List.fold_left stmt builder sl
        (* Generate code for this expression, return resulting builder *)
      | Expr e   -> let _ = expr builder e in builder
      | Return e -> let _ = match fdecl.typ with
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
      | If (predicate, then_stmt, else_stmt) ->
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

      | While (predicate, body) ->
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
      | Infloop (body) -> stmt builder ( Block [While ((BoolLit(true)), Block [body]) ] )
      (* Implement for loops as while loops! *)
      | For (e1, e2, e3, body) -> stmt builder ( Block [Expr e1 ; While (e2, Block [body ; Expr e3]) ] )
    (* Build the code for each statement in the function *)
    in let builder = stmt builder (Block fdecl.body)
    (* Add a return if the last block falls off the end *)
    in add_terminal builder (match fdecl.typ with
        A.TUnit -> L.build_ret_void
      | t       -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in List.iter build_function_body functions;
  the_module