(* Ocamllex scanner for MicroC *)

{ open Parser }

let digit        = ['0' - '9']
let digits       = digit+
let alphaLower   = ['a' - 'z']
let alphaUpper   = ['A' - 'Z']
let alpha        = (alphaLower | alphaUpper)
let alphas       = alpha+
let alphanumeric = (alpha | digit)

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf }  (* Whitespace *)
| "<~"      { comment lexbuf }           (* Block Comments *)
| "~~"      { line_comment lexbuf }      (* Line Comment *)
| "{.}"     { REEMPTY }                  (* RegExp literal for empty language *)
| "{{.}}"   { REEPS }                    (* RegExp literal for empty string *)
| '|'       { REOR }                     (* RegExp operator for "or" (union) *)
| '^'       { REAND }                    (* RegExp operator for "and" (concatenation) *)
| "**"      { RESTAR }                   (* RegExp operator for Kleene star (closure) *)
| "lit"     { RELIT }                    (* RegExp operator for encapsulating a literal symbol *)
| "regexp"  { REGEXP }                   (* RegExp keyword for declaring a RegExp *)
| "matches" { REMATCH }                  (* RegExp operator for pattern matching an RE against a string *)
| "dfa"     { DFA }
(* TODO we will need these next once the new automata syntax is decided.
| "nfa"     { NFA }
*)
| ':'      { COLON }                     (* Symbol for function definition arg types *)
(*| "->"     { ARROW }                      Symbol for function definition return type *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['      { LBRAC }
| ']'      { RBRAC }
| ';'      { SEMI }
| ','      { COMMA }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '='      { ASSIGN }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "&&"     { AND }
| "||"     { OR }
| "!"      { NOT }
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
(*| "while"  { WHILE }*)
| "return" { RETURN }
| "int"    { INT }
| "char"   { CHAR }
| "bool"   { BOOL }
(*| "float"  { FLOAT }*)
| "unit"   { UNIT }
| "true"   { BLIT(true)  }
| "false"  { BLIT(false) }
| digits as lxm { INTLIT(int_of_string lxm) }
(* | digits '.'  digit* ( ['e' 'E'] ['+' '-']? digits )? as lxm { FLIT(lxm) } *)
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*     as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "~>\n" { token lexbuf }
| _    { comment lexbuf }

and line_comment = parse
  '\n'   { token lexbuf }
| _    { line_comment lexbuf }
