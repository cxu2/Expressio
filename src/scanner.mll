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
(* TODO we may need this later
| "nfa"     { NFA }
*)
| ':'      { COLON }                     (* Symbol for function definition arg types *)
(* | "->"     { ARROW }                      Symbol for function definition return type *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
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
| "return" { RETURN }
| "int"    { INT }
| "char"   { CHAR }
| "string" { STRING }
| "bool"   { BOOL }
| "unit"   { UNIT }
| "true"   { BLIT(true)  }
| "false"  { BLIT(false) }
| digits as ds                       { INTLIT(int_of_string ds) }
| '"'([^'"']* as strlit)'"'          { STRLIT(strlit) }
| '''([^''']  as chlit)'''           { CHLIT(chlit) }
| alpha (alphanumeric | '_')* as str { ID(str) }
| eof                                { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "~>\n" { token lexbuf }
| _    { comment lexbuf }

and line_comment = parse
  '\n'   { token lexbuf }
| _    { line_comment lexbuf }
