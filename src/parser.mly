/* Ocamlyacc parser for MicroC */

%{
open Ast
open RegExp
%}

%token PERIOD SEMI LPAREN RPAREN LBRACE RBRACE COMMA PLUS MINUS TIMES DIVIDE ASSIGN
%token NOT EQ NEQ LT LEQ GT GEQ AND OR
%token RETURN IF ELSE FOR UNIT BOOL CHAR INT STRING
%token COLON ARROW
%token REGEXP REMATCH REEMPTY REEPS RELIT REOR REAND RESTAR
%token DFA
%token <int> INTLIT
%token <bool> BLIT
%token <char> CHLIT
%token <string> STRLIT
%token <string> ID
%token EOF

/* FIXME we will need to think about the correct precedence of these
within the context of the entire language before adding */
/*
For reference, this is the correct precedence between RegExp operators in Haskell:
infixl 6 + (Numeric.Additive.Class)
infixl 7 * (Numeric.Algebra.Class)
infixr 8 `closure`
*/

%start program
%type <Ast.program> program

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS PERIOD MINUS
%left TIMES DIVIDE
%right NOT NEG

%left REOR
%left REAND
%right RESTAR
%right RELIT
%right REMATCH

%%

program:
  decls EOF                                 { $1 }

decls:
   /* nothing */                            { ([], [])               }
 | decls vdecl                              { (($2 :: fst $1), snd $1) }
 | decls fdecl                              { (fst $1, ($2 :: snd $1)) }


fdecl:
  ID COLON formals_opt ARROW typ LBRACE vdecl_list stmt_list RBRACE
                                            { { typ = $5;
	                                              fname = $1;
	                                              formals = $3;
	                                              locals = List.rev $7;
	                                              body = List.rev $8 } }

formals_opt:
    /* nothing */                           { [] }
  | formal_list                             { List.rev $1 }


formal_list:
    LPAREN ID COLON typ RPAREN              { [($4,$2)]     }
  | formal_list LPAREN ID COLON typ RPAREN              
                                            { ($5,$3) :: $1 }

typ:
    INT                                     { TInt    }
  | BOOL                                    { TBool   }
  | UNIT                                    { TUnit   }
  | CHAR                                    { TChar   }
  | REGEXP                                  { TRegexp }
  | STRING                                  { TString }


typs_opt:
    /* nothing */                           { [] }
    | typ_list                              { List.rev $1 }

typ_list:
    typ                                     { [$1] }
    | typ_list ARROW typ                    { $3 :: $1 }

vdecl_list:
    /* nothing */                           { [] }
  | vdecl_list vdecl                        { $2 :: $1 }

vdecl:
   typ ID SEMI                              { ($1, $2) }

stmt_list:
    /* nothing */                           { [] }
  | stmt_list stmt                          { $2 :: $1 }


stmt:
    expr SEMI                               { Expr $1               }
  | RETURN expr_opt SEMI                    { Return $2             }
  | LBRACE stmt_list RBRACE                 { Block (List.rev $2)   }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If ($3, $5, Block []) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If ($3, $5, $7)       }
  | FOR expr_opt SEMI expr SEMI expr_opt SEMI stmt
                                            { For ($2, $4, $6, $8)  }
  | FOR SEMI expr SEMI stmt                 { While ($3, $5)        }
  | FOR stmt                                { Infloop ($2)          }

expr_opt:
    /* nothing */                           { Noexpr }
  | expr                                    { $1 }

expr:
    INTLIT                                  { IntLit ($1)              }
  | BLIT                                    { BoolLit ($1)             }
  | CHLIT                                   { CharLit ($1)             }
  | STRLIT                                  { StringLit ($1)           }
  | ID                                      { Id ($1)                  }
  | RELIT expr                              { Unop (ULit, $2)          }
  | REEMPTY                                 { Regex RegExp.Zero        }
  | REEPS                                   { Regex RegExp.One         }
  | expr PLUS   expr                        { Binop ($1, BAdd,     $3) }
  | expr MINUS PERIOD expr                  { Binop ($1, BSub,     $4) }
  | expr TIMES  expr                        { Binop ($1, BMult,    $3) }
  | expr DIVIDE expr                        { Binop ($1, BDiv,     $3) }
  | expr EQ     expr                        { Binop ($1, BEqual,   $3) }
  | expr NEQ    expr                        { Binop ($1, BNeq,     $3) }
  | expr LT     expr                        { Binop ($1, BLess,    $3) }
  | expr LEQ    expr                        { Binop ($1, BLeq,     $3) }
  | expr GT     expr                        { Binop ($1, BGreater, $3) }
  | expr GEQ    expr                        { Binop ($1, BGeq,     $3) }
  | expr AND    expr                        { Binop ($1, BAnd,     $3) }
  | expr OR     expr                        { Binop ($1, BOr,      $3) }
  | expr REOR   expr                        { Binop ($1, BUnion,   $3) }
  | expr REAND  expr                        { Binop ($1, BConcat,  $3) }
  | expr REMATCH expr                       { Binop ($1, BMatch,   $3) }
  | MINUS expr %prec NEG                    { Unop (UNeg, $2)          }
  | NOT expr                                { Unop (UNot, $2)          }
  | RESTAR expr                             { Unop (UStar, $2)         }
  | ID ASSIGN expr                          { Assign ($1, $3)          }
  | ID LPAREN args_opt RPAREN               { Call ($1, $3)            }
  | LPAREN expr RPAREN                      { $2                       }

args_opt:
    /* nothing */                           { [] }
  | args_list                               { List.rev $1 }

args_list:
    expr                                    { [$1] }
  | args_list COMMA expr                    { $3 :: $1 }
