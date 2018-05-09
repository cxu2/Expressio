/* Ocamlyacc parser for MicroC */

%{
open Ast
open RegExp
open Prelude
%}

%token PERIOD SEMI LPAREN RPAREN LBRACE RBRACE LBRAC RBRAC COMMA PLUS MINUS TIMES DIVIDE ASSIGN
%token NOT EQ NEQ LT LEQ GT GEQ AND OR
%token RETURN IF ELSE FOR UNIT BOOL CHAR INT STRING
%token CONTINUE BREAK
%token COLON ARROW
%token CASE CASETO
%token REGEXP REMATCH REEMPTY REEPS RELIT REOR RECAT RESTAR REAND RECOMP
%token DFAOR DFACONCAT DFASIM DFAACCEPTS
%token DFATOKEN STATES ALPH START FINAL TRANF
%token <int> INTLIT
%token <bool> BLIT
%token <char> CHLIT
%token <string> STRLIT
%token <string> ID
%token EOF

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
%nonassoc NOT NEG

%right REMATCH
%left REOR
%left RECAT
%left REAND
%nonassoc RECOMP
%nonassoc RESTAR
%nonassoc RELIT

%left DFAOR
%left DFACONCAT
%left DFASIM
%left DFAACCEPTS

%%

program:
  decls EOF                                 { $1 }

decls:
   /* nothing */                            { (                    [],                      [],                     []) }
 | decls vdecl                              { ($2 :: Prelude.first $1,       Prelude.second $1,       Prelude.third $1) }
 | decls ddecl                              { (      Prelude.first $1, $2 :: Prelude.second $1,       Prelude.third $1) }
 | decls fdecl                              { (      Prelude.first $1,       Prelude.second $1, $2 :: Prelude.third $1) }

/* DFA declarations */
ddecl:
  ID LBRACE STATES COLON INTLIT ALPH COLON LBRAC char_opt RBRAC START COLON
  INTLIT FINAL COLON LBRAC int_opt RBRAC TRANF COLON LBRAC tfdecl_opt RBRAC RBRACE
                                            { { dfa_name     = $1;
                                                dfa_states   = $5;
                                                dfa_alphabet = $9;
                                                dfa_start    = $13;
                                                dfa_final    = $17;
                                                dfa_tranves  = $22 } }


fdecl:
  ID COLON formals_opt ARROW typ LBRACE vdecl_list stmt_list RBRACE
                                            { { typ     = $5;
	                                              fname   = $1;
	                                              formals = $3;
	                                              locals  = List.rev $7;
	                                              body    = List.rev $8 } }

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
  | REGEXP                                  { TRE     }
  | STRING                                  { TString }
  | DFATOKEN                                { TDFA    }

int_opt:
  /* nothing */                             { [] }
  | int_list                                { List.rev $1 }

int_list:
  INTLIT                                    { [$1] }
  | int_list COMMA INTLIT                   { $3 :: $1 }

char_opt:
  char_list                                 { List.rev $1 }

char_list:
  CHLIT                                     { [$1] }
  | char_list COMMA CHLIT                   { $3 :: $1 }

vdecl_list:
    /* nothing */                           { [] }
  | vdecl_list vdecl                        { $2 :: $1 }

vdecl:
   typ ID SEMI                              { ($1, $2) }

/* transition function declarations */
tfdecl:
   LPAREN INTLIT CHLIT INTLIT RPAREN        { ($2, $3, $4) }

tfdecl_opt:
  /* nothing */                             { [] }
  | tfdecl_list                             { List.rev $1 }

tfdecl_list:
    tfdecl                                  { [$1] }
  | tfdecl_list COMMA tfdecl                { $3 :: $1 }


stmt_list:
    /* nothing */                           { [] }
  | stmt_list stmt                          { $2 :: $1 }

for_body: LBRACE stmt_list RBRACE           { Block (List.rev $2)  }

stmt:
    expr SEMI                               { Expr $1               }
  | RETURN expr_opt SEMI                    { Return $2             }
  | CONTINUE SEMI                           { Continue              }
  | BREAK SEMI                              { Break                 }
  | LBRACE stmt_list RBRACE                 { Block (List.rev $2)   }
  /*
  | CASE expr       COLON
    REEMPTY         CASETO expr COMMA
    REEPS           CASETO expr COMMA
    RELIT expr      CASETO expr COMMA
    expr REAND expr CASETO expr COMMA
    expr REOR  expr CASETO expr COMMA
    expr RECAT expr CASETO expr COMMA
    RECOMP expr     CASETO expr COMMA
    expr RESTAR     CASETO expr SEMI        { Case ($2, [(RE RegExp.Zero,                 $6 )
                                                        ;(RE RegExp.One,                  $10)
                                                        ; (Unop  (     URELit,       $13), $15)
                                                        ; (Binop ($17, BREIntersect, $19), $21)
                                                        ; (Binop ($23, BREUnion,     $25), $27)
                                                        ; (Binop ($29, BREConcat,    $31), $33)
                                                        ; (Unop  (     UREComp,      $36), $38)
                                                        ; (Unop  (     UREStar,      $40), $43)
                                                        ])         }
  */
  | CASE expr   COLON
    REEMPTY     CASETO expr COMMA
    REEPS       CASETO expr COMMA
    RELIT ID    CASETO expr COMMA
    ID REAND ID CASETO expr COMMA
    ID REOR  ID CASETO expr COMMA
    ID RECAT ID CASETO expr COMMA
    RECOMP ID   CASETO expr COMMA
    ID RESTAR   CASETO expr SEMI            { Case ($2, [ ((Noexpr, Noexpr), $6 )
                                                        ; ((Noexpr, Noexpr), $10)
                                                        ; ((Noexpr, Id $13), $15)
                                                        ; ((Id $17, Id $19), $21)
                                                        ; ((Id $23, Id $25), $27)
                                                        ; ((Id $29, Id $31), $33)
                                                        ; ((Noexpr, Id $36), $38)
                                                        ; ((Noexpr, Id $40), $43)
                                                        ])         }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If ($3, $5, Block []) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If ($3, $5, $7)       }
  | FOR expr SEMI expr SEMI expr for_body
                                            { For ($2, $4, $6, $7)  }
  | FOR SEMI expr SEMI for_body             { While ($3, $5)        }
  | FOR for_body                            { Infloop ($2)          }

expr_opt:
    /* nothing */                           { Noexpr }
  | expr                                    { $1 }

expr:
    INTLIT                                  { IntLit ($1)                   }
  | BLIT                                    { BoolLit ($1)                  }
  | CHLIT                                   { CharLit ($1)                  }
  | STRLIT                                  { StringLit ($1)                }
  | ID                                      { Id ($1)                       }
  | RELIT expr                              { Unop (URELit, $2)             }
  | REEMPTY                                 { RE RegExp.Zero                }
  | REEPS                                   { RE RegExp.One                 }
  | expr PLUS       expr                    { Binop ($1, BAdd,          $3) }
  | expr MINUS      expr                    { Binop ($1, BSub,          $3) }
  | expr TIMES      expr                    { Binop ($1, BMult,         $3) }
  | expr DIVIDE     expr                    { Binop ($1, BDiv,          $3) }
  | expr EQ         expr                    { Binop ($1, BEqual,        $3) }
  | expr NEQ        expr                    { Binop ($1, BNeq,          $3) }
  | expr LT         expr                    { Binop ($1, BLess,         $3) }
  | expr LEQ        expr                    { Binop ($1, BLeq,          $3) }
  | expr GT         expr                    { Binop ($1, BGreater,      $3) }
  | expr GEQ        expr                    { Binop ($1, BGeq,          $3) }
  | expr AND        expr                    { Binop ($1, BAnd,          $3) }
  | expr OR         expr                    { Binop ($1, BOr,           $3) }
  | expr REAND      expr                    { Binop ($1, BREIntersect,  $3) }
  | expr REOR       expr                    { Binop ($1, BREUnion,      $3) }
  | expr RECAT      expr                    { Binop ($1, BREConcat,     $3) }
  | expr REMATCH    expr                    { Binop ($1, BREMatches,    $3) }
  | expr DFAOR      expr                    { Binop ($1, BDFAUnion,     $3) }
  | expr DFACONCAT  expr                    { Binop ($1, BDFAConcat,    $3) }
  | expr DFASIM     expr                    { Binop ($1, BDFASimulates, $3) }
  | expr DFAACCEPTS expr                    { Binop ($1, BDFAAccepts,   $3) }
  | MINUS expr %prec NEG                    { Unop (UNeg, $2)               }
  | NOT expr                                { Unop (UNot, $2)               }
  | expr RESTAR                             { Unop (UREStar, $1)            }
  | RECOMP expr                             { Unop (UREComp, $2)            }
  | ID ASSIGN expr                          { Assign ($1, $3)               }
  | ID LPAREN args_opt RPAREN               { Call ($1, $3)                 }
  | LPAREN expr RPAREN                      { $2                            }
  | LBRACE
      STATES COLON expr
      ALPH   COLON LBRAC char_opt   RBRAC
      START  COLON expr
      FINAL  COLON LBRAC int_opt    RBRAC
      TRANF  COLON LBRAC tfdecl_opt RBRAC
    RBRACE
                                            { DFA ($4, $8, $12, $16, $21)  }

args_opt:
    /* nothing */                           { [] }
  | args_list                               { List.rev $1 }

args_list:
    expr                                    { [$1] }
  | args_list COMMA expr                    { $3 :: $1 }
