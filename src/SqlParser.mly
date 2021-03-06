%{
    open Shared
    open Command
    open Sql
%}

%token <string> ID
%token <string> STRING
%token <int> INT
%token SELECT WHERE GROUP BY FROM ORDER UNION MINUS
%token AND OR NOT IN LT GT LEQ GEQ EQ NEQ POINT COMMA
%token LPAR RPAR AS EOL TIMES ADD SUB DIV

%start main
%type<Command.t> main
%%


main:
  /* Top-level commands */
  | POINT ID  { Command ($2) }

  /* SQL queries */
  | query EOL { Query ($1) }


query:
  | SELECT attributes FROM relations
    { SqlSelect($2, $4, VoidOp) }
  | SELECT attributes FROM relations WHERE conditions
    { SqlSelect($2, $4, $6) }
  | LPAR query RPAR MINUS LPAR query RPAR
    { SqlMinus($2, $6) }
  | LPAR query RPAR UNION LPAR query RPAR
    { SqlUnion($2, $6) }


/* Attributes */
attributes:
  | TIMES { [] }
  | attribute_list { $1 }

attribute_list:
  | attribute_named COMMA attribute_list { $1 :: $3 }
  | attribute_named { [$1] }

attribute_named:
  | selector       { {a_selector = $1; a_alias = None} }
  | selector ID    { {a_selector = $1; a_alias = Some ($2)} }
  | selector AS ID { {a_selector = $1; a_alias = Some ($3)} }

selector:
  | ID              { (None, $1) }
  | ID POINT ID     { (Some ($1), $3) }


/* Relations */
relations:
  | relation_named COMMA relations { $1 :: $3 }
  | relation_named  { [$1] }

relation_named:
  | relation ID     { {r_source = $1; r_alias = $2} }
  | relation AS ID  { {r_source = $1; r_alias = $3} }

relation:
  | STRING          { File ($1) }
  | LPAR query RPAR { Sub ($2) }


/* Conditions */
conditions:
  | conditions_and OR conditions { BinOp(Or, $1, $3) }
  | conditions_and { $1 }

conditions_and:
  | condition AND conditions_and { BinOp(And, $1, $3) }
  | condition { $1 }

condition:
  | LPAR condition RPAR             { $2 }
  | selector comp selector          { CompOp($2, $1, $3) }
  | selector IN LPAR query RPAR     { In($1, $4) }
  | selector NOT IN LPAR query RPAR { NotIn($1, $5) }


/* Comparison operators */
comp:
  | LT  { Lt }
  | GT  { Gt }
  | LEQ { Leq }
  | GEQ { Geq }
  | EQ  { Eq }
  | NEQ { Neq }