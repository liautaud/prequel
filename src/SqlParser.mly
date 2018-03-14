%{
    open Command
    open Query
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
    { Select($2, $4, None) }
  | SELECT attributes FROM relations WHERE condition
    { Select($2, $4, Some $6) }
  | LPAR query RPAR MINUS LPAR query RPAR
    { Minus($2, $6) }
  | LPAR query RPAR UNION LPAR query RPAR
    { Union($2, $6) }


/* Attributes */
attributes:
  | TIMES { [] }
  | attribute_list { $1 }

attribute_list:
  | attribute_named COMMA attribute_list { $1 :: $3 }
  | attribute_named { [$1] }

attribute_named:
  | attribute       { $1 }
  | attribute ID    { {$1 with a_alias = Some ($2)} }
  | attribute AS ID { {$1 with a_alias = Some ($3)} }

attribute:
  | ID              { {a_name = $1; a_from = None;      a_alias = None} }
  | ID POINT ID     { {a_name = $3; a_from = Some ($1); a_alias = None} }


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


/* Conditions (TODO) */
condition:
  | { NoOp }