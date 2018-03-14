{
    open SqlParser
    exception Eof
}

let blank = [' ' '\r' '\t' '\n']
let digit = ['0'-'9']

rule token = parse
  | blank { token lexbuf }
  | ";"   { EOL }
  | "."   { POINT }
  | ","   { COMMA }
  | "("   { LPAR }
  | ")"   { RPAR }

  (* Comparison operators *)
  | "<"   { LT }
  | ">"   { GT }
  | "<="  { LEQ }
  | ">="  { GEQ }
  | "="   { EQ }
  | "!="  { NEQ }

  (* Arithmetic operators *)
  | "+"   { ADD }
  | "-"   { SUB }
  | "*"   { TIMES }
  | "/"   { DIV }

  (* Elementary tokens *)
  | digit+ as s           { INT (int_of_string s) }
  |'"' ([^'"']* as s) '"' { STRING (s) }
  | ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9']* as s {
    (* Borrowed from https://github.com/poechsel/MiniSQL. *)
    match String.lowercase s with
      | "select"  -> SELECT
      | "where"   -> WHERE
      | "from"    -> FROM
      | "group"   -> GROUP
      | "minus"   -> MINUS
      | "union"   -> UNION
      | "by"      -> BY
      | "order"   -> ORDER
      | "and"     -> AND
      | "or"      -> OR
      | "not"     -> NOT
      | "in"      -> IN
      | "as"      -> AS
      | _         -> ID (s)
}
