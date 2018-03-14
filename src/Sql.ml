(** Types and functions for manipulating SQL queries. *)

(** A logical operator. *)
type bin_op =
  | And
  | Or

(** A comparison operator. *)
and comp_op =
  | Lt
  | Gt
  | Leq
  | Geq
  | Eq
  | Neq

(** An attribute, with a possible alias and column. *)
and attribute = {
  a_name:  string;
  a_from:  string option;
  a_alias: string option; }

(** A relation, with a possible alias. *)
and relation = {
  r_source: source;
  r_alias:  string; }

(* An relation source (i.e. a CSV file or a subquery). *)
and source =
  | File of string
  | Sub of t

(** A boolean condition. *)
and condition =
  | VoidOp
  | BinOp of bin_op * condition * condition
  | CompOp of comp_op * attribute * attribute
  | In of attribute * t
  | NotIn of attribute * t

(** An abstract representation of a query. *)
and t =
  | Select of attribute list * relation list * condition
  | Minus of t * t
  | Union of t * t
  [@@deriving show]