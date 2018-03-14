(** Types and functions for manipulation SQL queries. *)

(** An attribute, with a possible alias and column. *)
type attribute = {
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
  | NoOp

(** An abstract representation of a query. *)
and t =
  | Select of attribute list * relation list * condition option
  | Minus of t * t
  | Union of t * t
  [@@deriving show]