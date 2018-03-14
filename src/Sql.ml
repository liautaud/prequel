(** Types and functions for manipulating SQL queries. *)
open Shared
open Relational

(** A relation, with a possible alias. *)
type relation = {
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

(** An attribute with a possible alias. *)
and aliased = {
  a_name:  attribute;
  a_alias: string option; }

(** An abstract representation of a query. *)
and t =
  | SqlSelect of aliased list * relation list * condition
  | SqlMinus of t * t
  | SqlUnion of t * t
  [@@deriving show]


(** compile : t -> Relational.t
    Compiles a query into a relational term. *)
let rec compile = function
  (* TODO(liautaud) *)
  | _ -> failwith "Not implemented."