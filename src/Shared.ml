let (<|) = (@@)

(** Common exceptions. *)
exception FileError of string
exception SyntaxError
exception ExecutionError

(** An attribute, with an optional relation name. *)
type attribute =
  (string option) * string
  [@@deriving show]

(** A logical operator. *)
type bin_op =
  | And
  | Or
  [@@deriving show]

(** A comparison operator. *)
type comp_op =
  | Lt
  | Gt
  | Leq
  | Geq
  | Eq
  | Neq
  [@@deriving show]
