(** A top-level input. **)
type t =
  | Command of string
  | Query of Query.t
  [@@deriving show]