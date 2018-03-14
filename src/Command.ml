(** A top-level input. **)
type t =
  | Command of string
  | Query of Sql.t
  [@@deriving show]