let (<|) = (@@)

(** Common exceptions. *)
exception FileError of string
exception SyntaxError
exception ExecutionError of string


(** fold_simple : ('a -> 'a -> 'a) -> 'a list -> 'a
    Folds the given list without using its head as
    the neutral element. *)
let fold_simple f l =
  List.fold_left f (List.hd l) (List.tl l)


(** pick_indices : int list -> 'a array -> 'a array
    Returns a copy of the source array where only
    the given indices remain. *)
let pick_indices ind a =
  let ind' = Array.of_list ind in
  Array.init (Array.length ind') (fun i -> a.(ind'.(i)))


(** Binary and comparison operators. *)
type bin_op =
  | And
  | Or
  [@@deriving show]

let fun_of_bin_op = function
  | And -> (&&)
  | Or  -> (||)

type comp_op =
  | Lt
  | Gt
  | Leq
  | Geq
  | Eq
  | Neq
  [@@deriving show]

let fun_of_comp_op = function
  | Lt  -> (<)
  | Gt  -> (>)
  | Leq -> (<=)
  | Geq -> (>=)
  | Eq  -> (=)
  | Neq -> (<>)


(* We chose to distinguish between the names of the
   attributes and the expressions which are used to
   select them. *)
type attribute = {
  att_relation: string;
  att_name: string;
  att_alias: string option; }

and selector =
  (string option) * string
  [@@deriving show]


(** matches_selector : selector -> attribute -> bool
    Returns whether a given selector matches an
    attribute name. *)
let matches_selector s a = match (s, a) with
  | (None, c), {att_alias = Some c'; _}
      when c = c' -> true
  | (None, c), {att_name = c'; _}
      when c = c' -> true
  | (Some r, c), {att_relation = r'; att_name = c'; _}
      when c = c' && r = r' -> true
  | _ -> false