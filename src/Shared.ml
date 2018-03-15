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


(** print_table : string array list -> unit
    Pretty-prints a given table of strings. *)
let print_table m =
  let width = 20 in
  let cells = Array.length <| List.hd m in
  let delim = String.make ((width + 3) * cells + 1) '-' in

  let print_cell cell =
    let content =
      if String.length cell > width then
        (String.sub cell 0 (width - 3)) ^ "..."
      else
        cell in

    Printf.printf "| %-*s " width content in

  let print_header line =
    print_endline delim;
    Array.iter print_cell line;
    Printf.printf "|";
    print_newline ();
    print_endline delim in

  let print_line line =
    Array.iter print_cell line;
    Printf.printf "|";
    print_newline () in

  List.hd m |> print_header;
  List.tl m |> List.iter print_line;
  print_endline delim


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