(** Types and functions for manipulating relational data. *)
open Shared

(** A tuple, i.e. an element of a relation instance. *)
type tuple =
  string array
  [@@deriving show]

(** A relation instance. *)
type instance = {
  i_header: string array;
  i_lines: tuple list; }
  [@@deriving show]

(** A relational term. *)
type t =
  | Projection of t * attribute list
  | Selection  of t * (tuple -> bool)
  | Renaming   of t * attribute * string
  | Product    of t * t
  | Union      of t * t
  | Minus      of t * t
  | Source     of string
  [@@deriving show]


(** instance_of_file : string -> instance
    Return the relation instance obtained from reading
    the CSV file at the given path. *)
let instance_of_file path =
  let csv = 
    try
      open_in path
      |> Csv.of_channel ?has_header:(Some true)
    with
      e -> raise (FileError path) in

  let header =
    csv
    |> Csv.Rows.header
    |> Array.of_list in

  let lines =
    Csv.fold_left
      ~f:(fun t x -> (Array.of_list x) :: t)
      ~init:[]
      csv in

  {i_header = header; i_lines = lines}


(** eval : t -> instance
    Evaluates a relational term and returns the resulting
    relation instance. *)
let rec eval = function
  | Source s -> instance_of_file s
  | _ -> failwith "Not implemented."