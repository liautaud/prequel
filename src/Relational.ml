(** Types and functions for manipulating relational data. *)
open Shared

type header =
  attribute array
  [@@deriving show]

type tuple =
  string array
  [@@deriving show]

(** A relation instance. *)
type instance = {
  i_header: header;
  i_lines: tuple list; }
  [@@deriving show]

(** A relational term. *)
type t =
  | Projection of t * selector list
  | Selection  of t * (header -> tuple -> bool)
  | Renaming   of t * selector * string
  | Product    of t * t
  | Union      of t * t
  | Minus      of t * t
  | Source     of string * string
  [@@deriving show]


(** print_instance : instance -> unit
    Prints the given instance to the standard output. *)
let print_instance inst =
  let headers =
    inst.i_header
    |> Array.map (function
      | {att_alias = Some s; _} -> s
      | {att_relation = r; att_name = s; _} -> r ^ "." ^ s) in

  print_table <| headers :: inst.i_lines


(** find_indices : header -> selector list -> int list
    Returns the list of column of the header which
    matches any of the given selectors. This could
    probably be optimized using pre-computation. *)
let find_indices header sels =
  let indices = ref [] in
  let aux sel =
    let matches = matches_selector sel in
    let found = ref false in
    header
    |> Array.iteri (fun i h ->
        if matches h then begin
          indices := i :: (!indices);
          found := true
        end);
    if not !found then
      (* We don't allow invalid selectors, so if some
         selectors don't match any of the attributes
         in the header, we throw an error. *)
      raise (ExecutionError (
        "there is no attribute matching the selector " ^
        (show_selector sel) ^ ".")) in

  List.iter aux sels;
  !indices


(** find_index : header -> selector list -> int
    Tries to find the first column of the header
    which matches any of the given selectors. *)
let find_index header sels =
  let indices = find_indices header sels in
  List.hd indices


(** instance_of_file : string -> string -> instance
    Return the relation instance obtained by reading
    the CSV file at the given path.

    Note that this is definitely not the best way to
    implement reading from a file. Ideally, we would
    use streams or a similar data structure, instead
    of loading everything into memory. But this will
    have to wait a little bit. *)
let instance_of_source path name =
  let csv = 
    try
      open_in path
      |> Csv.of_channel ?has_header:(Some true)
    with
      e -> raise (FileError path) in

  let header =
    csv
    |> Csv.Rows.header
    |> List.map (fun col -> {att_relation = name; att_name = col; att_alias = None})
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
  | Projection (t, selectors) ->
      let t' = eval t in

      (* We find out which indices we must keep. *)
      let ind = find_indices t'.i_header selectors in

      { i_header = pick_indices ind t'.i_header;
        i_lines = List.map (pick_indices ind) t'.i_lines }

  | Selection (t, f) ->
      let t' = eval t in
      let l' = t'.i_lines
        |> List.filter (f t'.i_header) in
      {t' with i_lines = l'}

  | Renaming (t, before, after) ->
      let t' = eval t in

      (* Alias all the attributes which match
         the `before` selector with the new name. *)
      let matches = matches_selector before in
      let h' = t'.i_header
        |> Array.map (fun a ->
          if matches a then
            {a with att_alias = Some after}
          else a) in

      {t' with i_header = h'}

  | Product (t1, t2) ->
      failwith "Not implemented."

  | Union (t1, t2) ->
      failwith "Not implemented."

  | Minus (t1, t2) ->
      failwith "Not implemented."

  | Source (path, name) ->
      instance_of_source path name
