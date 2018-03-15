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
  | CompOp of comp_op * selector * selector
  | In of selector * t
  | NotIn of selector * t

(** An attribute with a possible alias. *)
and aliased = {
  a_selector: selector;
  a_alias:    string option; }

(** An abstract representation of a query. *)
and t =
  | SqlSelect of aliased list * relation list * condition
  | SqlUnion of t * t
  | SqlMinus of t * t
  [@@deriving show]


(** terms_of_relations : relation list -> Relational.t list
    Transforms a list of relations into a list of relational
    terms. Does not support subqueries yet. *)
let terms_of_relations = List.map (
  fun r -> match r.r_source with
    | File (path) -> Source (path, r.r_alias)
    | _ -> failwith "Not implemented.")


(** fun_of_condition : condition -> (header -> tuple -> bool)
    Transforms a boolean condition into an OCaml function
    which returns true iff. the condition is verified for
    a given tuple (and its corresponding header). *)
let rec fun_of_condition = function
  | VoidOp ->
      (fun h t -> true)
  | BinOp (op, c1, c2) ->
      let fo = fun_of_bin_op op in
      let f1 = fun_of_condition c1 in
      let f2 = fun_of_condition c2 in
      (fun h t -> fo (f1 h t) (f2 h t))
  | CompOp (op, s1, s2) ->
      let fo = fun_of_comp_op op in
      (fun h t ->
        let i1 = find_index h [s1] in
        let i2 = find_index h [s2] in
        fo t.(i1) t.(i2))
  | _ -> failwith "Not implemented."

(** compile : t -> Relational.t
    Compiles a query into a relational term. *)
let rec compile = function
  | SqlSelect (sels, rels, conds) ->
      (* We start by creating a cartesian product of all
         the relations that appear in the FROM clause. *)
      let source =
        rels
        |> terms_of_relations
        |> fold_simple (fun a b -> Product (a, b)) in

      (* We wrap those relations into as many renamings
         as there are aliases in the SELECT clause. *)
      let aliased =
        List.fold_left (fun p -> function
          | { a_alias = None; _ } -> p
          | { a_alias = Some a; a_selector = s } -> Renaming (p, s, a))
        <| source
        <| sels in

      (* We then filter them using the conditions from
         the WHERE clause. *)
      let filtered =
        Selection (aliased, fun_of_condition conds) in

      (* We finally project everything onto the selectors
         from the SELECT clause. *)
      let selectors =
        sels
        |> List.map (function
          | { a_alias = None; a_selector = s } -> s
          | { a_alias = Some a; _ } -> (None, a)) in

      if selectors = [] then
        filtered
      else
        Projection (filtered, selectors)

  | SqlUnion (t1, t2) ->
      Union (compile t1, compile t2)

  | SqlMinus (t1, t2) ->
      Minus (compile t1, compile t2)
