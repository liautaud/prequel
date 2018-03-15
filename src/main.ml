open Shared
open Relational

let _ =
  Source ("tests/projets.csv", "projets")
  |> eval
  |> show_instance
  |> print_endline;

  matches_selector (None, "foo") (Simple "foo")
  |> string_of_bool
  |> print_endline