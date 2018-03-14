open Relational

let _ =
  Source "tests/projets.csv"
  |> eval
  |> show_instance
  |> print_endline