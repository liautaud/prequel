open Shared
open Print
open Command
open Sql


(** REPL state variables. *)
let debug = ref false


(** print_header : unit -> unit
    Prints the welcome message to the standard output. *)
let print_header () =
  "                                                    888\n" ^
  "                                                    888\n" ^
  "88888b.  888d888 .d88b.   .d88888 888  888  .d88b.  888\n" ^
  "888  88b 888P   d8P  Y8b d88  888 888  888 d8P  Y8b 888\n" ^
  "888  888 888    88888888 888  888 888  888 88888888 888\n" ^
  "888 d88P 888    Y8b.     Y88b 888 Y88b 888 Y8b.     888\n" ^
  "88888P   888      Y8888    Y88888   Y88888   Y8888  888\n" ^
  "888                           888                      \n" ^
  "888                           888                      \n"
  |> blue
  |> print_endline;

  "Prequel version 1.0.                                   \n" ^
  "Enter .help for usage tips.                            \n" ^
  "Enter .debug to turn on debug output.                  \n" ^
  "Enter .cd to change the working directory.             \n"
  |> faint
  |> print_endline


(** print_error : str -> unit
    Prints an error message to the standard output. *)
let print_error e =
  "[ERROR] " ^ e
  |> err
  |> print_endline


(** parse_input : unit -> Command.t
    Attemps to parse a command from the standard input. *)
let parse_input () =
  try
    Lexing.from_channel stdin
    |> SqlParser.main SqlLexer.token
  with _ ->
    raise SyntaxError


(** run_query : Sql.t -> unit
    Attempts to execute a query using the interpreter. *)
let run_query query =
  if !debug then begin
    print_newline ();
    print_endline <| bold "Requête SQL :";
    query
    |> Sql.show
    |> print_endline;
    print_newline ();

    print_endline <| bold "Terme relationnel :";
    query
    |> Sql.compile
    |> Relational.show
    |> print_endline;
    print_newline ();

    print_endline <| bold "Résultat :"
  end;

  query
  |> Sql.compile
  |> Relational.eval
  |> Relational.print_instance;
  print_newline ()


(** run_command : Command.t -> unit
    Attemps to run a top-level command. *)
let run_command = function
  | Command "debug" ->
      debug := true;
      print_endline <| faint "Debug output enabled.";
      print_newline ()
  | Command "help"  -> print_error "Not implemented."
  | Command "cd"    -> print_error "Not implemented."
  | Command _       -> print_error "Unknown command."
  | Query q         -> run_query q


(** Main read-eval-print loop. *)
let () =
  print_header ();

  while true do
    print_string <| bold "> ";
    flush stdout;

    try
      parse_input ()
      |> run_command
    with
      | SyntaxError        -> print_error <| "Syntax error."
      | ExecutionError (s) -> print_error <| "Execution error: " ^ s
  done