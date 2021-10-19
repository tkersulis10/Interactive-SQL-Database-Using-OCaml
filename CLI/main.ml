open Database
open Main

type command =
  | Create of (string * string list)
  | Delete of string
  | Help
  | Quit

exception Empty

exception NoArgs

exception Malformed

let print_help_msg t =
  print_string "\n\n";
  ANSITerminal.print_string [ ANSITerminal.red ] "Help Menu";
  print_endline "";
  print_string
    "  Commands are not case sensitive.\n  Type quit to exit.\n\n";

  ANSITerminal.print_string [ ANSITerminal.red ] "Command List:\n";
  print_endline "";
  ANSITerminal.print_string [ ANSITerminal.green ]
    "  Create new database table:\n";
  print_string
    "    Syntax: create table table_name field_name1 field_name2 \
     field_name3\n";
  print_string
    "    Any number of field names may be added, each seperated by a \
     space.\n";
  print_string
    "    Example: 'create table example a b abc' makes a new database \
     table titled 'example', with field 'a', 'b', and 'abc'.";
  print_endline "";
  print_endline ""

(*converts a list [args] into a list of arguments without any empty
  string*)
let rec parse_args (args : string list) acc : string list =
  match args with
  | h :: t ->
      if String.length h > 0 then parse_args t (acc @ [ h ])
      else parse_args t acc
  | [] -> if List.length acc > 0 then acc else raise NoArgs

(*Extracts a tuple of name and list of field name args given a create
  table command from args list [args] raises: NoArgs if no args are
  available*)
let rec get_create_args args =
  match args with
  | h :: t ->
      if String.length h > 0 then (h, parse_args t [])
      else get_create_args t
  | [] -> raise NoArgs

let cmd_do (cmd : string) (args : string list) =
  match cmd with
  | "create" -> Create (get_create_args args)
  | _ -> raise Malformed

(*locates the fully parsed command (in the event the command is 2+ words
  like CREATE TABLE) and a list of arguments. Passes result into
  [cmd_do] for argument handling. Requires: cmd is a lowercase ascii
  string.*)
let cmd_read cmd lst =
  match cmd with
  | "help" -> Help
  | "create" -> begin
      match lst with
      | h :: t ->
          if String.lowercase_ascii h = "table" then cmd_do "create" t
          else raise Malformed
      | [] -> raise Malformed
    end
  | "quit" -> Quit
  | _ -> raise Empty

(* gets the first word of a command in list [lst], indicating command
   type, followed by the rest of the list, indicating arguments, and
   calls [cmd_read] on the resulting command and args*)
let rec get_cmd_sep (lst : string list) =
  match lst with
  | h :: t ->
      if String.length h > 0 then cmd_read (String.lowercase_ascii h) t
      else get_cmd_sep t
  | [] -> raise Empty

(*Parse an input string to a command of type command*)
let parse (input : string) : command =
  input |> String.split_on_char ' ' |> get_cmd_sep

(*Allow for continuous input of commands until quit command is entered*)

let main () =
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    "\n\n~~ Welcome to the Database Management System. ~~";
  print_endline "";
  print_endline "Enter command, or type help for more information.";
  print_endline "*";
  print_endline "* Created by: Max Hadden & Tomas Kersulis";
  print_endline "*";
  let end_loop = ref false in
  while not !end_loop do
    print_string ">";
    let cmd_entry = read_line () in
    match parse cmd_entry with
    | Help -> print_help_msg "h"
    | Create (name, args) -> add_database "/database.json" name args
    | Quit ->
        print_endline "Changes have been saved. Exiting...";
        end_loop := true
    | exception Malformed -> print_endline "Invalid command"
    | exception Empty -> print_endline "Empty command"
    | exception NoArgs ->
        print_endline "No arguments or invalid argument number detected"
    | _ -> print_endline "wildcard"
  done

(*Execute engine*)
let () = main ()
