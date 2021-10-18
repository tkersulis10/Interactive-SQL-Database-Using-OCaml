open Database
open Main

type command =
  | Create of (string * string list)
  | Delete of string

exception Empty

exception NoArgs

exception Malformed

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
  like CREATE TABLE) and a list of arguments, and passes this into
  [cmd_do] to create a command object*)
let cmd_read cmd lst =
  match cmd with
  | "create" -> begin
      match lst with
      | h :: t ->
          if String.lowercase_ascii h = "table" then cmd_do "create" t
          else raise Malformed
      | [] -> raise Malformed
    end
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
  ANSITerminal.print_string [ ANSITerminal.green ]
    "\n\nWelcome to the Database Management System. ";
  print_endline "Enter command, or type Help for a list of commands.";
  print_string ">";
  let cmd_entry = read_line () in
  match parse cmd_entry with
  | Create (name, args) -> add_database "/database.json" name args
  | exception Malformed -> print_endline "Invalid command"
  | exception Empty -> print_endline "Empty command"
  | exception NoArgs ->
      print_endline "No arguments or invalid argument number detected"
  | _ -> print_endline "wildcard"

(*Execute engine*)
let () = main ()
