open Database
open Main

type command =
  | Create of (string * string list)
  | Delete of string
  | List
  | ListRows of string
  | Get of (string * string)
  | Help
  | Quit

exception Empty

exception NoArgs

exception Malformed

exception Invalid

(** Help message for when Help command is typed. *)
let print_help_msg () =
  print_string "\n\n";
  ANSITerminal.print_string [ ANSITerminal.red ] "Help Menu\n";
  print_endline "";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "  Commands ";
  print_string "are not case sensitive. ";
  ANSITerminal.print_string [ ANSITerminal.cyan ] "Arguments ";
  print_string
    "ARE case sensitive, and are separated by a single space each.\n\n";

  ANSITerminal.print_string [ ANSITerminal.red ] "Command List:\n";
  print_endline "";
  (*Create table help*)
  ANSITerminal.print_string [ ANSITerminal.green ]
    "  Create new database table:\n\n";
  print_string "    Syntax: ";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "create table ";
  ANSITerminal.print_string [ ANSITerminal.cyan ]
    "table_name field_name1 field_name2 field_name3 ";
  print_string "(for any number of field names > 0)\n\n";
  print_string "    Example: ";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "create table ";
  ANSITerminal.print_string [ ANSITerminal.cyan ]
    "Users name age fav_color ";
  print_string
    "makes a new database table 'Users',\n\
    \    with column labels 'name', 'age', and 'fav_color'. Each User \
     object later added to\n\
    \    this table must have each of these field names.\n";
  print_endline "";
  (*Delete table help*)
  ANSITerminal.print_string [ ANSITerminal.green ]
    "  Delete database table:\n\n";
  print_string "    Syntax: ";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "delete table ";
  ANSITerminal.print_string [ ANSITerminal.cyan ] "table_name\n\n";
  print_string "    Example: ";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "delete table ";
  ANSITerminal.print_string [ ANSITerminal.cyan ] "Users ";
  print_string "deletes the database table labeled 'Users'.\n\n";
  print_string "             ";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "delete table ";
  ANSITerminal.print_string [ ANSITerminal.cyan ] "* ";
  print_string "deletes all database tables in the system.\n\n";

  (*get help*)
  ANSITerminal.print_string [ ANSITerminal.green ]
    "  Get values by field name:\n\n";
  print_string "    Syntax: ";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "get ";
  ANSITerminal.print_string [ ANSITerminal.cyan ] "field_name ";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "in ";
  ANSITerminal.print_string [ ANSITerminal.cyan ] "database_name\n\n";
  print_string "    Example: ";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "get ";
  ANSITerminal.print_string [ ANSITerminal.cyan ] "name ";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "in ";
  ANSITerminal.print_string [ ANSITerminal.cyan ] "Users ";
  print_string
    "prints a list of the names of each user in the User database.\n\n";

  (*list help*)
  ANSITerminal.print_string [ ANSITerminal.green ]
    "  List all database table names:\n\n";
  print_string "    Syntax: ";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "list\n\n";

  (*list rows help*)
  ANSITerminal.print_string [ ANSITerminal.green ]
    "  List all rows of a database table:\n\n";
  print_string "    Syntax: ";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "list rows ";
  ANSITerminal.print_string [ ANSITerminal.cyan ] "table_name\n\n";

  (*quit help*)
  ANSITerminal.print_string [ ANSITerminal.green ] "  Quit:\n\n";
  print_string "    Syntax: ";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "quit\n\n"

let rec parse_args (args : string list) acc : string list =
  match args with
  | h :: t ->
      if String.length h > 0 then parse_args t (acc @ [ h ])
      else parse_args t acc
  | [] -> if List.length acc > 0 then acc else raise NoArgs

let rec get_create_args args =
  match args with
  | h :: t ->
      if String.length h > 0 then (h, parse_args t [])
      else get_create_args t
  | [] -> raise NoArgs

let rec get_get_args val_name args =
  match args with
  | h :: t ->
      if String.lowercase_ascii h = "in" then
        match t with
        | h :: t ->
            if List.length t = 0 then Get (val_name, h)
            else raise Malformed
        | [] -> raise NoArgs
      else raise Malformed
  | [] -> raise NoArgs

let cmd_do (cmd : string) (args : string list) =
  match cmd with
  | "create" -> Create (get_create_args args)
  | "delete" -> begin
      match args with
      | h :: t ->
          if String.length h > 0 then Delete h else raise Malformed
      | [] -> raise NoArgs
    end
  | "get" -> begin
      match args with
      | h :: t -> get_get_args h t
      | [] -> raise Empty
    end
  | _ -> raise Invalid

let cmd_read cmd lst =
  match cmd with
  | "help" -> Help
  | "list" -> begin
      match lst with
      | h :: t ->
          if String.lowercase_ascii h = "rows" then
            match t with
            | h :: t -> ListRows h
            | [] -> raise NoArgs
          else raise Malformed
      | [] -> List
    end
  | "create" -> begin
      match lst with
      | h :: t ->
          if String.lowercase_ascii h = "table" then cmd_do "create" t
          else raise Malformed
      | [] -> raise Malformed
    end
  | "delete" -> begin
      match lst with
      | h :: t ->
          if String.lowercase_ascii h = "table" then cmd_do "delete" t
          else raise Malformed
      | [] -> raise Malformed
    end
  | "get" -> cmd_do "get" lst
  | "quit" -> Quit
  | _ -> raise Invalid

let rec get_cmd_sep (lst : string list) =
  match lst with
  | h :: t ->
      if String.length h > 0 then cmd_read (String.lowercase_ascii h) t
      else get_cmd_sep t
  | [] -> raise Empty

let parse (input : string) : command =
  input |> String.split_on_char ' ' |> get_cmd_sep

(*Allow for continuous input of commands until quit command is entered*)

let main () =
  ANSITerminal.print_string
    [ ANSITerminal.magenta ]
    "\n\n~~ Welcome to the Database Management System. ~~";
  print_endline "\n";
  print_endline "* Created by: Max Hadden & Tomas Kersulis";
  print_endline "* ";
  print_endline "* Enter command, or type help for more information.\n";
  let end_loop = ref false in
  while not !end_loop do
    print_string "> ";
    let cmd_entry = read_line () in
    match parse cmd_entry with
    | Help -> print_help_msg ()
    | Create (name, args) ->
        add_database "/database.json" name args;
        print_string "\nDatabase table ";
        ANSITerminal.print_string [ ANSITerminal.green ] name;
        print_string " created successfully.\n\n"
    | Get (val_name, db_name) -> begin
        match
          find_value_in_database "/database.json" db_name val_name
        with
        | exception DatabaseNotFound n ->
            print_string "\nError: database ";
            ANSITerminal.print_string [ ANSITerminal.red ] n;
            print_string " not found.\n\n"
        | exception ValNotFound v ->
            print_string "\nError: field ";
            ANSITerminal.print_string [ ANSITerminal.red ] v;
            print_string " not found.\n\n"
        | _ ->
            print_string
              (find_value_in_database "/database.json" db_name val_name);
            print_string "\n\n"
      end
    | Delete name -> begin
        match name with
        | "*" -> (
            ANSITerminal.print_string [ ANSITerminal.red ]
              "\n\
               Are you sure you want to delete all database tables? \
               (Y/N)\n\
               > ";
            match String.lowercase_ascii (read_line ()) with
            | "Y"
            | "y" ->
                clear_database_file "/database.json";
                print_string
                  "All database tables deleted successfully\n\n"
            | "N"
            | "n" ->
                print_string "Operation cancelled\n\n"
            | _ ->
                ANSITerminal.print_string [ ANSITerminal.red ]
                  "Invalid Command.\n\n")
        | _ -> begin
            match delete_database "/database.json" name with
            | exception DatabaseNotFound fname ->
                print_string "\nDatabase table ";
                ANSITerminal.print_string [ ANSITerminal.red ] fname;
                print_string " not found.\n\n"
            | _ ->
                print_string "\nDeleted database table ";
                ANSITerminal.print_string [ ANSITerminal.green ] name;
                print_string " successfully\n\n"
          end
      end
    | List ->
        ANSITerminal.print_string [ ANSITerminal.red ] "\nDatabases:\n";
        print_string (get_db_names_list "database.json")
    | ListRows name -> begin
        match list_rows "database.json" name with
        | exception DatabaseNotFound name ->
            print_string "\nDatabase ";
            ANSITerminal.print_string [ ANSITerminal.red ] name;
            print_string " not found.\n\n"
        | _ ->
            print_string "\nRows of database ";
            ANSITerminal.print_string [ ANSITerminal.red ] name;
            print_string ":\n";
            print_string (list_rows "database.json" name)
      end
    | Quit ->
        print_endline "\nChanges have been saved. Exiting...\n\n";
        end_loop := true
    | exception Malformed ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          "\n\
           Malformed command. Be sure to separate arguments with a \
           single whitespace.\n\n"
    | exception Invalid ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          "\nInvalid command.\n\n"
    | exception Empty ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          "\nEmpty command.\n\n"
    | exception NoArgs ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          "\nNo arguments or invalid argument number detected.\n\n"
  done

(*Execute engine*)
let () = main ()
