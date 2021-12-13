open Database
open Main

type command =
  | Addfield of (string * string * string * string)
  | Addrow of (string * string list)
  | Create of (string * (string * string) list)
  | Delete of string
  | DeleteRow of (string * string * string)
  | Get of (string * string)
  | List
  | ListFields of string
  | ListRows of string
  | Sort of (string * string)
  | Update of (string * string * string * string * string)
  | Help
  | Quit

exception Empty

exception NoArgs

exception Malformed

exception Invalid

exception UnsupportedType of string

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
    \    this table must have values for each of these field names.\n";
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

  (*Delete rows help*)
  ANSITerminal.print_string [ ANSITerminal.green ]
    "  Delete rows in a database table:\n\n";
  print_string "    Syntax: ";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "delete rows in ";
  ANSITerminal.print_string [ ANSITerminal.cyan ] "database_name ";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "where ";
  ANSITerminal.print_string [ ANSITerminal.cyan ] "field_name ";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "is ";
  ANSITerminal.print_string [ ANSITerminal.cyan ] "value\n\n";
  print_string "    Example: ";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "delete rows in ";
  ANSITerminal.print_string [ ANSITerminal.cyan ] "Users ";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "where ";
  ANSITerminal.print_string [ ANSITerminal.cyan ] "age ";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "is ";
  ANSITerminal.print_string [ ANSITerminal.cyan ] "20 ";
  print_string
    "deletes all rows in database Users where the age field is 20.\n\n";
  print_string "             ";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "delete rows in ";
  ANSITerminal.print_string [ ANSITerminal.cyan ] "Users ";
  print_string "deletes every row in the databse Users.\n\n";

  (*add row help*)
  ANSITerminal.print_string [ ANSITerminal.green ]
    "  Add a new row to an existing database:\n\n";
  print_string "    Syntax: ";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "add row to ";
  ANSITerminal.print_string [ ANSITerminal.cyan ] "database_name ";
  ANSITerminal.print_string [ ANSITerminal.cyan ]
    "value_1, value2, value_3 ... , value_n \n\n";
  print_string "    Examples: ";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "add row to ";
  ANSITerminal.print_string [ ANSITerminal.cyan ]
    "Users Max, 20, cs major ";
  print_string
    "\n\
    \              adds a row [name: Max, age: 20, fact: cs major] to \
     the database Users (provided it has fields name age and fact)\n\n";
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    "              add row to ";
  ANSITerminal.print_string [ ANSITerminal.cyan ] "Users Max, , ";
  print_string
    "\n\
    \              adds a row [name: Max, age: , fact: ] to the \
     database Users (provided it has fields name age and fact)\n\n";

  (*add field help*)
  ANSITerminal.print_string [ ANSITerminal.green ]
    "  Add a new field to an existing database (with optional default \
     value):\n\n";
  print_string "    Syntax: ";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "add field ";
  ANSITerminal.print_string [ ANSITerminal.cyan ] "field_name ";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "to ";
  ANSITerminal.print_string [ ANSITerminal.cyan ] "database_name ";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "as ";
  ANSITerminal.print_string [ ANSITerminal.cyan ] "field_name \n\n";
  print_string "    Examples: ";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "add field ";
  ANSITerminal.print_string [ ANSITerminal.cyan ] "college ";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "to ";
  ANSITerminal.print_string [ ANSITerminal.cyan ] "Users ";
  print_string
    "adds a field 'college' with no corresponding value\n\
    \               to every user object in the database.\n\n";
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    "              add field ";
  ANSITerminal.print_string [ ANSITerminal.cyan ] "college ";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "to ";
  ANSITerminal.print_string [ ANSITerminal.cyan ] "Users ";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "as ";
  ANSITerminal.print_string [ ANSITerminal.cyan ] "Cornell ";
  print_string
    "adds a field 'college' with a value of Cornell\n\
    \               to every user object in the database.\n\n";

  (*get help*)
  ANSITerminal.print_string [ ANSITerminal.green ]
    "  Get values from a database by field name:\n\n";
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
  ANSITerminal.print_string [ ANSITerminal.cyan ] "database_name\n\n";

  (*list fields help*)
  ANSITerminal.print_string [ ANSITerminal.green ]
    "  List all field names of a database table:\n\n";
  print_string "    Syntax: ";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "list fields ";
  ANSITerminal.print_string [ ANSITerminal.cyan ] "database_name\n\n";

  (*update value help*)
  ANSITerminal.print_string [ ANSITerminal.green ]
    "  Update value(s) of database field(s):\n\n";
  print_string "    Syntax: ";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "update ";
  ANSITerminal.print_string [ ANSITerminal.cyan ] "field_name ";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "in ";
  ANSITerminal.print_string [ ANSITerminal.cyan ] "database_name ";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "to ";
  ANSITerminal.print_string [ ANSITerminal.cyan ] "new_value ";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "where ";
  ANSITerminal.print_string [ ANSITerminal.cyan ] "field_name_2 ";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "is ";
  ANSITerminal.print_string [ ANSITerminal.cyan ] "existing_value \n\n";
  print_string "    Examples: ";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "update ";
  ANSITerminal.print_string [ ANSITerminal.cyan ] "age ";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "in ";
  ANSITerminal.print_string [ ANSITerminal.cyan ] "Users ";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "to ";
  ANSITerminal.print_string [ ANSITerminal.cyan ] "21 ";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "where ";
  ANSITerminal.print_string [ ANSITerminal.cyan ] "name ";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "is ";
  ANSITerminal.print_string [ ANSITerminal.cyan ] "Max ";
  print_string
    "updates the age field to 21 for all the rows \n\
    \              with name field equal to Max.\n\n";
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    "              update ";
  ANSITerminal.print_string [ ANSITerminal.cyan ] "age ";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "in ";
  ANSITerminal.print_string [ ANSITerminal.cyan ] "Users ";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "to ";
  ANSITerminal.print_string [ ANSITerminal.cyan ] "21 ";
  print_string
    "updates the age field to 21 for all the rows \n\
    \              of the database.\n\n";

  (*quit help*)
  ANSITerminal.print_string [ ANSITerminal.green ] "  Quit:\n\n";
  print_string "    Syntax: ";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "quit\n\n"

(** [run_update_list row_list db_name field_name new_val] updates each
    value corresponding to the int row numbers in [row list] and field
    name [field_name] to new value [new_val].*)
let rec run_update_list row_list db_name field_name new_val num_updated
    =
  match row_list with
  | h :: t -> begin
      match
        update_value "database.json" db_name field_name h new_val
      with
      | exception WrongType (vname, exp_type) ->
          ANSITerminal.print_string [ ANSITerminal.red ] "\nError! ";
          print_string "Value: ";
          ANSITerminal.print_string [ ANSITerminal.red ] vname;
          print_string " cannot be added to field with type: ";
          ANSITerminal.print_string [ ANSITerminal.green ] exp_type;
          print_string "\n\n"
      | exception _ ->
          print_string
            "Error accessing database rows. Partial update may be \
             applied: check for errors.\n\n"
      | _ ->
          run_update_list t db_name field_name new_val (num_updated + 1)
    end
  | [] ->
      if num_updated > 1 then
        print_string
          ("\n"
          ^ Int.to_string num_updated
          ^ " values successfully updated.\n\n")
      else if num_updated = 1 then
        print_string ("\n" ^ "1" ^ " value successfully updated.\n\n")
      else
        print_string
          ("\n" ^ "No field" ^ field_name ^ " exists in database"
         ^ db_name ^ ".\n\n")

(** gets a list of (field, type) for each field in the create table
    arguments*)
let parse_args (args : string list) acc : (string * string) list =
  let combined = String.concat " " args in
  let pair_split = String.split_on_char ',' combined in
  let construct_tuple (fields_types : string list) =
    match fields_types with
    | field :: t -> (
        match t with
        | typename :: t -> (
            match String.trim (String.lowercase_ascii typename) with
            | "" -> (String.trim field, "string")
            | "string"
            | "int"
            | "float"
            | "bool" ->
                ( String.trim field,
                  String.trim (String.lowercase_ascii typename) )
            | _ -> raise (UnsupportedType typename))
        | [] -> raise Malformed)
    | [] -> raise NoArgs
  in
  let rec make_pair_list string_pairs acc =
    match string_pairs with
    | h :: t ->
        if List.length (String.split_on_char ':' h) = 2 then
          make_pair_list t
            (acc @ [ construct_tuple (String.split_on_char ':' h) ])
        else raise Malformed
    | [] -> acc
  in
  make_pair_list pair_split []

(** [get_create_args args] extracts a tuple of (name,
    field/type_name_args) given a create table command from args list
    [args]. Raises: NoArgs if no args are available. *)
let rec get_create_args args =
  match args with
  | h :: t ->
      if String.length h > 0 then (h, parse_args t [])
      else get_create_args t
  | [] -> raise NoArgs

(** [get_get_args val_name args] extracts a tuple of (value_name,
    database_name) given a get _ in _ command from args list [args] and
    given value_name [val_name]. Raises: NoArgs if no args are available
    or Malformed if incorrectly formatted. *)
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

(** [get_addfield_args args] extracts a tuple of (field_name,
    database_name, value_name) given an add field _ to _ as _ command
    from args list [args] . Raises: NoArgs if no args are available or
    Malformed if incorrectly formatted.

    let field_type = let header = List.hd table_list in String.sub
    header 1 (String.length header - 1) ^ "}" |>
    Yojson.Basic.from_string |> Yojson.Basic.Util.to_assoc |> List.assoc
    field_name |> Yojson.Basic.to_string*)
let get_addfield_args args =
  let combined = String.concat " " args in
  let field_and_type = String.split_on_char ':' combined in
  let field_name, field_type, rest_args =
    match field_and_type with
    | fname :: t -> (
        match t with
        | h :: t -> (
            match String.split_on_char ' ' (String.trim h) with
            | ftype :: rest -> (fname, ftype, rest)
            | [] -> raise Malformed)
        | [] -> (
            match args with
            | fname_notype :: t -> (fname_notype, "string", t)
            | [] -> raise NoArgs))
    | [] -> raise Malformed
  in
  match rest_args with
  | h :: t ->
      if h = "to" then
        match t with
        | h :: t -> (
            let db = h in
            match t with
            | h :: t ->
                if h = "as" then
                  match t with
                  | h :: t ->
                      let value_name = h in
                      ( String.trim field_name,
                        String.lowercase_ascii field_type,
                        String.trim db,
                        String.trim value_name )
                  | [] -> raise NoArgs
                else raise Malformed
            | [] ->
                let value_name = "" in
                ( String.trim field_name,
                  String.lowercase_ascii field_type,
                  String.trim db,
                  String.trim value_name ))
        | [] -> raise NoArgs
      else raise Malformed
  | [] -> raise NoArgs

(** [get_deleterow_args args] extracts a tuple of (db_name, comp_field,
    comp_val) given a delete row in _ where _ is _ command from args
    list [args] . Raises: NoArgs if no args are available or Malformed
    if incorrectly formatted. *)
let get_deleterow_args args =
  match args with
  | h :: t -> (
      let db = h in
      match t with
      | h :: t ->
          if h = "where" then
            match t with
            | h :: t -> (
                let comp_field = h in
                match t with
                | h :: t ->
                    if h = "is" then
                      match t with
                      | h :: t ->
                          let comp_val = h in
                          (db, comp_field, comp_val)
                      | [] -> raise NoArgs
                    else raise Malformed
                | [] -> raise NoArgs)
            | [] -> raise NoArgs
          else raise Malformed
      | [] -> (db, "*", "*"))
  | [] -> raise NoArgs

(** [get_update_args args] extracts a tuple of (field_name, db_name,
    new_val, comp_field, comp_val) given an update command from args
    list [args]. Raises: NoArgs if no args are available or Malformed if
    incorrect syntax keywords are used. *)
let get_update_args args =
  match args with
  | h :: t -> (
      let field_name = h in
      match t with
      | h :: t ->
          if h = "in" then
            match t with
            | h :: t -> (
                let db_name = h in
                match t with
                | h :: t ->
                    if h = "to" then
                      match t with
                      | h :: t -> (
                          let new_val = h in
                          match t with
                          | h :: t ->
                              if h = "where" then
                                match t with
                                | h :: t -> (
                                    let comp_field = h in
                                    match t with
                                    | h :: t ->
                                        if h = "is" then
                                          match t with
                                          | h :: t ->
                                              let comp_val = h in
                                              ( db_name,
                                                field_name,
                                                new_val,
                                                comp_field,
                                                comp_val )
                                          | [] -> raise NoArgs
                                        else raise Malformed
                                    | [] -> raise NoArgs)
                                | [] -> raise NoArgs
                              else raise Malformed
                          | [] ->
                              (db_name, field_name, new_val, "*", "*"))
                      | [] -> raise NoArgs
                    else raise Malformed
                | [] -> raise NoArgs)
            | [] -> raise NoArgs
          else raise Malformed
      | [] -> raise NoArgs)
  | [] -> raise NoArgs

let get_addrow_vals args =
  let combined = String.concat " " args in
  String.split_on_char ',' combined

let get_sort_args args =
  match args with
  | h :: t -> (
      let db_name = h in
      match t with
      | h :: t ->
          if h = "by" then
            match t with
            | h :: t ->
                let sort_field = h in
                (db_name, sort_field)
            | [] -> raise NoArgs
          else raise Malformed
      | [] -> raise NoArgs)
  | [] -> raise NoArgs

let cmd_do (cmd : string) (args : string list) =
  match cmd with
  | "addrow" ->
      if args != [] then
        if List.hd args = "to" then
          let val_args = List.tl args in
          if val_args != [] then
            Addrow (List.hd val_args, get_addrow_vals (List.tl val_args))
          else raise NoArgs
        else raise NoArgs
      else raise NoArgs
  | "create" -> Create (get_create_args args)
  | "addfield" -> Addfield (get_addfield_args args)
  | "delete" -> begin
      match args with
      | h :: t ->
          if String.length h > 0 then Delete h else raise Malformed
      | [] -> raise NoArgs
    end
  | "deleterow" -> DeleteRow (get_deleterow_args args)
  | "update" -> Update (get_update_args args)
  | "sort" -> Sort (get_sort_args args)
  | "get" -> begin
      match args with
      | h :: t -> get_get_args h t
      | [] -> raise Empty
    end
  | _ -> raise Invalid

let cmd_read cmd lst =
  match cmd with
  | "help" -> Help
  | "add" -> begin
      match lst with
      | h :: t ->
          if String.lowercase_ascii h = "field" then cmd_do "addfield" t
          else if String.lowercase_ascii h = "row" then
            cmd_do "addrow" t
          else raise Malformed
      | [] -> raise Malformed
    end
  | "list" -> begin
      match lst with
      | h :: t ->
          if String.lowercase_ascii h = "rows" then
            match t with
            | h :: t -> ListRows h
            | [] -> raise NoArgs
          else if String.lowercase_ascii h = "fields" then
            match t with
            | h :: t -> ListFields h
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
          else if
            String.lowercase_ascii h = "rows"
            && t != []
            && String.lowercase_ascii (List.hd t) = "in"
          then cmd_do "deleterow" (List.tl t)
          else raise Malformed
      | [] -> raise Malformed
    end
  | "sort" -> cmd_do "sort" lst
  | "update" -> cmd_do "update" lst
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
    | Addrow (db_name, values_list) -> begin
        match add_row "database.json" db_name values_list with
        | exception DatabaseNotFound n ->
            ANSITerminal.print_string [ ANSITerminal.red ] "\nError: ";
            print_string "database ";
            ANSITerminal.print_string [ ANSITerminal.red ] n;
            print_string " not found.\n\n"
        | exception InvalidShape ->
            ANSITerminal.print_string [ ANSITerminal.red ] "\nError: ";
            print_string "invalid number of value entries.\n\n";
            print_string
              "A value must be input for each of the fields of ";
            ANSITerminal.print_string [ ANSITerminal.green ] db_name;
            print_string ".\n\nThese fields are\n";
            print_string
              (String.concat ", "
                 (get_fields_list "database.json" db_name));
            print_string "\n\n"
        | exception WrongType (vname, field_t) ->
            ANSITerminal.print_string [ ANSITerminal.red ] "\nError! ";
            print_string "Value: ";
            ANSITerminal.print_string [ ANSITerminal.red ] vname;
            print_string " cannot be added to field with type: ";
            ANSITerminal.print_string [ ANSITerminal.green ] field_t;
            print_string "\n\n"
        | _ ->
            print_string "\n\nRow successfully added to ";
            ANSITerminal.print_string [ ANSITerminal.green ] db_name;
            print_string ".\n\n"
      end
    | Addfield (field_name, field_type, database_name, value_name) ->
    begin
        match
          add_field_to_all_rows "database.json" ~val_name:value_name
            database_name field_name field_type
        with
        | exception DatabaseNotFound n ->
            ANSITerminal.print_string [ ANSITerminal.red ] "\nError: ";
            print_string " database ";
            ANSITerminal.print_string [ ANSITerminal.red ] n;
            print_string " not found.\n\n"
        | exception WrongType (vname, field_t) ->
            ANSITerminal.print_string [ ANSITerminal.red ] "\nError! ";
            print_string "Value: ";
            ANSITerminal.print_string [ ANSITerminal.red ] vname;
            print_string " cannot be added to field with type: ";
            ANSITerminal.print_string [ ANSITerminal.green ] field_t;
            print_string "\n\n"
        | _ -> print_string "\n\nTable successfully updated.\n\n"
      end
    | Get (val_name, db_name) -> begin
        match
          find_value_in_database "/database.json" db_name val_name
        with
        | exception DatabaseNotFound n ->
            print_string "\nError: database ";
            ANSITerminal.print_string [ ANSITerminal.red ] n;
            print_string " not found.\n\n"
        | exception ValNotFound v ->
            ANSITerminal.print_string [ ANSITerminal.red ] "\nError: ";
            print_string " field ";
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
        | _ -> (
            ANSITerminal.print_string [ ANSITerminal.red ]
              ("\nAre you sure you want to delete database \"" ^ name
             ^ "\"?\n(Y/N)\n> ");
            match String.lowercase_ascii (read_line ()) with
            | "Y"
            | "y" -> begin
                match delete_database "database.json" name with
                | exception DatabaseNotFound fname ->
                    ANSITerminal.print_string [ ANSITerminal.red ]
                      "\nError: ";
                    print_string "database table ";
                    ANSITerminal.print_string [ ANSITerminal.red ] fname;
                    print_string " not found.\n\n"
                | _ ->
                    print_string "\nDeleted database table ";
                    ANSITerminal.print_string [ ANSITerminal.green ]
                      name;
                    print_string " successfully\n\n"
              end
            | "N"
            | "n" ->
                print_string "Operation cancelled\n\n"
            | _ ->
                ANSITerminal.print_string [ ANSITerminal.red ]
                  "Invalid Decision.\n\n")
      end
    | DeleteRow (db_name, comp_field, comp_val) -> (
        if comp_field = "*" && comp_val = "*" then (
          ANSITerminal.print_string [ ANSITerminal.red ]
            ("\nThis action will delete all rows of database " ^ db_name
           ^ ". Proceed? (Y/N)\n> ");
          match String.lowercase_ascii (read_line ()) with
          | "Y"
          | "y" -> (
              match clear_database "database.json" db_name with
              | exception DatabaseNotFound n ->
                  ANSITerminal.print_string [ ANSITerminal.red ]
                    "\nError: ";
                  print_string "database table ";
                  ANSITerminal.print_string [ ANSITerminal.red ] n;
                  print_string " not found.\n\n"
              | exception _ ->
                  print_string
                    "\n\
                     Internal error. This line should never have \
                     triggered."
              | _ ->
                  print_string "\nDeleted all rows from ";
                  ANSITerminal.print_string [ ANSITerminal.green ]
                    (db_name ^ "\n\n"))
          | "N"
          | "n" ->
              print_string "\nOperation cancelled\n\n"
          | _ ->
              ANSITerminal.print_string [ ANSITerminal.red ]
                "\nInvalid Decision.\n\n")
        else
          match
            find_row "database.json" db_name comp_field comp_val
          with
          | h :: t -> (
              ANSITerminal.print_string [ ANSITerminal.red ]
                ("\nThis action will delete "
                ^ Int.to_string
                    (List.length
                       (find_row "database.json" db_name comp_field
                          comp_val))
                ^ " rows. Proceed? (Y/N)\n> ");
              match String.lowercase_ascii (read_line ()) with
              | "Y"
              | "y" -> (
                  let safe_rows =
                    find_row "database.json" db_name comp_field comp_val
                  in
                  match
                    delete_rows_in_database "database.json" safe_rows
                      db_name
                  with
                  | exception _ ->
                      print_string
                        "\n\
                         Internal error. This line should never have \
                         triggered."
                  | _ ->
                      print_string
                        ("\nDeleted "
                        ^ Int.to_string (List.length safe_rows)
                        ^ " rows from ");
                      ANSITerminal.print_string [ ANSITerminal.green ]
                        (db_name ^ "\n\n"))
              | "N"
              | "n" ->
                  print_string "\nOperation cancelled\n\n"
              | _ ->
                  ANSITerminal.print_string [ ANSITerminal.red ]
                    "\nInvalid Decision.\n\n")
          | [] ->
              ANSITerminal.print_string [ ANSITerminal.red ] "\nError: ";
              print_string "no database rows match these parameters."
          | exception DatabaseNotFound dbn ->
              ANSITerminal.print_string [ ANSITerminal.red ] "\nError: ";
              print_string "no database ";
              ANSITerminal.print_string [ ANSITerminal.red ] dbn;
              print_string " exists.\n\n"
          | exception ValNotFound vn ->
              ANSITerminal.print_string [ ANSITerminal.red ] "\nError: ";
              print_string ("no fields match the value " ^ vn ^ ".\n\n")
          | exception _ ->
              ANSITerminal.print_string [ ANSITerminal.red ] "\nError: ";
              print_string
                ("no field " ^ comp_field ^ " exists in database");
              ANSITerminal.print_string [ ANSITerminal.green ] db_name;
              print_string ".\n\n")
    | Update (db_name, field_name, new_val, comp_field, comp_val) -> (
        if comp_field = "*" && comp_val = "*" then (
          update_all "database.json" db_name field_name new_val;
          print_string "\nAll values of database updated\n\n")
        else
          match
            find_row "database.json" db_name comp_field comp_val
          with
          | exception DatabaseNotFound dbn ->
              ANSITerminal.print_string [ ANSITerminal.red ] "\nError: ";
              print_string "database ";
              ANSITerminal.print_string [ ANSITerminal.red ] db_name;
              print_string " not found.\n\n"
          | exception ValNotFound value ->
              ANSITerminal.print_string [ ANSITerminal.red ] "\nError: ";
              print_string (comp_field ^ " : ");
              ANSITerminal.print_string [ ANSITerminal.red ] value;
              print_string " not found in database ";
              ANSITerminal.print_string [ ANSITerminal.green ] db_name;
              print_string ".\n\n"
          | exception _ ->
              ANSITerminal.print_string [ ANSITerminal.red ] "\nError: ";
              print_string
                ("no field " ^ comp_field ^ " exists in database ");
              ANSITerminal.print_string [ ANSITerminal.green ] db_name;
              print_string ".\n\n"
          | h :: t ->
              run_update_list (h :: t) db_name field_name new_val 0
          | [] ->
              ANSITerminal.print_string [ ANSITerminal.red ] "\nError: ";
              print_string (comp_field ^ " : ");
              ANSITerminal.print_string [ ANSITerminal.red ] comp_val;
              print_string " not found in database ";
              ANSITerminal.print_string [ ANSITerminal.green ] db_name;
              print_string ".\n\n")
    | List ->
        ANSITerminal.print_string [ ANSITerminal.red ] "\nDatabases:\n";
        print_string (get_db_names_list "database.json")
    | ListRows name -> begin
        match find_database "database.json" name with
        | exception DatabaseNotFound name ->
            ANSITerminal.print_string [ ANSITerminal.red ] "\nError: ";
            print_string "database ";
            ANSITerminal.print_string [ ANSITerminal.red ] name;
            print_string " not found.\n\n"
        | _ ->
            print_string "\nRows of database ";
            ANSITerminal.print_string [ ANSITerminal.red ] name;
            print_string ":\n";
            list_rows "database.json" name
      end
    | ListFields db_name -> begin
        match get_fields_list "database.json" db_name with
        | exception DatabaseNotFound name ->
            ANSITerminal.print_string [ ANSITerminal.red ] "\nError: ";
            print_string "database ";
            ANSITerminal.print_string [ ANSITerminal.red ] name;
            print_string " not found.\n\n"
        | _ ->
            print_string "\nFields of ";
            ANSITerminal.print_string [ ANSITerminal.green ] db_name;
            print_string ":\n";
            print_string
              (String.concat ", "
                 (get_fields_list "database.json" db_name));
            print_string "\n\n"
      end
    | Sort (db_name, sort_field) -> (
        match sort_rows "database.json" db_name sort_field with
        | exception DatabaseNotFound name ->
            ANSITerminal.print_string [ ANSITerminal.red ] "\nError: ";
            print_string "database ";
            ANSITerminal.print_string [ ANSITerminal.red ] name;
            print_string " not found.\n\n"
        | exception FieldNotFound fname ->
            ANSITerminal.print_string [ ANSITerminal.red ] "\nError: ";
            print_string "database ";
            ANSITerminal.print_string [ ANSITerminal.red ] fname;
            print_string " not found.\n\n"
        | _ ->
            print_string "\nDatabase ";
            ANSITerminal.print_string [ ANSITerminal.red ] db_name;
            print_string " successfully sorted.\n\n")
    | Quit ->
        ANSITerminal.print_string [ ANSITerminal.green ]
          "\nChanges have been saved. Exiting...\n\n\n";
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
    | exception UnsupportedType t ->
        ANSITerminal.print_string [ ANSITerminal.red ] "\nError! ";
        print_string "Field type: ";
        ANSITerminal.print_string [ ANSITerminal.red ] (String.trim t);
        print_string " is not supported.\n\n"
  done

(*Execute engine*)
let () = main ()
