open Yojson
open Yojson.Basic.Util

type db_list = { databases : Yojson.Basic.t }

exception DatabaseNotFound of string

exception ValNotFound of string

exception FieldNotFound of string

exception InvalidRow of string

exception InvalidShape

exception CannotConvertToNum

exception CannotConvertElement

exception CannotCompute

exception WrongType of (string * string)

let set_file_location (file : string) : string = "./data/" ^ file

(** File location, accessible via this function in the event the
    location changes. *)
let file_location = set_file_location "database.json"

let dbs_from_file (file : string) : Yojson.Basic.t =
  Yojson.Basic.from_file (set_file_location file)

let database_string (file : string) =
  dbs_from_file file |> Yojson.Basic.to_string

let write_to_file (file : string) (db : Yojson.Basic.t) =
  Yojson.Basic.to_file (set_file_location file) db

(** [convert_fields_help vals acc] is a recursive helper to
    [convert_fields fields]. *)
let rec convert_fields_help
    (fields_and_types : (string * string) list)
    (acc : string) : string =
  match fields_and_types with
  | h :: t ->
      if t = [] then
        convert_fields_help t
          (acc ^ "\"" ^ fst h ^ "\"" ^ ":" ^ "\"" ^ snd h ^ "\"")
      else
        convert_fields_help t
          (acc ^ "\"" ^ fst h ^ "\"" ^ ":" ^ "\"" ^ snd h ^ "\"" ^ ",")
  | [] -> acc ^ "}"

(** [convert_fields fields] converts a list of values [vals] into a json
    string. Example: [convert_vals (\["name","id","location"\]) ""]
    gives "{"name" : "", "id" : "","location" : ""}" *)
let convert_fields (fields_and_types : (string * string) list) : string
    =
  convert_fields_help fields_and_types "{"

let splice_outer_parens (dbm : string) =
  let spliced = String.sub dbm 1 (String.length dbm - 2) in
  if String.length spliced > 0 then spliced ^ "," else spliced

let add_database
    (file : string)
    (name : string)
    (fields_types : (string * string) list) =
  let str =
    "{"
    ^ splice_outer_parens (database_string file)
    ^ "\"" ^ name ^ "\"" ^ ":["
    ^ convert_fields fields_types
    ^ "]}"
  in
  let file_out = open_out (set_file_location file) in
  Printf.fprintf file_out "%s" str;
  close_out file_out

let add_database_t
    (file : string)
    (name : string)
    (values : Yojson.Basic.t) =
  let str =
    "{"
    ^ splice_outer_parens (database_string file)
    ^ "\"" ^ name ^ "\"" ^ ":"
    ^ Yojson.Basic.to_string values
    ^ "}"
  in
  let file_out = open_out (set_file_location file) in
  Printf.fprintf file_out "%s" str;
  close_out file_out

let clear_database_file (file : string) =
  let str = "{}" in
  let file_out = open_out (set_file_location file) in
  Printf.fprintf file_out "%s" str;
  close_out file_out

(** [delete_database_helper name database] removes the database with
    name [name] from the list of databases [databases]. Raises:
    DatabaseNotFound if database is not in system *)
let delete_database_helper
    (name : string)
    (databases : (string * Basic.t) list) =
  let rec delete_aux name databases deleted acc =
    match databases with
    | [] -> if deleted then acc else raise (DatabaseNotFound name)
    | ((str, values) as h) :: t ->
        if str = name then delete_aux name t true acc
        else delete_aux name t deleted (acc @ [ h ])
  in
  delete_aux name databases false []

(** [write_all_databases_helper file databases] adds the databases in
    [databases] to [file]. *)
let rec write_all_databases_helper
    (file : string)
    (databases : (string * Basic.t) list) =
  match databases with
  | [] -> ()
  | (name, values) :: t ->
      let _ = add_database_t file name values in
      write_all_databases_helper file t

(** [write_all_databases file databases] writes all the databases inside
    [databases] to the file [file]. *)
let write_all_databases
    (file : string)
    (databases : (string * Basic.t) list) =
  let _ = clear_database_file file in
  write_all_databases_helper file databases

let delete_database (file : string) (name : string) =
  let database = Yojson.Basic.Util.to_assoc (dbs_from_file file) in
  let list_after_removal = delete_database_helper name database in
  write_all_databases file list_after_removal

(** [find_database_helper database_name database_list] returns the
    values of the database with name [database_name] in the list of
    database [database_list]. Raises: [DatabaseNotFound database_name]
    if the [database_name] is not in [database_list]. *)
let rec find_database_helper
    (database_name : string)
    (database_list : (string * Basic.t) list) =
  match database_list with
  | [] -> raise (DatabaseNotFound database_name)
  | (name, values) :: t ->
      if name = database_name then values
      else find_database_helper database_name t

let rec find_database (file : string) (database_name : string) =
  let database_list = Yojson.Basic.Util.to_assoc (dbs_from_file file) in
  find_database_helper database_name database_list

(** [get_fields_types_list file db_name] returns the list of fields and
    their types in [db_name] in [file]. *)
let get_fields_types_list (file : string) (db_name : string) =
  let database_row_list =
    find_database file db_name |> Yojson.Basic.Util.to_list
  in
  List.hd database_row_list
  |> Yojson.Basic.Util.to_assoc
  |> List.map (fun (x, y) -> (x, Yojson.Basic.to_string y))

(** [get_field_type_help db_name field] returns the type of field
    [field] in database [db_name]*)
let get_field_type_help file db_name field =
  let pairs = get_fields_types_list file db_name in
  List.assoc field pairs

(** [get_fields_list file db_name] returns the fields of [db_name] in
    [file]. *)
let get_fields_list (file : string) (db_name : string) =
  let fields_types = get_fields_types_list file db_name in
  List.map (fun (x, y) -> x) fields_types

(** [write_all_databases_one_cleared file database_name databases field_list]
    writes all the databases in database list [databases] to file [file]
    except clears the database [database_name], leaving it with only
    it's shape element. *)
let rec write_all_databases_one_cleared
    (file : string)
    (database_name : string)
    (databases : (string * Basic.t) list)
    (fields_types : (string * string) list) =
  match databases with
  | [] -> ()
  | (name, values) :: t ->
      let _ =
        if database_name = name then
          add_database file database_name fields_types
        else add_database_t file name values
      in
      write_all_databases_one_cleared file database_name t fields_types

let clear_database (file : string) (name : string) =
  let fields_types = get_fields_types_list file name in
  let databases = Yojson.Basic.Util.to_assoc (dbs_from_file file) in
  let _ = clear_database_file file in
  write_all_databases_one_cleared file name databases fields_types

(** [find_value_helper value_list value_name] returns the value
    associated with the value [value_name] in the list of values
    [value_list] in a database. Raises: [ValNotFound val_name] if the
    [value_name] is not in [value_list]. *)
let rec find_value_helper
    (value_list : (string * Basic.t) list)
    (value_name : string) =
  match value_list with
  | [] -> raise (ValNotFound value_name)
  | (name, values) :: t ->
      if name = value_name then Yojson.Basic.to_string values
      else find_value_helper t value_name

let find_value_in_database
    (file : string)
    (database_name : string)
    (value_name : string) =
  let db_entry_list =
    Yojson.Basic.Util.to_list (find_database file database_name)
    |> List.tl |> List.rev
  in
  let rec scan_database entry_list acc =
    match entry_list with
    | h :: t ->
        if List.length t > 0 then
          scan_database t ", "
          ^ find_value_helper (Yojson.Basic.Util.to_assoc h) value_name
          ^ acc
        else
          scan_database t
            (find_value_helper (Yojson.Basic.Util.to_assoc h) value_name
            ^ acc)
    | [] ->
        if String.length acc > 0 then "\n" ^ acc
        else "\n  - No results found. Database is empty."
  in
  scan_database db_entry_list ""

let get_db_names_list (file : string) =
  let list = Yojson.Basic.Util.to_assoc (dbs_from_file file) in
  let rec get_names_rec acc list =
    match list with
    | h :: t -> begin
        match h with
        | x, y -> get_names_rec (acc ^ "  -  " ^ x ^ "\n") t
      end
    | [] ->
        if String.length acc > 0 then acc ^ "\n"
        else "\n  -Database management system is empty.\n\n"
  in
  get_names_rec "" list

let gen_whitespace len =
  if len <= 0 then ""
  else
    let rec gen_aux len acc =
      if len != 0 then gen_aux (len - 1) (acc ^ " ") else acc
    in
    gen_aux len ""

(** [gen_horiz_line len upper is_bottom]generates a horizontal line
    ————...[len] if [upper], ‾‾‾‾...[len] if [is_bottom] or ____...[len]
    otherwise. *)
let gen_horiz_line len upper is_bottom =
  let sym = if is_bottom then "‾" else if upper then "—" else "_" in
  if len <= 0 then ""
  else
    let rec gen_aux len acc =
      if len != 0 then gen_aux (len - 1) (acc ^ sym) else acc
    in
    gen_aux len ""

(** [get_row_char_len field_longest_list] combines the the lengths of
    the longest entries in each field and the lengths of their |
    separators*)
let get_row_char_len field_longest_list =
  let rec row_len_aux field_longest_list sum_acc =
    match field_longest_list with
    | (field, longest) :: t -> row_len_aux t (sum_acc + longest + 1)
    | [] -> sum_acc + 1
  in
  row_len_aux field_longest_list 0

(**[get_longest_val field_value_list acc] returns the longest string
   length value in *)
let get_longest_val file db_name rows field =
  let rec get_lval_aux rows field is_first acc =
    match rows with
    | val_list :: t ->
        let len =
          if is_first = true then
            String.length field
            + String.length (get_field_type_help file db_name field)
            + 2
          else
            String.length
              (Yojson.Basic.to_string
                 (List.assoc field
                    (Yojson.Basic.Util.to_assoc val_list)))
        in
        if len > acc then get_lval_aux t field false len
        else get_lval_aux t field false acc
    | [] -> acc
  in
  get_lval_aux rows field true 0

(**[gen_field_longval_pairs rows fields acc] generates a list of (field,
   length of longest value in that field) for all fields in database.*)
let rec gen_field_longval_pairs file db_name rows fields acc =
  match fields with
  | field :: t ->
      gen_field_longval_pairs file db_name rows t
        (acc @ [ (field, get_longest_val file db_name rows field) ])
  | [] -> acc

(** [list_vals_rec value_list length_scale_list is_first is_first_sub acc]
    adds the field name and value name in [value_list] to [acc] as a
    string representation with added whitespace to evenly align rows. *)
let rec list_vals_rec
    value_list
    length_scale_list
    is_first
    is_first_sub
    acc =
  match value_list with
  | (name, value) :: t ->
      if is_first = true then
        let len =
          List.assoc name length_scale_list
          - String.length name
          - String.length (Yojson.Basic.Util.to_string value)
          - 2
        in
        let left_len = len / 2 in
        let right_len = len - left_len in
        if is_first_sub = true then
          list_vals_rec t length_scale_list true false
            (acc ^ "|" ^ gen_whitespace left_len ^ name ^ ": "
            ^ Yojson.Basic.Util.to_string value
            ^ gen_whitespace right_len
            ^ "|")
        else
          list_vals_rec t length_scale_list true false
            (acc ^ gen_whitespace left_len ^ name ^ ": "
            ^ Yojson.Basic.Util.to_string value
            ^ gen_whitespace right_len
            ^ "|")
      else if is_first_sub = true then
        let len =
          List.assoc name length_scale_list
          - String.length (Yojson.Basic.Util.to_string value)
        in
        let left_len = len / 2 in
        let right_len = len - left_len in
        list_vals_rec t length_scale_list false false
          (acc ^ "|" ^ gen_whitespace left_len
          ^ Yojson.Basic.Util.to_string value
          ^ gen_whitespace right_len
          ^ "|")
      else
        let len =
          List.assoc name length_scale_list
          - String.length (Yojson.Basic.Util.to_string value)
        in
        let left_len = len / 2 in
        let right_len = len - left_len in
        list_vals_rec t length_scale_list false false
          (acc ^ gen_whitespace left_len
          ^ Yojson.Basic.Util.to_string value
          ^ gen_whitespace right_len
          ^ "|")
  | [] -> acc

(** [list_rows_rec rows acc] goes through the list of rows [rows] and
    adds each string representation of the row to [acc] with a newline
    separator. *)
let rec list_rows_rec
    rows
    fields_list
    length_scalings
    is_first
    row_acc
    header =
  match rows with
  | val_list :: t ->
      if is_first = true then
        list_rows_rec t fields_list length_scalings false row_acc
          (list_vals_rec
             (Yojson.Basic.Util.to_assoc val_list)
             length_scalings true true "\n")
      else
        list_rows_rec t fields_list length_scalings false
          (row_acc
          ^ list_vals_rec
              (Yojson.Basic.Util.to_assoc val_list)
              length_scalings false true "\n")
          header
  | [] ->
      if String.length row_acc > 0 then (header, row_acc)
      else ("\n - Database is empty.\n\n", "")

let list_rows (file : string) (database_name : string) =
  let fields_list = get_fields_list file database_name in
  let database_row_list =
    find_database file database_name |> Yojson.Basic.Util.to_list
  in
  let length_scalings =
    gen_field_longval_pairs file database_name database_row_list
      fields_list []
  in
  let header_and_rows =
    list_rows_rec database_row_list fields_list length_scalings true ""
      ""
  in
  if snd header_and_rows != "" then (
    ANSITerminal.print_string [ ANSITerminal.red ]
      (gen_horiz_line (get_row_char_len length_scalings) false false);

    ANSITerminal.print_string [ ANSITerminal.red ] (fst header_and_rows);
    print_string "\n";

    ANSITerminal.print_string [ ANSITerminal.red ]
      ("|"
      ^ gen_horiz_line (get_row_char_len length_scalings - 2) true false
      ^ "|");

    print_string (snd header_and_rows);
    print_string "\n";
    print_string
      (gen_horiz_line (get_row_char_len length_scalings) true true);
    print_string "\n")
  else print_string (fst header_and_rows)

(** [replace_db_in_dbms database_list database_name new_database]
    creates a string representing a json (without the first "{") with
    [new_database] in place for database [database_name] in
    [database_list]. *)
let rec replace_db_in_dbms
    (database_list : (string * Basic.t) list)
    (database_name : string)
    (new_database : string) =
  match database_list with
  | [] -> "}"
  | (name, values) :: t ->
      if name = database_name then
        "," ^ "\"" ^ name ^ "\": " ^ new_database
        ^ replace_db_in_dbms t database_name new_database
      else
        "," ^ "\"" ^ name ^ "\": "
        ^ Yojson.Basic.to_string values
        ^ replace_db_in_dbms t database_name new_database

(** [write_new_db file db_name db_list table_list template] writes the
    new database [db_name] in [file] after adding an element to the
    database using [db_list], [table_list], and [template]. *)
let write_new_db file db_name db_list table_list template =
  if List.length table_list < 3 then
    let str = template ^ "}]" in
    let new_str =
      let after_helper = replace_db_in_dbms db_list db_name str in
      "{" ^ String.sub after_helper 1 (String.length after_helper - 1)
    in
    write_to_file file (Yojson.Basic.from_string new_str)
  else
    let str = template ^ "}" ^ String.concat "}" (List.tl table_list) in
    let new_str =
      let after_helper = replace_db_in_dbms db_list db_name str in
      "{" ^ String.sub after_helper 1 (String.length after_helper - 1)
    in
    write_to_file file (Yojson.Basic.from_string new_str)

let add_element_to_database
    (file : string)
    (database_name : string)
    (value_name : string) =
  let database_list = Yojson.Basic.Util.to_assoc (dbs_from_file file) in
  let database_string =
    find_database file database_name |> Yojson.Basic.to_string
  in
  let table_list = String.split_on_char '}' database_string in
  let template = List.hd table_list ^ ",\"" ^ value_name ^ "\":\"\"" in
  write_new_db file database_name database_list table_list template

(** [string_value_adder table_list value_name] adds the element
    [value_name] to all the rows in the database represented by a list
    of strings [table_list]. *)
let rec string_value_adder
    (table_list : string list)
    (field_name : string)
    (field_type : string)
    (value_name : string)
    (first_pass : bool) =
  match table_list with
  | [] -> ""
  | h :: t ->
      if h = "]" then "]"
      else if first_pass then
        h ^ ",\"" ^ field_name ^ "\":" ^ "\"" ^ field_type ^ "\"" ^ "}"
        ^ string_value_adder t field_name field_type value_name false
      else
        h ^ ",\"" ^ field_name ^ "\":" ^ "\"" ^ value_name ^ "\"" ^ "}"
        ^ string_value_adder t field_name field_type value_name false

(** [get_field_type file db_name field_name] returns a string of the
    type of field [field_name] in database db_name in file file*)
let get_field_type
    (file : string)
    (db_name : string)
    (field_name : string) =
  let db_string =
    find_database file db_name |> Yojson.Basic.to_string
  in
  let table_list = String.split_on_char '}' db_string in
  let field_type_quotes =
    String.sub (List.hd table_list) 1
      (String.length (List.hd table_list) - 1)
    ^ "}"
    |> Yojson.Basic.from_string |> Yojson.Basic.Util.to_assoc
    |> List.assoc field_name |> Yojson.Basic.to_string
  in
  String.sub field_type_quotes 1 (String.length field_type_quotes - 2)

(** [type_check value field_type] checks whether [value] is of type
    [field_type], returning true if so, and a WrongType exception if
    not. *)
let type_check (value : string) (field_type : string) =
  if value != "" then
    match field_type with
    | "int"
    | "\"int\"" -> (
        try
          let _ = int_of_string value in
          true
        with
        | _ -> raise (WrongType (value, field_type)))
    | "bool"
    | "\"bool\"" ->
        if
          String.lowercase_ascii value = "true"
          || String.lowercase_ascii value = "false"
        then true
        else raise (WrongType (value, field_type))
    | "float"
    | "\"float\"" -> (
        try
          let _ = float_of_string value in
          true
        with
        | _ -> raise (WrongType (value, field_type)))
    | "string"
    | "\"string\"" ->
        true
    | _ -> raise (WrongType (value, field_type))
  else true

let add_field_to_all_rows
    (file : string)
    ?val_name:(value_name = "")
    (database_name : string)
    (field_name : string)
    (field_type : string) =
  let _ =
    if String.length value_name > 0 then
      type_check value_name field_type
    else true
  in
  let database_list = Yojson.Basic.Util.to_assoc (dbs_from_file file) in
  let database_string =
    find_database file database_name |> Yojson.Basic.to_string
  in
  let table_list = String.split_on_char '}' database_string in
  let new_database =
    string_value_adder table_list field_name field_type value_name true
  in
  let new_str =
    let after_helper =
      replace_db_in_dbms database_list database_name new_database
    in
    "{" ^ String.sub after_helper 1 (String.length after_helper - 1)
  in
  write_to_file file (Yojson.Basic.from_string new_str)

(** [update_element_helper value_list value_name new_value] creates a
    string representing a table row with [new_value] in place for
    database [field_name] in [value_list]. *)
let rec update_element_helper
    (value_list : (string * Basic.t) list)
    (field_name : string)
    (field_type : string)
    (new_value : string)
    (updated : bool) =
  let _ =
    if String.length new_value > 0 then type_check new_value field_type
    else true
  in
  match value_list with
  | [] -> if updated then "}" else raise (FieldNotFound field_name)
  | (name, values) :: t ->
      if name = field_name then
        "," ^ "\"" ^ name ^ "\": \"" ^ new_value ^ "\""
        ^ update_element_helper t field_name field_type new_value true
      else
        "," ^ "\"" ^ name ^ "\": "
        ^ Yojson.Basic.to_string values
        ^ update_element_helper t field_name field_type new_value
            updated

(** [row_finder table_list element_row] finds the [element_row]th row in
    [table_list]. *)
let rec row_finder (table_list : string list) (element_row : int) =
  match table_list with
  | [] -> raise (InvalidRow "Row not in database")
  | h :: t ->
      if element_row = 0 then h else row_finder t (element_row - 1)

(** [update_row_in_database table_list element_row new_row] formats the
    database with the rows in [table_list] with [new_row] inserted into
    [element_row]'s position in the row. *)
let rec update_row_in_database
    (table_list : string list)
    (element_row : int)
    (new_row : string) =
  match table_list with
  | [] -> ""
  | h :: t ->
      if element_row = 0 then
        "}" ^ new_row
        ^ update_row_in_database t (element_row - 1) new_row
      else "}" ^ h ^ update_row_in_database t (element_row - 1) new_row

(** [new_database field_name new_value element_row table_list] creates a
    new string representation of a database with [new_value] in for
    [field_name] in [element_row] from [table_list]. *)
let new_database field_name new_value element_row table_list =
  let wanted_row = row_finder table_list element_row in
  let field_type_quotes =
    String.sub (List.hd table_list) 1
      (String.length (List.hd table_list) - 1)
    ^ "}"
    |> Yojson.Basic.from_string |> Yojson.Basic.Util.to_assoc
    |> List.assoc field_name |> Yojson.Basic.to_string
  in
  let field_type =
    String.sub field_type_quotes 1 (String.length field_type_quotes - 2)
  in
  let value_list =
    String.sub wanted_row 1 (String.length wanted_row - 1) ^ "}"
    |> Yojson.Basic.from_string |> Yojson.Basic.Util.to_assoc
  in
  let unclean_row =
    update_element_helper value_list field_name field_type new_value
      false
  in
  let new_unclean_row =
    String.sub unclean_row 0 (String.length unclean_row - 1)
  in
  let new_row =
    ",{"
    ^ String.sub new_unclean_row 1 (String.length new_unclean_row - 1)
  in
  let new_db_before_format =
    update_row_in_database table_list element_row new_row
  in
  String.sub new_db_before_format 1
    (String.length new_db_before_format - 1)

let update_value
    (file : string)
    (db_name : string)
    (field_name : string)
    (element_row : int)
    (new_value : string) =
  if element_row < 1 then raise (InvalidRow "Row cannot be less than 1")
  else
    let db_list = Yojson.Basic.Util.to_assoc (dbs_from_file file) in
    let db_string =
      find_database file db_name |> Yojson.Basic.to_string
    in
    let table_list = String.split_on_char '}' db_string in
    if element_row > List.length table_list - 2 then
      raise (InvalidRow "Row not in database")
    else
      let new_db =
        new_database field_name new_value element_row table_list
      in
      let new_str =
        let after_helper = replace_db_in_dbms db_list db_name new_db in
        "{" ^ String.sub after_helper 1 (String.length after_helper - 1)
      in
      write_to_file file (Yojson.Basic.from_string new_str)

(** [find_row_no_exception file database_name field_name value_name]
    computes [find row file database_name field_name value_name] when
    there is no exception raised. *)
let find_row_no_exception file database_name field_name value_name =
  let database_list =
    dbs_from_file file |> member database_name
    |> Yojson.Basic.Util.to_list |> List.tl
    |> List.map Yojson.Basic.Util.to_assoc
  in
  let rec find_val_row db_list acc acc_list =
    match db_list with
    | h :: t ->
        if
          try
            Yojson.Basic.Util.to_string (List.assoc field_name h)
            = value_name
          with
          | Not_found -> raise (FieldNotFound field_name)
        then find_val_row t (acc + 1) (acc_list @ [ acc ])
        else find_val_row t (acc + 1) acc_list
    | [] ->
        if List.length acc_list = 0 then raise (ValNotFound value_name)
        else acc_list
  in
  find_val_row database_list 1 []

let find_row
    (file : string)
    (database_name : string)
    (field_name : string)
    (value_name : string) =
  match
    dbs_from_file file |> member database_name
    |> Yojson.Basic.Util.to_list |> List.tl
    |> List.map Yojson.Basic.Util.to_assoc
  with
  | exception _ -> raise (DatabaseNotFound database_name)
  | _ -> find_row_no_exception file database_name field_name value_name

(** [first_n_rows table_list number_rows] returns the first
    [number_rows] in [table_list]. *)
let rec first_n_rows (table_list : string list) (number_rows : int) =
  match table_list with
  | [] -> []
  | h :: t ->
      if number_rows > 0 then h :: first_n_rows t (number_rows - 1)
      else []

(** [update_all_helper list_of_element_rows file database_name value_name new_value]
    applies [update_element file database_name value_name h new_value]
    to each element h in the element_row list [list_of_element_rows]. *)
let rec update_all_helper
    (list_of_element_rows : int list)
    (file : string)
    (database_name : string)
    (field_name : string)
    (new_value : string) =
  match list_of_element_rows with
  | [] -> ()
  | h :: t ->
      update_value file database_name field_name h new_value;
      update_all_helper t file database_name field_name new_value

let update_all
    (file : string)
    (database_name : string)
    (field_name : string)
    (new_value : string) =
  let field_type = get_field_type_help file database_name field_name in
  let _ = type_check new_value field_type in
  let database_string =
    find_database file database_name |> Yojson.Basic.to_string
  in
  let table_list = List.tl (String.split_on_char '}' database_string) in
  let number_rows = List.length table_list - 1 in
  let list_of_element_rows = List.init number_rows (fun x -> x + 1) in
  update_all_helper list_of_element_rows file database_name field_name
    new_value

(** [delete_rows_in_database_iter row_list del_rows acc] removes the
    rows matching row numbers in [del_rows] from [row_list]. Requires
    del_rows is a sorted list. *)
let rec delete_rows_in_database_iter
    (row_list : string list)
    (del_rows : int list)
    (acc : int) =
  match row_list with
  | h :: t ->
      if del_rows != [] && acc = List.hd del_rows then
        if t != [] then
          delete_rows_in_database_iter t (List.tl del_rows) (acc + 1)
        else
          "}"
          ^ delete_rows_in_database_iter t (List.tl del_rows) (acc + 1)
      else "}" ^ h ^ delete_rows_in_database_iter t del_rows (acc + 1)
  | [] -> ""

let rec delete_rows_in_database
    (file : string)
    (del_rows : int list)
    (db_name : string) =
  let database_list = Yojson.Basic.Util.to_assoc (dbs_from_file file) in
  let database_string =
    find_database file db_name |> Yojson.Basic.to_string
  in
  let row_list = String.split_on_char '}' database_string in
  let new_bf_format =
    delete_rows_in_database_iter row_list del_rows 0
  in
  let new_database =
    String.sub new_bf_format 1 (String.length new_bf_format - 1)
  in
  let new_str =
    let after_helper =
      replace_db_in_dbms database_list db_name new_database
    in
    "{" ^ String.sub after_helper 1 (String.length after_helper - 1)
  in
  write_to_file file (Yojson.Basic.from_string new_str)

(** [create_row file field_val_pairs db_name] creates a string
    representation of a row that contains [field_val_pairs] in
    [db_name]. *)
let rec create_row
    file
    (field_val_pairs : (string * string) list)
    (db_name : string) =
  match field_val_pairs with
  | (field, value) :: t ->
      let _ =
        if String.length (String.trim value) != 0 then
          type_check (String.trim value)
            (get_field_type file db_name field)
        else true
      in
      if t = [] then
        "\"" ^ String.trim field ^ "\"" ^ ": " ^ "\""
        ^ String.trim value ^ "\""
        ^ create_row file t db_name
      else
        "\"" ^ String.trim field ^ "\"" ^ ": " ^ "\""
        ^ String.trim value ^ "\"" ^ ","
        ^ create_row file t db_name
  | [] -> "}]"

let add_row (file : string) (db_name : string) (values : string list) =
  let database_list = Yojson.Basic.Util.to_assoc (dbs_from_file file) in
  let database_string =
    find_database file db_name |> Yojson.Basic.to_string
  in
  let trimmed =
    String.sub database_string 0 (String.length database_string - 1)
  in
  let field_names = get_fields_list file db_name in
  if List.length field_names != List.length values then
    raise InvalidShape
  else
    let updated_db =
      trimmed ^ ",{"
      ^ create_row file (List.combine field_names values) db_name
    in
    let new_str =
      let after_helper =
        replace_db_in_dbms database_list db_name updated_db
      in
      "{" ^ String.sub after_helper 1 (String.length after_helper - 1)
    in
    write_to_file file (Yojson.Basic.from_string new_str)

(** [pair_list_no_field pairs remove_field changed acc] returns a new
    pair list from the old pair list [pairs] with [remove_field] changed
    and used [changed] and [acc] to keep track of progress. *)
let rec pair_list_no_field pairs remove_field changed acc =
  match pairs with
  | (name, value) :: t ->
      if name = remove_field then
        pair_list_no_field t remove_field true acc
      else
        pair_list_no_field t remove_field changed
          (acc @ [ (name, value) ])
  | [] ->
      if changed = false then raise (FieldNotFound remove_field)
      else acc

(** [delete_from_row row_entries field_name is_first acc] deletes the
    [field_name] from [row_entries] using [is_first] and [acc] to keep
    track of progress. *)
let rec delete_from_row row_entries field_name is_first acc =
  match row_entries with
  | (name, value) :: t ->
      if is_first = true then
        delete_from_row t field_name false acc
        ^ "\"" ^ name ^ "\"" ^ ": " ^ "\""
        ^ String.sub
            (Yojson.Basic.to_string value)
            1
            (String.length (Yojson.Basic.to_string value) - 2)
        ^ "\""
      else
        delete_from_row t field_name false acc
        ^ "\"" ^ name ^ "\"" ^ ": " ^ "\""
        ^ String.sub
            (Yojson.Basic.to_string value)
            1
            (String.length (Yojson.Basic.to_string value) - 2)
        ^ "\","
  | [] -> acc

let delete_field
    (file : string)
    (db_name : string)
    (field_name : string) =
  let row_list =
    try
      dbs_from_file file |> member db_name |> Yojson.Basic.Util.to_list
      |> List.map Yojson.Basic.Util.to_assoc
    with
    | _ -> raise (DatabaseNotFound db_name)
  in
  let rec delete_field_aux row_list field_name is_first list_acc =
    match row_list with
    | h :: t ->
        if is_first then
          delete_field_aux t field_name false
            (list_acc ^ "{"
            ^ delete_from_row
                (List.rev (pair_list_no_field h field_name false []))
                field_name true ""
            ^ "}")
        else
          delete_field_aux t field_name false
            (list_acc ^ ", " ^ "{"
            ^ delete_from_row
                (List.rev (pair_list_no_field h field_name false []))
                field_name true ""
            ^ "}")
    | [] -> list_acc
  in
  let new_db = delete_field_aux row_list field_name true "" in
  let _ = print_string "" in
  let string_db = "[" ^ new_db ^ "]" in
  let db_list = Yojson.Basic.Util.to_assoc (dbs_from_file file) in
  let penult_db = replace_db_in_dbms db_list db_name string_db in
  let final_db =
    "{" ^ String.sub penult_db 1 (String.length penult_db - 1)
  in
  write_to_file file (Yojson.Basic.from_string final_db)

(** [constructed_sorted_db row_list pairs header db_acc is_first]
    creates a string for the sorted databases containing rows [row_list]
    and [pairs] with [header] using [db_acc] and [is_first]. *)
let rec construct_sorted_db row_list pairs header db_acc is_first =
  match pairs with
  | (index, value) :: t ->
      let current_row = row_finder row_list index in
      if is_first = true then
        construct_sorted_db row_list t header (db_acc ^ current_row)
          false
      else
        construct_sorted_db row_list t header
          (db_acc ^ ", " ^ current_row)
          false
  | [] ->
      if is_first then "[" ^ header ^ "]"
      else "[" ^ header ^ "," ^ db_acc ^ "]"

(** [row_comp_func x y conv_func] compares [x] and [y] using
    [conv_func]. *)
let row_comp_func x y conv_func =
  let v1, v2 = (String.trim (snd x), String.trim (snd y)) in
  match (String.length v1, String.length v2) with
  | 0, 0 -> 0
  | 0, _ -> -1
  | _, 0 -> 1
  | _, _ -> compare (conv_func v1) (conv_func v2)

let sort_rows (file : string) (db_name : string) (sort_field : string) =
  let row_list =
    try
      dbs_from_file file |> member db_name |> Yojson.Basic.Util.to_list
      |> List.tl
      |> List.map Yojson.Basic.Util.to_assoc
    with
    | _ -> raise (DatabaseNotFound db_name)
  in
  let rec get_row_val_pair row_list rownum_acc list_acc =
    try
      match row_list with
      | h :: t ->
          get_row_val_pair t (rownum_acc + 1)
            (list_acc
            @ [
                ( rownum_acc,
                  Yojson.Basic.Util.to_string (List.assoc sort_field h)
                );
              ])
      | [] -> list_acc
    with
    | _ -> raise (FieldNotFound sort_field)
  in
  let header =
    dbs_from_file file |> member db_name |> Yojson.Basic.Util.to_list
    |> List.hd |> Yojson.Basic.to_string
  in
  let str_row_list =
    dbs_from_file file |> member db_name |> Yojson.Basic.Util.to_list
    |> List.tl
    |> List.map Yojson.Basic.to_string
  in
  let pairs = get_row_val_pair row_list 0 [] in
  let sort_type = get_field_type file db_name sort_field in
  let sorted_row_nums =
    match sort_type with
    | "int" ->
        List.sort (fun x y -> row_comp_func x y int_of_string) pairs
    | "bool" ->
        List.sort (fun x y -> row_comp_func x y bool_of_string) pairs
    | "float" ->
        List.sort (fun x y -> row_comp_func x y float_of_string) pairs
    | _ -> List.sort (fun x y -> row_comp_func x y (fun x -> x)) pairs
  in
  let sorted_db_string =
    construct_sorted_db str_row_list sorted_row_nums header "" true
  in
  let db_list = Yojson.Basic.Util.to_assoc (dbs_from_file file) in
  let penult_db = replace_db_in_dbms db_list db_name sorted_db_string in
  let final_db =
    "{" ^ String.sub penult_db 1 (String.length penult_db - 1)
  in
  write_to_file file (Yojson.Basic.from_string final_db)

let sum_of_field
    (file : string)
    (db_name : string)
    (field_name : string) =
  let field_type = get_field_type file db_name field_name in
  let value_list =
    dbs_from_file file |> member db_name |> Yojson.Basic.Util.to_list
    |> List.tl
    |> List.map Yojson.Basic.Util.to_assoc
    |> List.map (fun x ->
           let ext_str =
             Yojson.Basic.to_string (List.assoc field_name x)
           in
           if String.length ext_str > 0 then
             String.sub ext_str 1 (String.length ext_str - 2)
           else ext_str)
  in
  match field_type with
  | "int" ->
      string_of_int
        (List.fold_left
           (fun acc x ->
             let num = if String.length x > 0 then x else "0" in
             acc + int_of_string num)
           0 value_list)
  | "bool" ->
      string_of_bool
        (List.fold_left
           (fun acc x ->
             let bl = if String.length x > 0 then x else "false" in
             acc || bool_of_string bl)
           false value_list)
  | "float" ->
      string_of_float
        (List.fold_left
           (fun acc x ->
             let num = if String.length x > 0 then x else "0." in
             acc +. float_of_string num)
           0. value_list)
  | _ ->
      let fin_str =
        String.trim
          (List.fold_left
             (fun acc x ->
               let str = if String.length x > 0 then x else " " in
               acc ^ str ^ ", ")
             "" value_list)
      in
      let buf = if String.sub fin_str 0 1 = "," then " " else "" in
      "\"" ^ buf
      ^ String.sub fin_str 0 (String.length fin_str - 1)
      ^ "\""

let mean_of_field
    (file : string)
    (db_name : string)
    (field_name : string) =
  let field_type = get_field_type file db_name field_name in
  let value_list =
    dbs_from_file file |> member db_name |> Yojson.Basic.Util.to_list
    |> List.tl
    |> List.map Yojson.Basic.Util.to_assoc
    |> List.map (fun x ->
           let ext_str =
             Yojson.Basic.to_string (List.assoc field_name x)
           in
           if String.length ext_str > 0 then
             String.sub ext_str 1 (String.length ext_str - 2)
           else ext_str)
  in
  match field_type with
  | "int" ->
      string_of_float
        (List.fold_left
           (fun acc x ->
             let num = if String.length x > 0 then x else "0" in
             acc +. float_of_string num)
           0. value_list
        /. float_of_int (List.length value_list))
  | "float" ->
      string_of_float
        (List.fold_left
           (fun acc x ->
             let num = if String.length x > 0 then x else "0." in
             acc +. float_of_string num)
           0. value_list
        /. float_of_int (List.length value_list))
  | _ -> raise (WrongType ("", field_type))
