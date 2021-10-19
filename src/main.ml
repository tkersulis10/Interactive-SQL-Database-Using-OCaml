open Yojson
open Yojson.Basic.Util

type db_list = { databases : Yojson.Basic.t }

exception NotFound of string

let set_file_location (file : string) : string = "./data/" ^ file

(** File location, accessible via this function in the event the
    location changes. *)
let file_location = set_file_location "database.json"

let dbs_from_file (file : string) : Yojson.Basic.t =
  Yojson.Basic.from_file (set_file_location file)

let database_list (file : string) =
  dbs_from_file file |> Yojson.Basic.to_string

let write_to_file (file : string) (db : Yojson.Basic.t) =
  Yojson.Basic.to_file (set_file_location file) db

(** [convert_vals_help vals acc] is a recursive helper to
    [convert_vals vals]. *)
let rec convert_vals_help (vals : string list) (acc : string) : string =
  match vals with
  | h :: t ->
      if t = [] then
        convert_vals_help t (acc ^ "\"" ^ h ^ "\"" ^ ":" ^ "\"\"")
      else
        convert_vals_help t (acc ^ "\"" ^ h ^ "\"" ^ ":" ^ "\"\"" ^ ",")
  | [] -> acc ^ "}"

(** [convert_vals vals] converts a list of values [vals] into a json
    string. Example: [convert_vals (\["name","id","location"\]) ""]
    gives "{"name" : "", "id" : "","location" : ""}" *)
let convert_vals (vals : string list) : string =
  convert_vals_help vals "{"

let splice_outer_parens (dbm : string) =
  let spliced = String.sub dbm 1 (String.length dbm - 2) in
  if String.length spliced > 0 then spliced ^ "," else spliced

let add_database (file : string) (name : string) (values : string list)
    =
  let str =
    "{"
    ^ splice_outer_parens (database_list file)
    ^ "\"" ^ name ^ "\"" ^ ":[" ^ convert_vals values ^ "]}"
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
    ^ splice_outer_parens (database_list file)
    ^ "\"" ^ name ^ "\"" ^ ":["
    ^ Yojson.Basic.to_string values
    ^ "]}"
  in
  let file_out = open_out (set_file_location file) in
  Printf.fprintf file_out "%s" str;
  close_out file_out

let clear_database_file (file : string) =
  let str = "{}" in
  let file_out = open_out (set_file_location file) in
  Printf.fprintf file_out "%s" str;
  close_out file_out

(** [clear_database_helper name database] removes the database with name
    [name] from the list of databases [databases]. If name is not in
    databases, then [clear_database_helper name database] returns
    [databases]. *)
let rec clear_database_helper
    (name : string)
    (databases : (string * Basic.t) list) =
  match databases with
  | [] -> []
  | ((str, values) as h) :: t ->
      if str = name then t else h :: clear_database_helper name t

(** [write_all_databases file databases] writes all the databases inside
    [databases] to the file [file]. *)
let rec write_all_databases
    (file : string)
    (databases : (string * Basic.t) list) =
  let _ = clear_database_file file in
  match databases with
  | [] -> ()
  | (name, values) :: t ->
      let _ = add_database_t file name values in
      write_all_databases file t

let clear_database (file : string) (name : string) =
  let database = Yojson.Basic.Util.to_assoc (dbs_from_file file) in
  let list_after_removal = clear_database_helper name database in
  write_all_databases file list_after_removal

(** [find_database_helper database_name database_list] returns the
    values of the database with name [database_name] in the list of
    database [database_list]. Raises: NotFound "Database not found in
    file" if the [database_name] is not in [database_list]. *)
let rec find_database_helper
    (database_name : string)
    (database_list : (string * Basic.t) list) =
  let exception NotFound of string in
  match database_list with
  | [] -> raise (NotFound "Database not found in file")
  | (name, values) :: t ->
      if name = database_name then values
      else find_database_helper database_name t

let rec find_database (file : string) (database_name : string) =
  let database_list = Yojson.Basic.Util.to_assoc (dbs_from_file file) in
  find_database_helper database_name database_list

(** [find_value_helper value_list value_name] returns the value
    associated with the value [value_name] in the list of values
    [value_list] in a database. Raises: NotFound "Value not found in
    database" if the [value_name] is not in [value_list]. *)
let rec find_value_helper
    (value_list : (string * Basic.t) list)
    (value_name : string) =
  match value_list with
  | [] -> raise (NotFound "Value not found in database")
  | (name, values) :: t ->
      if name = value_name then values
      else find_value_helper t value_name

let find_value_in_database
    (file : string)
    (database_name : string)
    (value_name : string) =
  let database = find_database file database_name in
  let value_list = Yojson.Basic.Util.to_assoc database in
  find_value_helper value_list value_name