open Yojson
open Yojson.Basic.Util

(************************************************************ A SQL
  database management system.

  @author Max Hadden (mnh38)

  ************************************************************)

(* Json list representation of a set of databases *)
type db_list = { databases : Yojson.Basic.t }

(* File location, accessible via this function in the event the location
   changes*)
let file_location = "./data/database.json"

(* Retrieve json representation of database file*)
let dbs_from_file : Yojson.Basic.t =
  Yojson.Basic.from_file file_location

(* Convert json database to list of individual databases*)
let database_list = dbs_from_file |> Yojson.Basic.to_string

(* write [db] to database file*)
let write_to_file (db : Yojson.Basic.t) =
  Yojson.Basic.to_file file_location db

(* recursive helper to convert_vals*)
let rec convert_vals_help (vals : string list) (acc : string) : string =
  match vals with
  | h :: t ->
      if t = [] then
        convert_vals_help t (acc ^ "\"" ^ h ^ "\"" ^ ":" ^ "\"\"")
      else
        convert_vals_help t (acc ^ "\"" ^ h ^ "\"" ^ ":" ^ "\"\"" ^ ",")
  | [] -> acc ^ "}"

(* converts a list of values [vals] into a json string. Example:
   [convert_vals (\["name","id","location"\]) ""] gives "{"name" : "",
   "id" : "","location" : ""}" *)
let convert_vals (vals : string list) : string =
  convert_vals_help vals "{"

(* remove outer parentheses of database management object [dbm] to allow
   for later insertion of another database inside. Returns with trailing
   comma if inside parens is not empty. Requires: [dbm] must have length
   of at least 2, representing the constant 2 outer parentheses of the
   dbms*)
let splice_outer_parens (dbm : string) =
  let spliced = String.sub dbm 1 (String.length dbm - 2) in
  if String.length spliced > 0 then spliced ^ "," else spliced

(* Add a new database labeled [name] with values [values] to the
   database list -- Yojson.Basic.to_file file_location
   (Yojson.Basic.from_string str)*)
let add_database (name : string) (values : string list) =
  let str =
    "{"
    ^ splice_outer_parens database_list
    ^ "\"" ^ name ^ "\"" ^ ":" ^ convert_vals values ^ "}"
  in
  let file_out = open_out file_location in
  Printf.fprintf file_out "%s" str;
  close_out file_out
