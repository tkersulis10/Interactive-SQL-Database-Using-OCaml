open Yojson
open Yojson.Basic.Util

type db_list = { databases : Yojson.Basic.t }

let set_file_location (file : string) : string = "./data/" ^ file

(** File location, accessible via this function in the event the
    location changes*)
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
    ^ "\"" ^ name ^ "\"" ^ ":" ^ convert_vals values ^ "}"
  in
  let file_out = open_out (set_file_location file) in
  Printf.fprintf file_out "%s" str;
  close_out file_out
