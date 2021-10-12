open Yojson
open Yojson.Basic.Util

(************************************************************
  A SQL database management system.
  
   @author Max Hadden (mnh38)

  ************************************************************)

  (** Json list representation of a set of databases *)
  type db_list = {
    databases : Yojson.Basic.t list;
  }

  (** Retrieve json representation of database file*)
  let dbs_from_file : Yojson.Basic.t =
    Yojson.Basic.from_file "./data/database.json"

  (** Convert json database to list of individual databases*)
  let database_list =
    {
      databases = dbs_from_file |> member "databases" |> to_list
    }
  
  let write_to_file (db : db_list) = 
    0

  (** Add a new database to the database list*)
  let add_database (name : string) (values : string list) = 
    0