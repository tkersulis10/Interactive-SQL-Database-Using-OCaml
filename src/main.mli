type db_list
(** Json list representation of a set of databases. *)

val set_file_location : string -> string
(** [set_file_location file] sets the location of the file [file] for
    the current database. Requires: [file] must be a legal json file in
    the data directory*)

val database_list : string
(** Convert json database to list of individual databases*)

val dbs_from_file : Yojson.Basic.t
(** Retrieve json representation of database file. *)

val write_to_file : Yojson.Basic.t -> unit
(** [write_to_file db] writes [db] to database file. *)

val splice_outer_parens : string -> string
(** [splice_outer_parens dbm] removes the outer parentheses of database
    management object [dbm] to allow for later insertion of another
    database inside. Returns with trailing comma if inside parens is not
    empty. Requires: [dbm] must have length of at least 2, representing
    the constant 2 outer parentheses of the database management object. *)

val add_database : string -> string list -> unit
(** [add_database name values] adds a new database labeled [name] with
    values [values] to the database list -- Yojson.Basic.to_file
    file_location (Yojson.Basic.from_string str). *)
