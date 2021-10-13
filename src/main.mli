type db_list
(** Json list representation of a set of databases. *)

val set_file_location : string -> string
(** [set_file_location file] sets the location of the file [file] for
    the current database. Requires: [file] must be a legal json file in
    the data directory*)

val dbs_from_file : string -> Yojson.Basic.t
(** [dbs_from_file file] retrieves the json representation of database
    file [file]. *)

val database_list : string
(** [database_list file] converts json database in file [file] to a list
    of individual databases. *)

val write_to_file : Yojson.Basic.t -> unit
(** [write_to_file db] writes [db] to database file. *)

val splice_outer_parens : string -> string
(** [splice_outer_parens dbm] removes the outer parentheses of database
    management object [dbm] to allow for later insertion of another
    database inside. Returns with trailing comma if inside parens is not
    empty. Requires: [dbm] must have length of at least 2, representing
    the constant 2 outer parentheses of the database management object. *)

val add_database : string -> string -> string list -> unit
(** [add_database file name values] adds a new database labeled [name]
    with values [values] to the database list in file [file] --
    Yojson.Basic.to_file file_location (Yojson.Basic.from_string str). *)
