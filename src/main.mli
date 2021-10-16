type db_list
(** Json list representation of a set of databases. *)

exception NotFound of string
(** Raised when a database that is not in the database file is searched
    for. *)

val set_file_location : string -> string
(** [set_file_location file] sets the location of the file [file] for
    the current database. Requires: [file] must be a legal json file in
    the data directory*)

val dbs_from_file : string -> Yojson.Basic.t
(** [dbs_from_file file] retrieves the json representation of database
    file [file]. *)

val database_list : string -> string
(** [database_list file] converts json database in file [file] to a list
    of individual databases. *)

val write_to_file : string -> Yojson.Basic.t -> unit
(** [write_to_file file db] writes [db] to database file [file]. *)

val splice_outer_parens : string -> string
(** [splice_outer_parens dbm] removes the outer parentheses of database
    management object [dbm] to allow for later insertion of another
    database inside. Returns with trailing comma if inside parens is not
    empty. Requires: [dbm] must have length of at least 2, representing
    the constant 2 outer parentheses of the database management object. *)

val add_database : string -> string -> string list -> unit
(** [add_database file name values] adds a new database labeled [name]
    with values [values] to the database list in file [file] --
    Yojson.Basic.to_file file (Yojson.Basic.from_string str). *)

val clear_database : string -> string -> unit
(** [delete_database file name] removes the database with name [name]
    from the database file [file]. Removes the first instance of a
    database with name [name] in [file] if there are multiple databases
    with the same name. If [name] is not in database file [file], then
    the same file is returned unchanged. *)

val clear_database_file : string -> unit
(** [clear_database_file file] clears the whole database file [file], so
    that there are no databases in the file. *)

val find_database : string -> string -> Yojson.Basic.t
(** [find_database file database_name] returns the values of the
    database [database_name] in database file [file]. Raises: NotFound
    "Database not found in file" if the [database_name] is not in
    [database_list]. *)
