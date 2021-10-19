type db_list
(** Json list representation of a set of databases. *)

exception NotFound of string
(** Raised when a database or value is searched for and not found in a
    file or database. *)

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

val add_database_t : string -> string -> Yojson.Basic.t -> unit
(** [add_database_t file name values] adds a new database labeled [name]
    with values [values] to the datbase list in file file [file]. Does
    the same thing as [add_databse file name values] except with an
    input of type Yojson.Basic.t instead of string list for [values]. *)

val clear_database_file : string -> unit
(** [clear_database_file file] clears the whole database file [file], so
    that there are no databases in the file. *)

val delete_database : string -> string -> unit
(** [delete_database file name] removes the database with name [name]
    from the database file [file]. Removes the first instance of a
    database with name [name] in [file] if there are multiple databases
    with the same name. If [name] is not in database file [file], then
    the same file is returned unchanged. *)

val find_database : string -> string -> Yojson.Basic.t
(** [find_database file database_name] returns the values of the
    database [database_name] in database file [file]. Raises: NotFound
    "Database not found in file" if the [database_name] is not in
    [database_list]. *)

val find_value_in_database :
  string -> string -> string -> Yojson.Basic.t
(** [find_value_in_database file database_name value_name] returns the
    value associated with the value [value_name] in the database
    [database_name]. Raises: NotFound "Value not found in database" if
    the [value_name] is not in [database_name]. *)
