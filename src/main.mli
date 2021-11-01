type db_list
(** Json list representation of a set of databases. *)

exception ValNotFound of string
(** Raised when a value is searched for and not found in a file or
    database. *)

exception DatabaseNotFound of string
(** Raised when a database is searched for and not found in a file or
    database. *)

exception InvalidRow of string
(** Raised when an invalid row is searched for in a database. *)

val file_location : string
(** Default file path of main DBMS*)

val set_file_location : string -> string
(** [set_file_location file] sets the location of the file [file] for
    the current database. Requires: [file] must be a legal json file in
    the data directory*)

val dbs_from_file : string -> Yojson.Basic.t
(** [dbs_from_file file] retrieves the json representation of database
    file [file]. *)

val database_string : string -> string
(** [database_string file] converts json database in file [file] to a
    json string of individual databases. *)

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
    with the same name. Raises: DatabaseNotFound "database_name" if the
    [database_name] is not in [file]. *)

val clear_database : string -> string -> unit
(** [clear_database file name] clears te database with name [name] from
    the database file [file]. The database remains in the file, but its
    values are removed. Clears the first instance of a database with
    name [name] in [file] if there are multiple databases with the same
    name. If [name] is not in the database file [file], then the same
    file is returned unchanged. *)

val find_database : string -> string -> Yojson.Basic.t
(** [find_database file database_name] returns the values of the
    database [database_name] in database file [file]. Raises:
    DatabaseNotFound "database_name" if the [database_name] is not in
    [file]. *)

val find_value_in_database : string -> string -> string -> string
(** [find_value_in_database file database_name value_name] returns the
    value associated with the value [value_name] in the database
    [database_name]. Requires: The value is a string. Raises:
    ValNotFound "value_name" if the [value_name] is not in
    [database_name]. *)

val get_db_names_list : string -> string
(** [get_db_names_list file] returns a string of each database name in
    the system in [file], each separated by a newline for printing into
    console. *)

val list_rows : string -> string -> string
(** [list_rows file database_name] returns string of field-value pairs
    for each row a database in [file] given by [database_name], with
    each row separated by a newline. *)

val add_element_to_database : string -> string -> string -> unit
(** [add_element_to_database file database_name value_name] adds the
    element [value_name] to the template of database [database_name] in
    [file]. *)

val add_element_to_all_database :
  string -> ?val_name:string -> string -> string -> unit
(** [add_element_to_all_database file database_name value_name] adds the
    element [value_name] to all rows of the database [database_name] in
    [file]. *)

val update_element : string -> string -> string -> int -> string -> unit
(** [update_element file database_name value_name element_row new_value]
    updates the element in [file] in [database_name] with element name
    [value_name] in the [element_row]th row of the database to new value
    [new_value]. The first row after the template row in the database
    corresponds to [element_row] = 1. If [value_name] is not
    [database_name] then nothing changes. Requires: [element_row] >= 1
    and is a valid row in the [database_name]. *)

val update_all : string -> string -> string -> string -> unit
(** [update_element file database_name value_name new_value] updates all
    the elements in [file] in [database_name] with element name
    [value_name] to new value [new_value]. If [value_name] is not
    [database_name] then nothing changes. *)
