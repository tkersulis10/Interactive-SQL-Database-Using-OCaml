type db_list

val database_list : string

val dbs_from_file : Yojson.Basic.t

val write_to_file : Yojson.Basic.t -> unit

val add_database : string -> string list -> unit

val splice_outer_parens : string -> string