type command
(** The representation of a command. *)

exception Empty
(** Raised when a command is empty. *)

exception NoArgs
(** Raised when a command has no arguments. *)

exception Malformed
(** Raised when a command is malformed. *)

val parse_args : string list -> string list -> string list
(** [parse_args args acc] converts a list [args] into a list of
    arguments without any empty strings. *)

val get_create_args : string list -> string * string list
(** [get_create_args args] extracts a tuple of name and list of field
    name args given a create table command from args list [args].
    Raises: NoArgs if no args are available. *)

val cmd_do : string -> string list -> command
(** [cmd_do cmd args] handles the arguments [args] of the command [cmd]. *)

val cmd_read : string -> string list -> command
(** [cmd_read cmd lst] locates the fully parsed command (in the event
    the command is 2+ words like CREATE TABLE) and a list of arguments.
    Passes result into [cmd_do] for argument handling. Requires: cmd is
    a lowercase ascii string. *)

val get_cmd_sep : string list -> command
(** [get_cmd_sep lst] gets the first word of a command in list [lst],
    indicating command type, followed by the rest of the list,
    indicating arguments, and calls [cmd_read] on the resulting command
    and args. *)

val parse : string -> command
(** [parse input] parses an input string to a command of type command. *)
