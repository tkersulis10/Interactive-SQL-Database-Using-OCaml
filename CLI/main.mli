(** Representation of a command line.

    This module contains all of the functions to work the command line. *)

type command
(** The representation of a command. *)

exception Empty
(** Raised when a command is empty. *)

exception NoArgs
(** Raised when a command has no arguments. *)

exception Malformed
(** Raised when a command is badly formatted. *)

exception Invalid
(** Raised when a command is unknown. *)

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
