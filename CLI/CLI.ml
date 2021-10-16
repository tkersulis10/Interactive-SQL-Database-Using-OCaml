open Database


type command = 
|Create of (string * string list)
|Delete of string

exception Empty
exception Malformed



let rec split_by_cmd_args (acc:(string * string list)) (input:string)  = 
  match explode input with
  | ' ' -> acc
  |


(* returns the first word of a command in list [lst], indicating command type, followed by the rest of the list, indicating arguments, returned as a tuple (cmd, [args])*)
let rec get_cmd_sep  (lst:string list) =
  match lst with
  | h :: t ->
      if String.length h > 0 then h
      else get_cmd_sep t
  | [] -> raise Empty

let cmd_read cmd lst =
  let args = match lst with
  |h::t -> 1
  | [] -> 0
  in
  match cmd with
  | "create" -> Create of (fst args, snd args)

let rec cmd_get lst = 
  match lst with
  | h :: t -> 
  | [] -> raise Empty

let parse (input:string) : command =
input |> String.split_on_char ' ' |> get_cmd_sep |>


let main () =
  ANSITerminal.print_string [ ANSITerminal.green ]
    "\n\nWelcome to the Database Management System. ";
  print_endline
    "Enter command, or type Help for a list of commands.";
    print_string ">";
    match (parse read_line()) with
    |exception _ -> print_endline "Invalid command"
    | x -> 