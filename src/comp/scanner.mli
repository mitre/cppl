(** Scanner *)

val init : Lexing.lexbuf -> string -> unit
(** Set the scanner's file name. *)

val token : Lexing.lexbuf -> Parser.token
(** Scan for the next token. *)
