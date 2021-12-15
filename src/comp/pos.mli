(** Token Positions *)

type pos

val mkpos : Lexing.lexbuf -> pos
(** Create a position from the current lexbuf state. *)

val pos_message : pos -> string -> string
(** Add a position to a message. *)

val pos_msg : pos -> string -> string
(** Add a position to a message, short form. *)

val pos_line : pos -> string
(** Create a line directive. *)
