(** Program Pretty Printer *)

open Ast

val print_program : string -> program -> unit
(** Pretty prints a CPPL program.  The first argument is the name of
   the file.  Standard output is used if the file name is "-". *)
