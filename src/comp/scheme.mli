(** Scheme S-Expression Code Generator *)

(** This module generates a representation of a CPPL program in Scheme
   S-Expression syntax. *)

open Ast

val print_program : string -> program -> unit

(** Prints a CPPL program in Scheme S-Expression syntax.  The first
   argument is the name of the file.  Standard output is used if the
   file name is "-". *)

val print_axioms : string -> clause list -> unit
(** Prints axioms in Scheme S-Expression syntax.  The first argument
   is the name of the file.  Standard output is used if the file name
   is "-". *)
