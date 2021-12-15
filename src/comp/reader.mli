(** Program and Axioms Reader *)

open Ast

val read_program : string -> program
(** Reads a CPPL program.  The argument is the name of the file.
   Standard input is used if the file name is "-". *)

val read_axioms : string -> clause list
(** Reads axioms.  The argument is the name of the file.
   Standard input is used if the file name is "-". *)
