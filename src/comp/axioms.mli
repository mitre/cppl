(** Axioms to OCaml Code Generator *)

open Ast

val print_axioms : string -> clause list -> unit
(** Prints axioms.  The first argument is the name of the
   file.  Standard output is used if the file name is "-". *)
