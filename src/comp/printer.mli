(** Program to OCaml Code Generator *)

open Ast

val print_program : bool -> string -> program -> unit
(** [print_program debug file program] prints a CPPL program as an
   OCaml source file.  When [debug] is true, the generated code
   produces trace output useful when debugging a CPPL program.  The
   [file] parameter is the name of the file used for output.  Standard
   output is used if the file name is "-". *)

val print_interface : string -> program -> unit
(** [print_interface file program] prints the signature of a CPPL
   program as an OCaml interface (mli) file.  The [file] parameter is
   the name of the file used for output.  Standard output is used if
   the file name is "-". *)
