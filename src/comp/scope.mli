(** Identifier Scope *)

val scope : Ast.program -> Ast.program
(** Determines the scope of identifiers and returns a version of the
   program with def lists added to statements.  The def lists record
   the identifiers introduced by parts of a statement.  The returned
   version of the program also has return statements that contain a
   copy of the procedure's output identifiers and its guarantee. *)
