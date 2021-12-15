(* Canonical S-expression syntax for structuring binary data.
   Copyright (C) 2005 The MITRE Corporation

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

(** Canonical S-Expressions *)

(** This module provides canonical S-expression syntax for structuring
binary data.  The syntax is canonical in that there is exactly one way
to encode an S-expression as a sequence of bytes.

The S-expression syntax adds list structure to atomic binary data.  An
atom is a sequence of bytes.  When encoded, the atom is preceded by
its length as an ASCII encoded integer and a colon character.  To make
the syntax canonical, no leading zeros are allowed in the length
integer, except that the single ASCII character for zero is used for
the empty string.  Three example atoms follow.

{v 2:ab
4:I am
0: v}

Parentheses are used to delimit lists of S-expressions.  The first
element in every list must be an atom.

{v S-expr ::= Atom | List
List   ::= ( S-expr* ) v}

A sample S-expression follows.

{v (1:a(2:ab0:)3:xyz) v}

The S-expression is a list of three S-expressions, the atom "[a]", a
list of two items, and the atom "[xyz]".  The embedded list contains the
atom "[ab]" and the empty atom "".

Canonical S-expression syntax is similar to, and motivated by, the
syntax of the same name described by Ronald L. Rivest
[<http://theory.lcs.mit.edu/~rivest/sexp.html>].  This syntax omits
Rivest's display hints.

*)

(** The type of an S-Expression in which atoms are strings. *)
type sexpr =
    String of string
  | List of sexpr list

exception Sexpr_input_error of string

val input_sexpr : in_channel -> sexpr
(** Read an S-Expression from a channel.  Raise an
   {!Sexpr.Sexpr_input_error} if the input fails to contain data in
   the appropriate format. *)

val sexpr_of_string : string -> sexpr
(** Read an S-Expression from a string.  Raise an
   {!Sexpr.Sexpr_input_error} if the input fails to contain data in
   the appropriate format. *)

val output_sexpr : out_channel -> sexpr -> unit
(** Write an S-Expression to a channel.  *)

val string_of_sexpr : sexpr -> string
(** Write an S-Expression to a string.  *)
