(* An abstract syntax tree for CPPL programs.
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

type pos = Pos.pos

type const = pos * string

type id = pos * string

type var = pos * string

type name = id

type path = const list * name

type key_type =
    Pubkey of pos
  | Symkey of pos

type id_type =
    Name of pos
  | Text of pos
  | Msg of pos
  | Nonce of pos
  | Chan of pos
  | Key of key_type

type decl = id * id_type option

(* Used during type checking and scope analysis *)

type def = id * id_type option ref

type term =
    Constant of const
  | Identifier of decl
  | Variable of var

type literal = id * term list

type clause = literal * literal list

(* Empty list is _, else it's a list of conjuncts. *)

type assume = clause list

type prove = literal list

type expression =
    Ide of id
  | New_key of key_type
  | New_nonce of pos
  | Connect of id
  | Accept of pos
  | Receive of id

type message =
    Decl of decl
  | Cat of pos * message * message
  | Tag of pos * string * message
  | Encrypt of pos * message * decl * bool
	(* bool is true for sym key usage *)
  | Sign of pos * message * decl * bool
  | Hash of pos * message
  | Bind of id * message

type call = path * decl list * def list * decl list * assume

(* A def list is used in statements to record identifiers introduced
   by parts of the statement.  For example, the guarantee associated
   with a return may create values to be returned. *)

type source =
    Match of id
  | Recv of id

type statement =
    Return of pos * def list * prove * decl list
  | Let of pos * def * decl * expression * statement
  | To of pos * send list
  | From of pos * source * receive list
  | Call of pos * invocation list
and send = pos * def list * prove * id * message * statement
and receive = pos * def list * message * assume * statement
and invocation = pos * def list * prove * call option * statement

type procedure =
    name * def list * decl list * assume
      * prove * decl list * statement

type program = procedure list
