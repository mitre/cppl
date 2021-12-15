(* Prints a CPPL program as a Scheme S-Expression.
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

open Format
open Ast

(* Simple S-Expressions *)

type scheme_data =
    S of string
  | L of scheme_data list

let indent = 1

(* Pretty print an S-Expression *)

let rec print_data data =
  match data with
    S s -> printf "%S" s
  | L [] -> print_string "()"
  | L (l :: ls) ->
      open_box indent;
      print_string "(";
      print_data l;
      List.iter (fun d -> print_space(); print_data d) ls;
      print_string ")";
      close_box()

(* Convert a CPPL abstract syntax tree into an S-Expression *)

let show_value (_, string) =
  L [ S "value"; S string ]

let show_var (_, string) =
  L [ S "variable"; S string ]

let show_id (_, id) =
  S id

let show_name id =
  show_id id

let show_path (path, name) =
  if path = [] then
    show_name name
  else
    L (List.map show_id path @ [ show_name name ])

let string_of_key_type key_type =
  match key_type with
    Pubkey _ -> "pubkey"
  | Symkey _ -> "symkey"

let string_of_id_type id_type =
  match id_type with
    Name _ -> "name"
  | Text _ -> "text"
  | Msg _ -> "msg"
  | Nonce _ -> "nonce"
  | Chan _ -> "channel"
  | Key key_type ->
      string_of_key_type key_type

let string_of_opt_id_type id_type =
  match id_type with
    None ->
      "none"
  | Some t ->
      string_of_id_type t

let show_decl (id, id_type) =
  L [ S "decl"; show_id id; S (string_of_opt_id_type id_type) ]

let show_decls decls =
  L (List.map show_decl decls)

(* Show formulas *)

let show_term term =
  match term with
    Constant con -> show_value con
  | Identifier decl -> show_decl decl
  | Variable var -> show_var var

let show_literal ((_, pred), terms) =
  L (S pred :: List.map show_term terms)

let show_clause (goal, body) =
  L (S ":=" :: show_literal goal :: List.map show_literal body)

let show_rely conjuncts =
  L (List.map show_clause conjuncts)

let show_guar conjuncts =
  L (List.map show_literal conjuncts)

(* Show expressions *)

let show_expression expr =
  match expr with
    Ide id ->
      L [ S "ide"; show_id id ]
  | New_key (key_type) ->
      L [ S ("new-" ^ (string_of_key_type key_type)) ]
  | New_nonce _ ->
      L [ S "new-nonce" ]
  | Connect id ->
      L [ S "connect"; show_id id ]
  | Accept _ ->
      L [ S "accept" ]
  | Receive id ->
      L [ S "receive"; show_id id ]

(* Show messages *)

let rec show_message message =
  match message with
    Decl d ->
      L [ S "ref"; show_decl d ]
  | Cat (_, mess1, mess2) ->
      L [ S "cat"; show_message mess1; show_message mess2 ]
  | Tag (_, tag, mess) ->
      L [ S "tag"; S tag; show_message mess ]
  | Encrypt (_, message, decl, sym) ->
      let funct =
	if sym then
	  "symencrypt"
	else
	  "pubencrypt" in
      L [ S funct; show_message message; show_decl decl ]
  | Sign (_, message, decl, sym) ->
      let funct =
	if sym then
	  "symsign"
	else
	  "pubsign" in
      L [ S funct; show_message message; show_decl decl ]
  | Hash (_, message) ->
      L [ S "hash"; show_message message ]
  | Bind (id, message) ->
      L [ S "bind"; show_id id; show_message message ]

(* Show statements *)

let rec show_send o g (_, _, guar, id, message, statement) =
  L [ S "send";
      show_guar guar;
      show_id id;
      show_message message;
      show_statement o g statement ]

and show_recv o g (_, _, message, rely, statement) =
  L [ S "recv";
      show_message message;
      show_rely rely;
      show_statement o g statement ]

and show_invocation o g (_, _, guar, call, statement) =
  match call with
    None ->
      L [ S "null_call";
	  show_guar guar;
	  show_statement o g statement ]
  | Some (path, input, _, output, rely) ->
      L [ S "call";
	  show_guar guar;
	  show_path path;
	  show_decls input;
	  show_decls output;
	  show_rely rely;
	  show_statement o g statement ]

and show_statement output guar statement =
  match statement with
    Return _ ->
      L [ S "return"; show_decls output; show_guar guar ]
  | Let (_, _, decl, expr, statement) ->
      L [ S "let";
	  L [ show_decl decl; show_expression expr ];
	  show_statement output guar statement ]
  | To (_, sends) ->
      L (S "sends" ::
	 List.map (show_send output guar) sends)
  | From (_, Recv id, recvs) ->
      L (S "recvs" ::
	 show_id id ::
	 List.map (show_recv output guar) recvs)
  | From (_, Match id, recvs) ->
      L (S "matches" ::
	 show_id id ::
	 List.map (show_recv output guar) recvs)
  | Call (_, invs) ->
      L (S "calls" ::
	 List.map (show_invocation output guar) invs)

(* Show a procedure *)

let show_procedure (name, _, input, rely, guar, output, statement) =
  L [ S "proc";
      L [ show_name name;
	  show_decls input;
	  show_rely rely;
	  show_guar guar;
	  show_decls output ];
      show_statement output guar statement ]

let print_procedure proc =
  print_newline();
  print_data (show_procedure proc);
  print_newline()

let print_axiom clause =
  print_newline();
  print_data (show_clause clause);
  print_newline()

let open_out_or_stdout file_name =
  if file_name = "-" then
    stdout
  else
    open_out file_name

let print_program file_name program =
  let out = open_out_or_stdout file_name in
  set_formatter_out_channel out;
  print_string "; -*- mode: scheme -*-";
  print_newline();
  List.iter print_procedure program

let print_axioms file_name axioms =
  let out = open_out_or_stdout file_name in
  set_formatter_out_channel out;
  print_string "; -*- mode: scheme -*-";
  print_newline();
  List.iter print_axiom axioms
