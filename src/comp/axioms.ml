(* Prints Datalog axioms as OCaml source.
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

let print_str str =
  printf "%S" str

let print_id (_, str) =
  print_str str

let pos_of_id (pos, _) = pos

let print_list f l =
  print_string "[";
  begin match l with
    [] ->
      ()
  | e :: l ->
      open_hvbox 0;
      f e;
      let semi_sep e =
	print_string ";";
	print_space();
	f e in
      List.iter semi_sep l;
      close_box()
  end;
  print_string "]"

let print_term term =
  match term with
    Constant const ->
      print_string "mkval (mk_text ";
      print_id const;
      print_string ")"
  | Identifier (id, None) ->
      print_string "mkval (mk_text ";
      print_id id;
      print_string ")"
  | Identifier (id, Some _) ->
      let msg = "Types not allowed here" in
      failwith (Pos.pos_message (pos_of_id id) msg)
  | Variable var ->
      print_string "mkvar ";
      print_id var

let print_literal (pred, terms) =
  print_string "(mkliteral ";
  open_hvbox 0;
  print_id pred;
  print_space();
  print_list print_term terms;
  print_string ")";
  close_box()

let print_pos ((pos, _), _) =
  print_string (Pos.pos_line pos);
  print_newline()

let print_axiom (goal, body) =
  print_pos goal;
  print_string "  mkclause ";
  open_hvbox 0;
  print_literal goal;
  print_space();
  print_list print_literal body;
  print_string ";";
  close_box();
  print_newline()

let open_out_or_stdout file_name =
  if file_name = "-" then
    stdout
  else
    open_out file_name

let print_axioms file_name axioms =
  let out = open_out_or_stdout file_name in
  set_formatter_out_channel out;
  print_string "open Cppl";
  print_newline();
  print_string "let (axioms: clause list) = [";
  print_newline();
  List.iter print_axiom axioms;
  print_string "]";
  print_newline()
