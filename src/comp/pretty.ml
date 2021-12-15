(* A pretty printer for CPPL programs.
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

let margin = 68
let indent = 2

let rec print_blanks indent =
  if indent > 0 then begin
    print_string " ";
    print_blanks (indent - 1)
  end

let show_id (_, id) =
  print_string id

let show_name = show_id

let show_path (path, name) =
  List.iter (fun id -> show_id id; print_string ".") path;
  show_name name

let string_of_key_type key_type =
  match key_type with
    Pubkey _ -> "pubkey"
  | Symkey _ -> "symkey"

let show_id_type id_type =
  let string =
    match id_type with
      Name _ -> "name"
    | Text _ -> "text"
    | Msg _ -> "msg"
    | Nonce _ -> "nonce"
    | Chan _ -> "channel"
    | Key key_type ->
	string_of_key_type key_type in
  print_string string

let show_decl (id, id_type) =
  match id_type with
    None ->
      show_id id
  | Some t ->
      show_id id;
      print_string ":";
      show_id_type t

let show_decls decls =
  match decls with
    [] ->   print_string "()"
  | p :: ps ->
      open_box 1;
      print_string "(";
      show_decl p;
      let show_rest_decl decl =
	print_string ",";
	print_space();
	show_decl decl in
      List.iter show_rest_decl ps;
      print_string ")";
      close_box()

let print_tag = print_string

let print_value (_, string) =
  printf "%S" string

let print_term term =
  match term with
    Constant con -> print_value con
  | Identifier decl -> show_decl decl
  | Variable (_, var) -> print_string var

(* infix equality printer *)
let print_literal (pred, terms) =
  match pred, terms with
    (_, "="), [a; b] ->			(* print equality *)
      open_box indent;
      print_term a;
      print_space();
      print_string "= ";
      print_term b;
      close_box()
  | _ ->				(* print predicate *)
      show_id pred;
      match terms with
	[] -> ()
      | term :: terms ->
	  open_box 1;
	  print_char '(';
	  print_term term;
	  let print_rest term =
	    print_char ',';
	    print_space();
	    print_term term in
	  List.iter print_rest terms;
	  print_char ')';
	  close_box()

let print_clause period (goal, body) =
  open_hvbox indent;
  print_literal goal;
  begin match body with
    [] -> ()
  | literal :: literals ->
      print_string " :-";
      print_space();
      print_literal literal;
      let print_rest literal =
	print_char ',';
	print_space();
	print_literal literal in
      List.iter print_rest literals
  end;
  if period then print_string ".";
  close_box()

let show_rely keyword conjuncts =
  print_string keyword;
  match conjuncts with
  | [] ->
      print_string "_"
  | [clause] ->
      print_clause false clause
  | clause :: conjuncts ->
      print_clause false clause;
      let show_and clause =
	print_space();
	print_string "and ";
	print_clause false clause in
      List.iter show_and conjuncts

let show_guar keyword conjuncts =
  print_string keyword;
  match conjuncts with
  | [] ->
      print_string "_"
  | [literal] ->
      print_literal literal
  | literal :: conjuncts ->
      print_literal literal;
      let show_and literal =
	print_space();
	print_string "and ";
	print_literal literal in
      List.iter show_and conjuncts

let show_expression expr =
  match expr with
    Ide (_, id) ->
      print_string id
  | New_key key_type ->
      print_string "new ";
      print_string (string_of_key_type key_type)
  | New_nonce _  ->
      print_string "new nonce"
  | Connect id ->
      print_string "connect(";
      show_id id;
      print_string ")"
  | Accept _ ->
      print_string "accept"
  | Receive id ->
      print_string "receive(";
      show_id id;
      print_string ")"

let rec show_message message =
  match message with
    Decl d ->
      show_decl d
  | Cat (_, mesg1, mesg2) ->
      open_box 0;
      show_message_item mesg1;
      print_string ",";
      print_space();
      show_message_list mesg2;
      close_box ()
  | Tag (_, tag, mesg) ->
      open_box 0;
      print_tag tag;
      print_string ",";
      print_space();
      show_message_list mesg;
      close_box ()
  | Encrypt (_, message, decl, sym) ->
      print_string "{";
      if sym then
	print_string "|";
      show_message message;
      if sym then
	print_string "|";
      print_string "} ";
      show_decl decl
  | Sign (_, message, decl, sym) ->
      print_string "[";
      if sym then
	print_string "|";
      show_message message;
      if sym then
	print_string "|";
      print_string "] ";
      show_decl decl
  | Hash (_, message) ->
      print_string "hash(";
      show_message message;
      print_string ")"
  | Bind (id, message) ->
      open_box 1;
      print_string "<";
      show_id id;
      print_string " =";
      print_space();
      show_message message;
      print_string ">";
      close_box ()

and show_message_item message =
  match message with
    Cat (_, _, _) ->
      print_string "(";
      show_message message;
      print_string ")"
  | Tag (_, _, _) ->
      print_string "(";
      show_message message;
      print_string ")"
  | _ ->
      show_message message

and show_message_list message =
  match message with
    Cat (_, mesg1, mesg2) ->
      show_message_item mesg1;
      print_string ",";
      print_space();
      show_message_list mesg2
  | Tag (_, tag, mesg) ->
      print_tag tag;
      print_string ",";
      print_space();
      show_message_list mesg
  | _ ->
      show_message message

let rec show_let decl expr statement =
  open_box indent;
  print_string "let ";
  show_decl decl;
  print_string " =";
  print_space();
  show_expression expr;
  print_string " in";
  close_box();
  print_space();
  show_statement statement

and show_send keyword (_, _, guar, id, message, statement) =
  open_vbox 0;
  open_hvbox indent;
  show_guar keyword guar;
  if guar = [] then
    print_string " "
  else
    print_space();
  print_string "--> ";
  show_id id;
  print_string " ";
  show_message message;
  close_box();
  print_space();
  show_statement statement;
  close_box()

and show_sends sends =
  open_vbox 0;
  print_string "send cases";
  print_space();
  begin match sends with
    [] -> ()
  | send :: sends ->
      print_blanks indent;
      show_send "" send;
      print_space();
      let loop send =
	print_string "|";
	print_blanks (indent - 1);
	show_send "" send;
	print_space() in
      List.iter loop sends
  end;
  print_string "end"

and show_recv keyword (_, _, message, rely, statement) =
  open_vbox 0;
  open_hvbox indent;
  print_string keyword;
  show_message message;
  print_space();
  show_rely "--> " rely;
  close_box();
  print_space();
  show_statement statement;
  close_box()

and show_recvs src recvs =
  open_vbox 0;
  let id =
    match src with
      Match id ->
	print_string "match ";
	id
    | Recv id ->
	print_string "receive ";
	id in
  show_id id;
  print_string " cases";
  print_space();
  begin match recvs with
    [] -> ()
  | recv :: recvs ->
      print_blanks indent;
      show_recv "" recv;
      print_space();
      let loop recv =
	print_string "|";
	print_blanks (indent - 1);
	show_recv "" recv;
	print_space() in
      List.iter loop recvs
  end;
  print_string "end"

and show_invocation keyword (_, _, guar, call, statement) =
  open_vbox 0;
  open_hvbox indent;
  show_guar keyword guar;
  print_space();
  print_string "--> ";
  begin match call with
    None ->
      print_string "_"
  | Some (path, input, _, output, rely) ->
      open_box indent;
      show_path path;
      print_space();
      show_decls input;
      print_space();
      show_decls output;
      close_box();
      if rely = [] then
	print_string " "
      else
	print_space();
      show_rely "" rely
  end;
  close_box();
  print_space();
  show_statement statement;
  close_box()

and show_invocations invs =
  open_vbox 0;
  print_string "call cases";
  print_space();
  begin match invs with
    [] -> ()
  | inv :: invs ->
      print_blanks indent;
      show_invocation "" inv;
      print_space();
      let loop inv =
	print_string "|";
	print_blanks (indent - 1);
	show_invocation "" inv;
	print_space() in
      List.iter loop invs
  end;
  print_string "end"

and show_statement statement =
  match statement with
    Return _ ->
      print_string "return"
  | Let (_, _, decl, expr, statement) ->
      show_let decl expr statement
  | To (_, [send]) ->
      show_send "send " send
  | To (_, sends) ->
      show_sends sends
  | From (_, Match (_, id), [recv]) ->
      show_recv ("match " ^ id ^ " ") recv
  | From (_, Recv (_, id), [recv]) ->
      show_recv ("receive " ^ id ^ " ") recv
  | From (_, src, recvs) ->
      show_recvs src recvs
  | Call (_, [inv]) ->
      show_invocation "call " inv
  | Call (_, invs) ->
      show_invocations invs

let print_procedure (name, _, input, rely, guar, output, statement) =
  print_newline();
  open_vbox (2 * indent);
  show_name name;
  print_string " ";
  open_box 0;
  show_decls input;
  print_space();
  show_decls output;
  close_box();
  if rely <> [] then begin
    print_space();
    open_hvbox 2;
    show_rely "assumes " rely;
    close_box()
  end;
  if guar <> [] then begin
    print_space();
    open_hvbox 2;
    show_guar "proves  " guar;
    close_box()
  end;
  close_box();
  print_newline();
  open_vbox indent;
  print_blanks indent;
  show_statement statement;
  close_box();
  print_newline();
  print_string "end";
  print_newline()

let open_out_or_stdout file_name =
  if file_name = "-" then
    stdout
  else
    open_out file_name

let print_program file_name program =
  let out = open_out_or_stdout file_name in
  set_formatter_out_channel out;
  set_margin margin;
  List.iter print_procedure program
