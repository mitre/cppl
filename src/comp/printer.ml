(* Prints a CPPL program as OCaml source.
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

open Ast
open Message

let out = ref stdout			(* destination of code*)

let debug = ref false			(* enables runtime traces *)

(* Generated output is more readable without the line directives *)

let omit_line_directives = false (* true *)

let package = "Cppl"	      (* Name of the runtime library module *)

module S = Set.Make(struct
  type t = string			(* A set of strings *)
  let compare = compare
end)

let name_of_id (_, name) = name

let pos_of_id (pos, _) = pos

let name_of_var (_, name) = name

let name_of_decl (id, _) =
  name_of_id id

let pos_of_decl (id, _) =
  pos_of_id id

(* Output functions *)

let out_string str =
  output_string !out str

let oprintf format =
  Printf.fprintf !out format

let put_nl () =
  output_char !out '\n'

let put_pos pos =
  if not omit_line_directives then
    oprintf "%s\n" (Pos.pos_line pos)

let put_list f l =			(* Print an OCaml list. *)
  match l with				(* Elements are placed *)
    [] ->				(* on separate lines. *)
      out_string "[]"
  | e :: l ->
      out_string "[";
      f e;
      let semi_sep e =
	out_string ";\n";
	f e in
      List.iter semi_sep l;
      out_string "]"

(* Output various kinds of identifiers *)

let put_pred id =
  oprintf "%S" (name_of_id id)

let put_var id =
  oprintf "%S" (name_of_id id)

let put_name id =
  out_string (name_of_id id)

let put_path (path, name) =
  List.iter (fun name -> put_name name; out_string ".") path;
  put_name name

let put_id id =
  oprintf "%s'" (name_of_id id)

(* Types *)

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

let string_of_type typ =
  match typ with
    Some id_type ->
      string_of_id_type id_type
  | None ->
      failwith "internal error: missing type"

let type_string_of_decl (_, typ) =
  string_of_type typ

let is_value (_, typ) =
  match typ with
    None -> false
  | Some (Msg _) -> false
  | Some (Chan _) -> false
  | _ -> true

let ensure_value decl =
  if not (is_value decl) then
    let pos = pos_of_decl decl in
    let name = name_of_decl decl in
    let typ = type_string_of_decl decl in
    let msg = "expecting a value, but " ^ name ^ " is a " ^ typ in
    failwith (Pos.pos_message pos msg)

let can_send (_, typ) =
  match typ with
    None -> false
  | Some (Chan _) -> false
  | _ -> true

let ensure_msg_decl decl =
  if not (can_send decl) then
    let pos = pos_of_decl decl in
    let name = name_of_decl decl in
    let typ = type_string_of_decl decl in
    let msg = "expecting an atom, but " ^ name ^ " is a " ^ typ in
    failwith (Pos.pos_message pos msg)

let put_decl (id, typ) =
  oprintf "(%s':%s.%s)" (name_of_id id) package (string_of_type typ)

let put_decls decls =
  match decls with
    [] ->
      out_string "()"
  | d :: ds ->
      out_string "(";
      put_decl d;
      let show_rest_decl d =
	out_string ", ";
	put_decl d in
      List.iter show_rest_decl ds;
      out_string ")"

let put_type (id, typ) =
  oprintf "%s.%s" package (string_of_type typ)

let put_types decls =
  match decls with
      [] ->
	out_string "unit"
    | d :: ds ->
	put_type d;
	let show_rest_types d =
	  out_string " * ";
	  put_type d in
	List.iter show_rest_types ds

let put_tag tag =
  match tag with
    None ->
      print_string "None"
  | Some tag ->
      oprintf "(Some %S)" tag

let pos_msg pos msg =
  if !debug then
    Pos.pos_message pos msg
  else
    msg

let put_raise pos msg =
  oprintf "raise (%s.Cppl_abort %S)\n" package (pos_msg pos msg)

(* For formulas, the variable _t contains the current theory. *)

(* Relies *)

let put_decl_as_val decl =
  let typ = type_string_of_decl decl in
  oprintf "%s.mk_%s " package typ;
  put_decl decl

let put_term term =
  match term with
    Constant (pos, _) ->
    let msg = "Constant not allowed here" in
    failwith (Pos.pos_message pos msg)
  | Identifier decl ->
      ensure_value decl;
      oprintf "%s.mkval (" package;
      put_decl_as_val decl;
      out_string ")"
  | Variable var ->
      oprintf "%s.mkvar " package;
      put_var var

let put_literal (pred, terms) =
  oprintf "(%s.mkliteral " package;
  put_pred pred;
  put_nl();
  put_list put_term terms;
  out_string ")"

let put_assume () =
  oprintf "%s.trace_assume " package;
  if !debug then
    out_string "true"
  else
    out_string "false"

let put_clause (goal, body) =
  put_pos (pos_of_decl goal);
  put_assume();
  oprintf " _t\n(%s.mkclause " package;
  put_nl();
  put_literal goal;
  put_nl();
  put_list put_literal body;
  out_string ");\n"

let put_rely rely =
  List.iter put_clause rely

(* Guarantees *)

let put_prove_term defs term =
  match term with
    Constant (pos, _) ->
    let msg = "Constant not allowed here" in
    failwith (Pos.pos_message pos msg)
  | Identifier decl ->
      ensure_value decl;
      let name = name_of_decl decl in
      if S.mem name defs then		(* Not yet bound *)
	oprintf "%s.mkvar %S" package name
      else begin			(* Is bound *)
	oprintf "%s.mkval (" package;
	put_decl_as_val decl;
	out_string ")"
      end
  | Variable var ->
      oprintf "%s.mkvar " package;
      put_var var

(* A proof result is either failure or a list of values corresponding
   to the variables in the query.  This routine returns a list of the
   variablies in the query that give the location of the value
   associated with the variable in the result.  *)

let prove_vars defs terms =
  let prove_var defs vars term =
    match term with
      Constant const ->
	vars
    | Identifier decl ->
	ensure_value decl;
	let name = name_of_decl decl in
	if S.mem name defs then		(* Not yet bound *)
	  if List.mem name vars then	(* Already handled *)
	    vars
	  else
	    name :: vars
	else
	  vars
    | Variable var ->
	let name = name_of_var var in
	if List.mem name vars then	(* Already handled *)
	  vars
	else
	  name :: vars in
  List.rev (List.fold_left (prove_var defs) [] terms)

(* Given a list l, this routine returns the index into the list of the
   element e.  If e is not in the list, an exception is raised. *)

let loc e l =
  let rec loop n l =
    match l with
      [] ->
	failwith "internal error"
    | e' :: l ->
	if e = e' then
	  n
	else
	  loop (n + 1) l in
  loop 0 l

(* Prints a let statement if the term demands one. *)

let put_prove_let vars defs term =
  match term with
  | Identifier decl ->
      let name = name_of_decl decl in
      if S.mem name defs then		(* Bound by proof *)
	let typ = type_string_of_decl decl in
	out_string "let ";
	put_decl decl;
	let n = loc name vars in
	oprintf " = %s.to_%s (List.nth _a %d) in\n" package typ n;
	S.remove name defs
      else
	defs
  | _ ->
      defs

let put_prove () =
  oprintf "%s.prove_once " package;
  if !debug then
    out_string "true"
  else
    out_string "false"

(* Returns the set of unbound variables *)

let put_query defs (pred, terms) =
  let pos = pos_of_id pred in
  put_pos pos;
  out_string "let _q = ";
  put_prove();
  oprintf " _t\n(%s.mkliteral " package;
  put_pred pred;
  put_nl();
  put_list (put_prove_term defs) terms;
  oprintf ") in\nmatch _q with\nNone ->\n";
  put_raise pos "proof failed";
  oprintf "| Some _a ->\n";
  let vars = prove_vars defs terms in
  List.fold_left (put_prove_let vars) defs terms

let add_def s def =
  S.add (name_of_decl def) s

let set_of_defs defs =
  List.fold_left add_def S.empty defs

let put_guar defs guar =
  let set = set_of_defs defs in
  let _ = List.fold_left put_query set guar in
  ()

(* Outgoing message patterns *)

let rec put_send_pattern defs msg =
  match msg with
    Decl decl ->
      ensure_msg_decl decl;
      let name = name_of_decl decl in
      if S.mem name defs then
	let pos = pos_of_decl decl in
	let msg ="unbound identifier " ^ name in
	failwith (Pos.pos_message pos msg)
      else
	let typ = type_string_of_decl decl in
	oprintf "(%s.message_of_%s " package typ;
	put_decl decl;
	out_string ")"
  | Cat (pos, msg, msg') ->
      oprintf "(%s.concatenate\n"package;
      put_send_pattern defs msg;
      put_nl();
      put_send_pattern defs msg';
      out_string ")"
  | Tag (pos, tag, msg) ->
      oprintf "(%s.addtag %S\n" package tag;
      put_send_pattern defs msg;
      out_string ")"
  | Encrypt (pos, msg, decl, _) ->
      oprintf "(%s.%s_encrypt\n" package (type_string_of_decl decl);
      put_send_pattern defs msg;
      put_nl();
      put_decl decl;
      out_string ")"
  | Sign (pos, msg, decl, _) ->
      oprintf "(%s.%s_sign\n" package (type_string_of_decl decl);
      put_send_pattern defs msg;
      put_nl();
      put_decl decl;
      out_string ")"
  | Hash (pos, msg) ->
      oprintf "(%s.hash\n" package;
      put_send_pattern defs msg;
      out_string ")"
  | Bind (id, _) ->
      let msg = "submessage naming not allowed here" in
      failwith (Pos.pos_message (pos_of_id id) msg)

(* Incoming message patterns *)

let mktemp =
  let counter = ref 0 in
  fun () ->
    let i = !counter in
    counter := i + 1;
    Printf.sprintf "_%d" i

(* The data structure used to hold a todo list. *)

type todo = (S.elt * pos * action) list
and action = Action of (S.t -> todo -> S.t * todo)

(* Process everything on the todo list that meets its prerequisite,
   which means the name that makes up the first element in the todo
   list is bound.  In that case, invoke the action to perform the
   remembered activity.  The pos is used when reporting a failure
   to handle items on the todo list after code for the receive message
   pattern has been generated. *)

let rec put_deferred_subpattern defs todo =
  let rec loop acc todo =
    match todo with
      [] -> defs, acc
    | item :: todo ->
	let name, _, Action action = item in
	if S.mem name defs then		(* Continue to defer *)
	  loop (item :: acc) todo
	else				(* Prerequisite present *)
	  let defs, todo = action defs (List.rev_append acc todo) in
	  put_deferred_subpattern defs todo in
  loop [] todo

(* Compiles an incomming message pattern.  Patterns that require that
   a name is bound are held in a todo list.  When a new binding
   occurs, the todo list is checked to see if remembered activities
   can be performed. *)

let rec put_recv_subpattern src defs todo msg =
  match msg with
    Decl decl ->
      ensure_msg_decl decl;
      let name = name_of_decl decl in
      let typ = type_string_of_decl decl in
      if S.mem name defs then begin	(* Bind variable *)
	out_string "let ";
	put_decl decl;
	oprintf " = %s.%s_of_message %s in\n" package typ src;
	put_deferred_subpattern (S.remove name defs) todo
      end
      else begin			(* Check value of variable *)
	oprintf "%s.%s_same " package typ;
	put_decl decl;
	oprintf " (%s.%s_of_message %s);\n" package typ src;
	defs, todo
      end
  | Cat (pos, msg, msg') ->
      let l = mktemp() in
      let r = mktemp() in
      oprintf "let %s, %s = %s.separate %s in\n" l r package src;
      let defs, todo = put_recv_subpattern l defs todo msg in
      put_recv_subpattern r defs todo msg'
  | Tag (pos, tag, msg) ->
      let t = mktemp() in
      oprintf "let %s = %s.untag %S %s in\n" t package tag src;
      put_recv_subpattern t defs todo msg
  | Encrypt (pos, msg, decl, _) ->
      let put_decrypt defs todo =
	let typ = type_string_of_decl decl in
	let m = mktemp() in
	oprintf "let %s = %s.%s_decrypt %s " m package typ src;
	put_decl decl;
	out_string " in\n";
	put_recv_subpattern m defs todo msg in
      let name = name_of_decl decl in
      if S.mem name defs then
	let pos = pos_of_decl decl in
	defs, ((name, pos, Action put_decrypt) :: todo)
      else
	put_decrypt defs todo
  | Sign (pos, msg, decl, _) ->
      let typ = type_string_of_decl decl in
      let put_verify defs todo =
	oprintf "%s.%s_verify %s " package typ src;
	put_decl decl;
	out_string ";\n";
	defs, todo in
      let m = mktemp() in
      oprintf "let %s = %s.%s_signed_message %s in\n" m package typ src;
      let defs, todo = put_recv_subpattern m defs todo msg in
      let name = name_of_decl decl in
      if S.mem name defs then
	let pos = pos_of_decl decl in
	defs, ((name, pos, Action put_verify) :: todo)
      else
	put_verify defs todo
  | Hash (pos, msg) ->
      oprintf "%s.message_same %s\n(%s.hash " package src package;
      put_send_pattern defs msg;
      out_string ");\n";
      defs, todo
  | Bind (id, msg) ->
      let decl = id, Some (Msg (pos_of_id id)) in
      let defs, todo = put_recv_subpattern src defs todo (Decl decl) in
      put_recv_subpattern src defs todo msg

let put_recv_pattern src defs msg =
  let defs, todo = put_recv_subpattern src defs [] msg in
  match todo with
    [] -> ()
  | (name, pos, _) :: _ ->
      let msg = "no way found to bind " ^ name in
      failwith (Pos.pos_message pos msg)

(* Expressions *)

let put_expr expr =
  match expr with
    Ide id ->
      put_id id
  | New_key key_type ->
      oprintf "%s.new_%s()" package (string_of_key_type key_type)
  | New_nonce _ ->
      oprintf "%s.new_nonce()" package
  | Connect id ->
      oprintf "%s.connect " package;
      put_id id
  | Accept _ ->
      oprintf "%s.accept()" package
  | Receive id ->
      oprintf "%s.msg_of_message (%s.receive " package package;
      put_id id;
      out_string ")"

(* Choice points *)

let show_aborts pos =
  if !debug then
    let p = Pos.pos_msg pos "" in
    oprintf "%s.trace %S _e;\n" package p

let rec put_try put stmt stmts =
  match stmts with
    [] ->
      let _ = put stmt in
      ()
  | stmt' :: stmts ->
      out_string "try begin\n";
      let pos = put stmt in
      out_string "end\nwith _e ->\n";
      show_aborts pos;
      put_try put stmt' stmts

(* Recognizer of tail calls *)

let same_term term term' =
  match term, term' with
    Constant (_, const), Constant (_, const') ->
      const = const'
  | Identifier decl, Identifier decl' ->
      name_of_decl decl = name_of_decl decl'
  | Variable var, Variable var' ->
      name_of_id var = name_of_id var'
  | _ -> false

let rec same_terms terms terms' =
  match terms, terms' with
    [], [] -> true
  | term :: terms, term' :: terms' ->
      same_term term term' && same_terms terms terms'
  | _ -> false

let same_literal literal literal' =
  let pred, terms = literal in
  let pred', terms' = literal' in
  pred = pred' && same_terms terms terms'

let rec same_formula relies guars =
  match relies, guars with
    [], [] -> true
  | rely :: relies, guar :: guars ->
      let goal, body = rely in
      body = []
	&& same_literal goal guar
	&& same_formula relies guars
  | _ -> false

let rec same_decls ds ds' =
  match ds, ds' with
    [], [] -> true
  | d :: ds, d' :: ds' ->
      name_of_decl d = name_of_decl d' && same_decls ds ds'
  | _ -> false

let tail_call_allowed outputs rely stmt =
  match stmt with
    Return (_, _, guar, decls) ->
      same_decls outputs decls && same_formula rely guar
  | _ -> false

(* Statements *)

let rec put_stmt stmt =
  match stmt with
    Return (pos, defs, guar, decls) ->
      put_guar defs guar;
      put_pos pos;
      put_decls decls;
      put_nl()
  | Let (pos, def, decl, expr, stmt) ->
      put_pos pos;
      out_string "let ";
      put_decl decl;
      out_string " = ";
      put_expr expr;
      out_string " in\n";
      put_stmt stmt
  | To (pos, []) ->
      put_raise pos "no send branches"
  | To (pos, send :: sends) ->
      put_try put_send send sends
  | From (pos, _, []) ->
      put_raise pos "no receive branches"
  | From (pos, src, recv :: recvs) ->
      put_pos pos;
      begin
	match src with
	  Match id ->
	    oprintf "let _m = %s.message_of_msg " package;
	    put_id id
	| Recv id ->
	    oprintf "let _m = %s.receive " package;
	    put_id id
      end;
      out_string " in\n";
      put_try put_recv recv recvs
  | Call (pos, []) ->
      put_raise pos "no call branches"
  | Call (pos, call :: calls) ->
      put_try put_call call calls

(* Send branch *)

and put_send (pos, defs, guar, id, msg, stmt) =
  put_pos pos;
  put_guar defs guar;
  oprintf "%s.send " package;
  put_id id;
  out_string "\n";
  put_send_pattern S.empty msg;
  out_string ";\n";
  if !debug then begin
    let txt = Printf.sprintf "\n+(%s)\n" (string_of_message msg) in
    oprintf "print_string %S;\n" txt
  end;
  put_stmt stmt;
  pos

(* Receive branch *)

and put_recv (pos, defs, msg, rely, stmt) =
  put_pos pos;
  put_recv_pattern "_m" (set_of_defs defs) msg;
  put_rely rely;
  if !debug then begin
    let txt = Printf.sprintf "\n-(%s)\n" (string_of_message msg) in
    oprintf "print_string %S;\n" txt
  end;
  put_stmt stmt;
  pos

(* Call branch *)

and put_call (pos, guar_defs, guar, call, stmt) =
  put_pos pos;
  put_guar guar_defs guar;
  match call with
    None ->
      put_stmt stmt;
      pos
  | Some (path, input, out_defs, output, rely) ->
      let tail_call = tail_call_allowed output rely stmt in
      if not tail_call then begin
	out_string "let ";
	put_decls output;
	out_string " =\n"
      end;
      put_path path;
      put_decls input;
      if not tail_call then begin
	out_string " in\n";
	put_rely rely;
	put_stmt stmt
      end
      else
	put_nl();
      pos

(* Procedure *)

let put_proc intro (name, defs, input, rely, guar, output, stmt) =
  put_nl();
  let pos = pos_of_id name in
  put_pos pos;
  out_string intro;
  out_string " ";
  put_name name;
  out_string " ";
  put_decls input;
  out_string " =\n";
  oprintf "let _t = %s.copy _t in\n" package;
  put_rely rely;
  put_stmt stmt

let put_proc_interface (name, defs, input, rely, guar, output, stmt) =
  out_string "val ";
  put_name name;
  out_string " : ";
  put_types input;
  out_string " -> ";
  put_types output;
  put_nl ()

(* Prologue *)

let put_prologue() =
  oprintf "let _v = %S	(* Compiler version *)\n\n" Version.version;
  if !debug then begin
    let fmt = "Compiler version: %s\n" in
    oprintf "let _ =\nPrintf.printf %S _v;\n" fmt;
    let fmt = "Library version: %s\n\n" in
    oprintf "Printf.printf %S %s.version\n\n" fmt package
  end;
  oprintf "let _t = %s.create %s.size\n" package package

let put_prologue_interface () =
  oprintf "(* CPPL compiler version %S *)\n\n" Version.version;
  oprintf "val _t : theory\n\n"

let open_out_or_stdout file_name =
  if file_name = "-" then
    stdout
  else
    open_out file_name

let clean_up file_name =
  if file_name <> "-" then
    try
      close_out !out;
      Sys.remove file_name
    with _ -> ()

let print_program debug_flag file_name program =
  debug := debug_flag;
  out := open_out_or_stdout file_name;
  try
    put_prologue();
    match program with
      [] -> ()
    | proc :: program ->
	put_proc "let rec" proc;
	List.iter (put_proc "and") program
  with e ->
    clean_up file_name;
    raise e

let print_interface file_name program =
  out := open_out_or_stdout file_name;
  try
    put_prologue_interface ();
    List.iter put_proc_interface program
  with e ->
    clean_up file_name;
    raise e
