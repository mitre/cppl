(* Infers the type of all identifiers.
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

open Pos
open Ast
open Scope

(* Determines if two types are compatible *)

let compat_key key_type key_type' =
  match key_type, key_type' with
    Pubkey _, Pubkey _ -> true
  | Symkey _, Symkey _ -> true
  | _, _ -> false

let compat opt_id_type opt_id_type' =
  match opt_id_type, opt_id_type' with
    None, _ -> true
  | _, None -> true
  | Some id_type, Some id_type' ->
      match id_type, id_type' with
      | Name _, Name _ -> true
      | Text _, Text _ -> true
      | Msg _, Msg _ -> true
      | Nonce _, Nonce _ -> true
      | Chan _, Chan _ -> true
      | Key k, Key k' -> compat_key k k'
      | _, _ -> false

(* An environment is a list of string def pairs *)

(* Lookup an identifier *)

let lookup env (pos, name) =
  try
    let _, def =
      List.find (fun (name', def) -> name = name') env in
    def
  with
    Not_found ->
      let msg = "internal error: cannot find def for " ^ name in
      failwith (pos_message pos msg)

(* Extend an enviroment *)

let extend env def =
  let (_, name), _ = def in
  (name, def) :: env

let type_conflict (pos, name) =
  let msg = "bad type for " ^ name in
  failwith (pos_message pos msg)

(* See if a decl is compatible with an enviroment *)

let merge env (id, opt_id_type) =
  if opt_id_type <> None then
    let _, typ = lookup env id in
    if compat !typ opt_id_type then
      typ := opt_id_type
    else
      type_conflict id

(* Check the types of identifiers by walking the abstract syntax
   tree. *)

let check_term env term =
  match term with
    Identifier decl -> merge env decl
  | _ -> ()

let check_literal env (_, terms) =
  List.iter (check_term env) terms

let check_clause env (goal, body) =
  check_literal env goal;
  List.iter (check_literal env) body

let check_rely env conjuncts =
  List.iter (check_clause env) conjuncts

let check_guar env conjuncts =
  List.iter (check_literal env) conjuncts

(* Returns the type of an expression *)

let check_expression env expr =
   match expr with
     Ide id ->
       let _, typ = lookup env id in
       !typ
   | New_key k ->
       Some (Key k)
   | New_nonce pos ->
       Some (Nonce pos)
   | Connect (pos, id) ->
       merge env ((pos,id), Some (Name pos));
       Some (Chan pos)
   | Accept pos ->
       Some (Chan pos)
   | Receive (pos, id) ->
       merge env ((pos,id), Some (Chan pos));
       Some (Msg pos)

let check_key mktype env decl =
  merge env decl;
  let (pos, name), _ = decl in
  merge env ((pos, name), Some (Key (mktype pos)))

let rec check_message env message =
  match message with
    Decl decl ->
      merge env decl
  | Cat (_, msg, msg') ->
      check_message env msg;
      check_message env msg'
  | Tag (_, _, msg) ->
      check_message env msg
  | Encrypt (_, msg, decl, sym) ->
      check_message env msg;
      let mktype pos = if sym then Symkey pos else Pubkey pos in
      check_key mktype env decl
  | Sign (_, msg, decl, sym) ->
      check_message env msg;
      let mktype pos = if sym then Symkey pos else Pubkey pos in
      check_key mktype env decl
  | Hash (_, msg) ->
      check_message env msg
  | Bind ((pos,id), msg) ->
      merge env ((pos, id), Some (Msg pos));
      check_message env msg

let check_def ((pos, name), typ) =
  if !typ = None then
    let msg = "no type for identifier " ^ name in
    failwith (pos_message pos msg)

let rec check_statement env statement =
  match statement with
    Return (_, defs, guar, decls) ->
      let env = List.fold_left extend env defs in
      check_guar env guar;
      List.iter (merge env) decls;
      List.iter check_def defs
  | Let (_, def, decl, expr, stmt) ->
      let typ = check_expression env expr in
      let env = extend env def in
      merge env decl;
      let id, _ = decl in
      merge env (id, typ);
      check_statement env stmt;
      check_def def
  | To (_, sends) ->
      List.iter (check_send env) sends
  | From (_, src, recvs) ->
      begin
	match src with
	  Match id ->
	    let pos, _ = id in
	    merge env (id, Some (Msg pos))
	| Recv id ->
	    let pos, _ = id in
	    merge env (id, Some (Chan pos))
      end;
      List.iter (check_recv env) recvs
  | Call(_, calls) ->
      List.iter (check_call env) calls

and check_send env (pos, defs, guar, id, message, statement) =
  let env = List.fold_left extend env defs in
  check_guar env guar;
  let pos, _ = id in
  merge env (id, Some (Chan pos));
  check_message env message;
  check_statement env statement;
  List.iter check_def defs

and check_recv env (pos, defs, message, rely, statement) =
  let env = List.fold_left extend env defs in
  check_message env message;
  check_rely env rely;
  check_statement env statement;
  List.iter check_def defs

and check_call env (pos, guar_defs, guar, call, statement) =
  let env = List.fold_left extend env guar_defs in
  check_guar env guar;
  match call with
    None ->
      check_statement env statement;
      List.iter check_def guar_defs
  | Some (path, input, out_defs, output, rely) ->
      List.iter (merge env) input;
      let env = List.fold_left extend env out_defs in
      List.iter (merge env) output;
      check_rely env rely;
      check_statement env statement;
      List.iter check_def guar_defs;
      List.iter check_def out_defs

and check_proc (name, defs, input, rely, guar, output, statement) =
  let env = List.fold_left extend [] defs in
  List.iter (merge env) input;
  check_rely env rely;
  (* Check guar and output at return site *)
  check_statement env statement;
  List.iter check_def defs

(* Substitute the results of the type check everywhere but in outputs
   and the guarantee of a procedure. *)

let subst env (id, _) =
  let _, typ = lookup env id in
  id, !typ

let subst_term env term =
  match term with
    Identifier decl ->
      Identifier (subst env decl)
  | _ -> term

let subst_literal env (pred, terms) =
  pred, List.map (subst_term env) terms

let subst_guar env conjuncts =
  List.map (subst_literal env) conjuncts

let subst_clause env (goal, body) =
  subst_literal env goal, List.map (subst_literal env) body

let subst_rely env conjuncts =
  List.map (subst_clause env) conjuncts

let subst_expression env expr = expr

let rec subst_message env expr =
  match expr with
    Decl decl ->
      Decl (subst env decl)
  | Cat (pos, msg, msg') ->
      Cat (pos, subst_message env msg, subst_message env msg')
  | Tag (pos, tag, msg) ->
      Tag (pos, tag, subst_message env msg)
  | Encrypt (pos, msg, decl, sym) ->
      Encrypt (pos, subst_message env msg, subst env decl, sym)
  | Sign (pos, msg, decl, sym) ->
      Sign (pos, subst_message env msg, subst env decl, sym)
  | Hash (pos, msg) ->
      Hash (pos, subst_message env msg)
  | Bind (id, msg) ->
      Bind (id, subst_message env msg)

let rec subst_statement env statement =
  match statement with
    Return (pos, defs, guar, decls) ->
      let env = List.fold_left extend env defs in
      let guar = subst_guar env guar in
      let decls = List.map (subst env) decls in
      Return (pos, defs, guar, decls)
  | Let (pos, def, decl, expr, stmt) ->
      let expr = subst_expression env expr in
      let env = extend env def in
      let decl = subst env decl in
      let stmt = subst_statement env stmt in
      Let (pos, def, decl, expr, stmt)
  | To (pos, sends) ->
      To (pos, List.map (subst_send env) sends)
  | From (pos, id, recvs) ->
      From (pos, id, List.map (subst_recv env) recvs)
  | Call(pos, calls) ->
      Call (pos, List.map (subst_call env) calls)

and subst_send env (pos, defs, guar, id, message, statement) =
  let env = List.fold_left extend env defs in
  let guar = subst_guar env guar in
  let message = subst_message env message in
  let statement = subst_statement env statement in
  pos, defs, guar, id, message, statement

and subst_recv env (pos, defs, message, rely, statement) =
  let env = List.fold_left extend env defs in
  let message = subst_message env message in
  let rely = subst_rely env rely in
  let statement = subst_statement env statement in
  pos, defs, message, rely, statement

and subst_call env (pos, guar_defs, guar, call, statement) =
  let env = List.fold_left extend env guar_defs in
  let guar = subst_guar env guar in
  match call with
    None ->
      let statement = subst_statement env statement in
      pos, guar_defs, guar, None, statement
  | Some (path, input, out_defs, output, rely) ->
      let input = List.map (subst env) input in
      let env = List.fold_left extend env out_defs in
      let output = List.map (subst env) output in
      let rely = subst_rely env rely in
      let call = Some (path, input, out_defs, output, rely) in
      let statement = subst_statement env statement in
      pos, guar_defs, guar, call, statement

let subst_proc (name, defs, input, rely, guar, output, statement) =
  let env = List.fold_left extend [] defs in
  let input = List.map (subst env) input in
  let rely = subst_rely env rely in
  let statement = subst_statement env statement in
  name, defs, input, rely, guar, output, statement

(* Check that the procedure's guarantee and output agree with what is
   at each return site *)

let rec check_out_statement env statement =
  match statement with
    Return (_, _, _, decls) ->
      List.iter (merge env) decls
  | Let (_, _, _, _, stmt) ->
      check_out_statement env stmt
  | To (_, sends) ->
      List.iter (check_out_send env) sends
  | From (_, _, recvs) ->
      List.iter (check_out_recv env) recvs
  | Call (_, calls) ->
      List.iter (check_out_call env) calls

and check_out_send env (pos, defs, guar, id, message, statement) =
  check_out_statement env statement

and check_out_recv env (pos, defs, message, rely, statement) =
  check_out_statement env statement

and check_out_call env (pos, guar_defs, guar, call, statement) =
  check_out_statement env statement

let mem env (_, id) =
  List.exists (fun (_, id') -> id = id') env

let grow env ((pos, id), _) =
  if mem env (pos, id) then
    env
  else
    (pos, id) :: env

let mkdef id =
  id, ref None

(* Check and then substitute the results of the type check in outputs
   and the guarantee of a procedure. *)

let check_out (name, defs, input, rely, guar, output, statement) =
  let env = List.fold_left grow [] input in
  let env = List.fold_left grow env output in
  let defs = List.map mkdef env in
  let env = List.fold_left extend [] defs in
  List.iter (merge env) input;
  List.iter (merge env) output;
  check_out_statement env statement;
  List.iter check_def defs;
  let guar = subst_guar env guar in
  let output = List.map (subst env) output in
  name, defs, input, rely, guar, output, statement

let infer program =
  let program = scope program in
  List.iter check_proc program;
  let program = List.map subst_proc program in
  List.map check_out program
