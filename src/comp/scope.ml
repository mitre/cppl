(* Analyze the scope of identifiers.
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

(* Determines the scope of identifiers and returns a version of the
   program with def lists added to statements that record the
   identifiers introduced by parts of the statement and return
   statements that contain a copy of the procedure's output
   identifiers and its guarantee. *)

open Pos
open Ast

let debug = false

let show_ids ids =
  print_string "[";
  begin match ids with
    [] -> ()
  | (_, id) :: ids ->
      print_string id;
      List.iter (fun (_, id) -> print_string " "; print_string id) ids
  end;
  print_endline "]"

(* Environments used during scope analysis are lists of ids *)

let mem env (_, id) =
  List.exists (fun (_, id') -> id = id') env

(* Functions that take ids *)

let add_identifier env id =
  if mem env id then
    env
  else
    id :: env

let unbound_identifier env (pos, id) =
  if mem env (pos, id) then
    env
  else
    failwith (pos_message pos ("identifier " ^ id ^ " is unbound"))

(* Functions that take decls *)

let no_grow env (id, _) =
  unbound_identifier env id

let grow env ((pos, id), _) =
  if mem env (pos, id) then
    env
  else
    (pos, id) :: env

let grow_no_dups env ((pos, id), _) =
  if mem env (pos, id) then
    failwith (pos_message pos ("identifier " ^ id ^ " already seen"))
  else
    (pos, id) :: env

let id_of_src src =
  match src with
    Match id -> id
  | Recv id -> id

(* Resolve the scope of identifiers by walking the abstract syntax
   tree.  The function cont is used to extend the environment when an
   identifier is found. *)

let resolve_term cont env term =
  match term with
    Identifier (id, _) ->
      cont env id
  | _ -> env

let resolve_literal cont env (_, terms) =
  List.fold_left (resolve_term cont) env terms

let resolve_guar cont env conjuncts =
  List.fold_left (resolve_literal cont) env conjuncts

let resolve_clause cont env (goal, body) =
  let env = resolve_literal cont env goal in
  List.fold_left (resolve_literal cont) env body

let rec resolve_rely cont env conjuncts =
  List.fold_left (resolve_clause cont) env conjuncts

let resolve_expression cont env expr =
  match expr with
    Ide id ->
      cont env id
  | Connect id ->
      cont env id
  | Receive id ->
      cont env id
  | _ -> env

let rec resolve_message cont env msg =
  match msg with
    Decl (id, _) ->
      cont env id
  | Cat (_, msg, msg') ->
      resolve_message cont (resolve_message cont env msg) msg'
  | Tag (_, _, msg) ->
      resolve_message cont env msg
  | Encrypt (_, msg, (id, _), _) ->
      resolve_message cont (cont env id) msg
  | Sign (_, msg, (id, _), _) ->
      resolve_message cont (cont env id) msg
  | Hash (_, msg) ->
      resolve_message cont env msg
  | Bind (id, msg) ->
      resolve_message cont (cont env id) msg

(* list_diff is used to find out what has been added to an environment *)

let list_diff l l' =
  let rec loop a l =
    if l = l' then
      a
    else if l = [] then
      failwith "internal error, bad list difference"
    else
      loop (List.hd l :: a) (List.tl l) in
  loop [] l

let mkdef id =
  id, ref None

let rec resolve_statement prove out env statement =
  match statement with
    Return (pos, _, _, _) ->
      let env' = resolve_guar add_identifier env prove in
      let ids = list_diff env' env in
      if debug then begin print_string "return "; show_ids ids end;
      let defs = List.map mkdef ids in
      let _ = List.map (no_grow env') out in
      Return (pos, defs, prove, out)
  | Let (pos, def, decl, expr, stmt) ->
      let env = resolve_expression unbound_identifier env expr in
      let env' = grow_no_dups env decl in
      let stmt = resolve_statement prove out env' stmt in
      Let (pos, def, decl, expr, stmt)
  | To (pos, sends) ->
      To (pos, List.map (resolve_send prove out env) sends)
  | From (pos, src, recvs) ->
      let env = unbound_identifier env (id_of_src src) in
      From (pos, src, List.map (resolve_recv prove out env) recvs)
  | Call(pos, calls) ->
      Call (pos, List.map (resolve_call prove out env) calls)

and resolve_send prove out env
    (pos, _, guar, id, message, statement) =
  let env' = resolve_guar add_identifier env guar in
  let env' = unbound_identifier env' id in
  let ids = list_diff env' env in
  if debug then begin print_string "send "; show_ids ids end;
  let defs = List.map mkdef ids in
  let env' = resolve_message unbound_identifier env' message in
  let stmt = resolve_statement prove out env' statement in
  pos, defs, guar, id, message, stmt

and resolve_recv prove out env
    (pos, _, message, rely, statement) =
  let env' = resolve_message add_identifier env message in
  let ids = list_diff env' env in
  if debug then begin print_string "recv "; show_ids ids end;
  let defs = List.map mkdef ids in
  let env' = resolve_rely unbound_identifier env' rely in
  let stmt = resolve_statement prove out env' statement in
  pos, defs, message, rely, stmt

and resolve_call prove out env (pos, _, guar, call, statement) =
  let env' = resolve_guar add_identifier env guar in
  let ids = list_diff env' env in
  if debug then begin print_string "call guar"; show_ids ids end;
  let guar_defs = List.map mkdef ids in
  match call with
    None ->
      let stmt = resolve_statement prove out env' statement in
      pos, guar_defs, guar, None, stmt
  | Some (path, input, _, output, rely) ->
      let env' = List.fold_left no_grow env' input in
      let env'' = List.fold_left grow_no_dups env' output in
      let ids = list_diff env'' env' in
      if debug then begin print_string "call outs"; show_ids ids end;
      let out_defs = List.map mkdef ids in
      let env'' = resolve_rely unbound_identifier env'' rely in
      let call = Some (path, input, out_defs, output, rely) in
      let stmt = resolve_statement prove out env'' statement in
      pos, guar_defs, guar, call, stmt

let resolve (name, _, input, rely, guar, output, statement) =
  let env = List.fold_left grow_no_dups [] input in
  let env = resolve_rely unbound_identifier env rely in
  let env' = List.fold_left grow env output in
  let _ = resolve_guar unbound_identifier env' guar in
  let defs = List.map mkdef env in
  if debug then begin
    let _, s = name in
    print_string s;
    print_string " ";
    show_ids env
  end;
  let statement = resolve_statement guar output env statement in
  name, defs, input, rely, guar, output, statement

let scope program =
  List.map resolve program
