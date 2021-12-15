open Ast
open Buffer

let show_id buf (_, id) =
  add_string buf id

let show_decl buf (id, _) =
  show_id buf id

let rec show_message buf message =
  match message with
    Decl d ->
      show_decl buf d
  | Cat (_, mesg1, mesg2) ->
      show_message_item buf mesg1;
      add_string buf ", ";
      show_message buf mesg2
  | Tag (_, tag, mesg) ->
      add_string buf tag;
      add_string buf ", ";
      show_message buf mesg
  | Encrypt (_, message, decl, sym) ->
      add_string buf "{";
      if sym then
	add_string buf "|";
      show_message buf message;
      if sym then
	add_string buf "|";
      add_string buf "} ";
      show_decl buf decl
  | Sign (_, message, decl, sym) ->
      add_string buf "[";
      if sym then
	add_string buf "|";
      show_message buf message;
      if sym then
	add_string buf "|";
      add_string buf "] ";
      show_decl buf decl
  | Hash (_, message) ->
      add_string buf "hash(";
      show_message buf message;
      add_string buf ")"
  | Bind (id, message) ->
      add_string buf "<";
      show_id buf id;
      add_string buf " = ";
      show_message buf message;
      add_string buf ">"

and show_message_item buf message =
  match message with
    Cat (_, _, _) ->
      add_string buf "(";
      show_message buf message;
      add_string buf ")"
  | _ ->
      show_message buf message

let string_of_message msg =
  let buf = create 64 in
  show_message buf msg;
  contents buf
