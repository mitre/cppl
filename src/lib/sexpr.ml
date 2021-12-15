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

type sexpr =
    String of string
  | List of sexpr list

exception Sexpr_input_error of string

let parse_error msg =
  raise (Sexpr_input_error msg)

(* For channel input *)

let parse_string chan ch =
  let rec loop len =			(* loop to read the length *)
    let ch = input_char chan in		(* of a string *)
    if ch >= '0' && ch <= '9' then
      let digit = int_of_char ch - int_of_char '0' in
      let len = 10 * len + digit in
      if len <= 0 then			(* ensure no overflow *)
	parse_error "Bad string size"
      else
	loop len
    else if ch = ':' then
      if len > Sys.max_string_length then
	parse_error "Bad string size"
      else
	let buf = String.create len in
	really_input chan buf 0 len;	(* real read of the string *)
	String buf
    else
      parse_error "Error in string" in
  loop (int_of_char ch - int_of_char '0')

let rec parse_sexpr chan ch =
  if ch >= '1' && ch <= '9' then	(* read non-empty string *)
    parse_string chan ch
  else if ch = '0' then			(* read an empty string *)
    let ch = input_char chan in
    if ch = ':' then
      String ""
    else
      parse_error "Error in empty string"
  else if ch = '(' then			(* read a list *)
    let rec loop list =
      let ch = input_char chan in
      if ch = ')' then
	List (List.rev list)
      else
	loop (parse_sexpr chan ch :: list) in
    loop []
  else
    parse_error "Bad token"

let input_sexpr chan =
  let ch = input_char chan in
  try
    parse_sexpr chan ch
  with
    End_of_file ->
      parse_error "Incomplete s-expression found"

(* For string input *)

let stream_string stream ch =
  let rec loop len =			(* loop to read the length *)
    let ch = Stream.next stream in		(* of a string *)
    if ch >= '0' && ch <= '9' then
      let digit = int_of_char ch - int_of_char '0' in
      let len = 10 * len + digit in
      if len <= 0 then			(* ensure no overflow *)
	parse_error "Bad string size"
      else
	loop len
    else if ch = ':' then
      if len > Sys.max_string_length then
	parse_error "Bad string size"
      else
	let buf = String.create len in
	for i = 0 to len - 1 do
	  String.set buf i (Stream.next stream)	(* load buf *)
	done;
	String buf
    else
      parse_error "Error in string" in
  loop (int_of_char ch - int_of_char '0')

let rec stream_sexpr stream ch =
  if ch >= '1' && ch <= '9' then	(* read non-empty string *)
    stream_string stream ch
  else if ch = '0' then			(* read an empty string *)
    let ch = Stream.next stream in
    if ch = ':' then
      String ""
    else
      parse_error "Error in empty string"
  else if ch = '(' then			(* read a list *)
    let rec loop list =
      let ch = Stream.next stream in
      if ch = ')' then
	List (List.rev list)
      else
	loop (stream_sexpr stream ch :: list) in
    loop []
  else
    parse_error "Bad token"

let sexpr_of_string string =
  let stream = Stream.of_string string in
  try
    let sexpr = stream_sexpr stream (Stream.next stream) in
    Stream.empty stream;       (* Ensure only one sexpr is in string *)
    sexpr
  with
    Stream.Failure ->
      parse_error "Incomplete s-expression found"

(* For channel output *)

let rec output_sexpr chan sexpr =
  match sexpr with
    String string ->
      output_string chan (string_of_int (String.length string));
      output_char chan ':';
      output_string chan string
  | List list ->
      output_char chan '(';
      List.iter (output_sexpr chan) list;
      output_char chan ')'

(* For string output *)

let string_of_sexpr sexpr =
  let buf = Buffer.create 64 in
  let rec to_string sexpr =
    match sexpr with
      String string ->
	Buffer.add_string buf (string_of_int (String.length string));
	Buffer.add_char buf ':';
	Buffer.add_string buf string
    | List list ->
	Buffer.add_char buf '(';
	List.iter to_string list;
	Buffer.add_char buf ')' in
  to_string sexpr;
  Buffer.contents buf
