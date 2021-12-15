(* A lexical analyzer for CPPL input.
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

{

open Lexing
open Parser

(* Compute the position of a lexeme *)
let pos = Pos.mkpos

(* Update the current location with file name and line number. *)

let update_loc lexbuf file line absolute =
  let pos = lexbuf.lex_curr_p in
  let new_file =
    match file with
    | None -> pos.pos_fname
    | Some s -> s in
  let new_line =
    if absolute then line else pos.pos_lnum + line in
  lexbuf.lex_curr_p
  <- { pos with
       pos_fname = new_file;
       pos_lnum = new_line;
       pos_bol = pos.pos_cnum;
     }

let init lexbuf file_name =
  update_loc lexbuf (Some file_name) 1 true

let newline_found lexbuf =
  update_loc lexbuf None 1 false

(* Quoted string handling *)

(* Convert the external version of string into the internal representation *)
let strip str =
  Scanf.sscanf str "%S" (fun s -> s)

(* Keywords that scan as an identifier use a keyword table *)

let mktbl () =
  let keywords = [
    "accept", (fun pos -> ACCEPT pos);
    "and", (fun pos -> AND pos);
    "assumes", (fun pos -> ASSUMES pos);
    "call", (fun pos -> CALL pos);
    "cases", (fun pos -> CASES pos);
    "channel", (fun pos -> CHANNEL pos);
    "connect", (fun pos -> CONNECT pos);
    "end", (fun pos -> END pos);
    "hash", (fun pos -> HASH pos);
    "in", (fun pos -> IN pos);
    "let", (fun pos -> LET pos);
    "match", (fun pos -> MATCH pos);
    "msg", (fun pos -> MSG pos);
    "name", (fun pos -> NAME pos);
    "new", (fun pos -> NEW pos);
    "nonce", (fun pos -> NONCE pos);
    "proves", (fun pos -> PROVES pos);
    "pubkey", (fun pos -> PUBKEY pos);
    "receive", (fun pos -> RECEIVE pos);
    "return", (fun pos -> RETURN pos);
    "send", (fun pos -> SEND pos);
    "symkey", (fun pos -> SYMKEY pos);
    "text", (fun pos -> TEXT pos) ] in
  let table = Hashtbl.create(List.length keywords) in
  List.iter (fun (kwd, tok) -> Hashtbl.add table kwd tok) keywords;
  table

let table = mktbl()
let find_keyword_or_id lexbuf =
  let item = lexeme lexbuf in
  try
    Hashtbl.find table item (pos lexbuf)
  with Not_found ->
    ID(pos lexbuf, item)
}

let newline = ('\n' | '\r' | "\r\n")
let var_start = ['A'-'Z']
let con_start = ['0' - '9']
let id_start = ['a'-'z']
let part = ['A'-'Z' 'a'-'z' '_' '0'-'9']

let identifier = id_start part*
let const = con_start part*
let variable = var_start part*
let str_chars = [^ '"' '\\'] | "\\\\" | "\\\""  | "\\'"
| "\\n" | "\\r" | "\\t" | "\\b"
| "\\" [ '0'-'9' ]  [ '0'-'9' ]  [ '0'-'9' ]
let str = '"' str_chars* '"'

rule token = parse
  [' ' '\t']                { token lexbuf }
| newline                   { newline_found lexbuf; token lexbuf }
  (* skip comments starting with the percent sign '%' *)
| '%' [^ '\n' '\r' ]* newline { newline_found lexbuf; token lexbuf }
  (* and ML style comments *)
| "(*"			    { comment lexbuf; token lexbuf }
  (* handle line number directives *)
| "#" [' ' '\t']* (['0'-'9']+ as num) [' ' '\t']*
	("\"" ([^ '\n' '\r' '"' ] * as name) "\"")?
	[^ '\n' '\r']* newline
			    { update_loc lexbuf name
				(int_of_string num) true;
			      token lexbuf }
| "-->"                     { ARROW(pos lexbuf) }
| '|'                       { BAR(pos lexbuf) }
| ">"                       { CANGLE(pos lexbuf) }
| "]"                       { CBRAC(pos lexbuf) }
| "|]"                      { CBRACBAR(pos lexbuf) }
| "}"                       { CCURL(pos lexbuf) }
| "|}"                      { CCURLBAR(pos lexbuf) }
| ')'                       { CPAREN(pos lexbuf) }
| ':'                       { COLON(pos lexbuf) }
| ','                       { COMMA(pos lexbuf) }
| '='                       { EQUAL(pos lexbuf) }
| ":-"                      { IMPLY(pos lexbuf) }
| "<"                       { OANGLE(pos lexbuf) }
| "["                       { OBRAC(pos lexbuf) }
| "[|"                      { OBRACBAR(pos lexbuf) }
| "{"                       { OCURL(pos lexbuf) }
| "{|"                      { OCURLBAR(pos lexbuf) }
| '('                       { OPAREN(pos lexbuf) }
| '.'                       { PERIOD(pos lexbuf) }
| '_'                       { UNDERSCORE(pos lexbuf) }
| identifier                { find_keyword_or_id lexbuf }
| const                     { CONST(pos lexbuf, lexeme lexbuf) }
| variable                  { VARIABLE(pos lexbuf, lexeme lexbuf) }
| str                       { STRING(pos lexbuf, strip(lexeme lexbuf)) }
| eof                       { EOF }
| _                         { raise Parsing.Parse_error }
and comment = parse
  "*)"			    { () }
| newline                   { newline_found lexbuf; comment lexbuf }
| "(*"			    { comment lexbuf; comment lexbuf }
| _	                    { comment lexbuf }
