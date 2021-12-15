(* A reader for CPPL programs.
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

let report_error lexbuf msg =
  failwith (pos_message (mkpos lexbuf) msg)

let report_parse_error lexbuf =
  report_error lexbuf "syntax error"

let open_in_or_stdin file_name =
  if file_name = "-" then
    stdin
  else
    open_in file_name

let read start file_name =
  let chan = open_in_or_stdin file_name in
  let lexbuf = Lexing.from_channel chan in
  Scanner.init lexbuf file_name;
  try
    let prog =
      start Scanner.token lexbuf in
    close_in chan;
    prog
  with Parsing.Parse_error ->
    close_in chan;
    report_parse_error lexbuf
  | Failure s ->
    close_in chan;
    report_error lexbuf s

(* Reads a CPPL program.  The argument is the name of the file.
   Standard input is used if the file name is "-". *)
let read_program file_name =
  read Parser.program file_name

(* Reads Datalog assertions.  The argument is the name of the file.
   Standard input is used if the file name is "-". *)
let read_axioms file_name =
  read Parser.axioms file_name
