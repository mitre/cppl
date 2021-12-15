(* Operations on source file token positions.
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

open Lexing

type pos = string * int * int * int

let mkpos lexbuf =
  let start_p = lexeme_start_p lexbuf in
  let end_p = lexeme_end_p lexbuf in
  start_p.pos_fname, start_p.pos_lnum,
  start_p.pos_cnum - start_p.pos_bol,
  end_p.pos_cnum - end_p.pos_bol

let pos_message (f, l, s, e) msg =
  Printf.sprintf
    "File \"%s\", line %d, at %d-%d: %s"
    f l s e msg

let pos_msg (f, l, _, _) msg =
  Printf.sprintf "File \"%s\", line %d: %s" f l msg

let pos_line (f, l, _, _) =
  Printf.sprintf "# %d \"%s\"" l f
