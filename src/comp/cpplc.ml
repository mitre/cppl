(** A main routine for a CPPL compiler. *)

(*
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

(** This section is all for argument processing *)

type mode = Pretty | Typed | Scheme | Code | Interface | Debug
(** Ouput mode *)

let show_version () =
  print_endline Version.version;
  exit 0

let get_args() =

  (* file names *)

  (** defaults to stdout *)
  let output_file = ref "-" in

  (** defaults to stdin *)
  let input_file = ref "-" in

  (** Ouput mode.  Specify default here *)
  let output_format = ref Code in

  let axioms = ref false in

  (** [anon_var] is used to track the number of free-floating
     arguments.  0 means we have seen none, 1 means we have seen 1, 2
     means we have seen too many. *)

  let anon_var = ref 0 in

  let anon_fun str =
    match !anon_var with
      0 ->
	input_file := str;
	anon_var := 1
    | _ ->
	anon_var := 2 in

  (* set up Arg.parse *)

  let arg_spec =
    [("-o", Arg.String(fun s -> output_file := s),
      "name - send output to this file (default stdout)" );
     ("-a", Arg.Set(axioms),
      "     - read axioms instead of a program" );
     ("-i", Arg.Unit(fun () -> output_format := Pretty),
      "     - indent program" );
     ("-t", Arg.Unit(fun () -> output_format := Typed),
      "     - indent program and add implied types" );
     ("-s", Arg.Unit(fun () -> output_format := Scheme),
      "     - generate Scheme S-Expression syntax" );
     ("-c", Arg.Unit(fun () -> output_format := Code),
      "     - generate code (default)" );
     ("-n", Arg.Unit(fun () -> output_format := Interface),
      "     - generate interface file" );
     ("-d", Arg.Unit(fun () -> output_format := Debug),
      "     - generate debugging code" );
     ("-v", Arg.Unit(show_version),
      "     - show version" );
     ("--", Arg.Rest(anon_fun),
      "     - treat remaining args as file names, where - means stdin")] in

  let usage_msg = "Usage: cpplc [-o output] [-aitscndv] [input]" in

  Arg.parse arg_spec anon_fun usage_msg;

  if !anon_var > 1 then
    begin
      prerr_string "bad arg count";
      prerr_newline();
      prerr_string usage_msg;
      prerr_newline();
      exit 1
    end;

  !output_format, !axioms, !input_file, !output_file

let run (output_format, axioms, input_file, output_file) =
  try
    if axioms then
      let ast = Reader.read_axioms input_file in
      if output_format = Scheme then
	Scheme.print_axioms output_file ast
      else
	Axioms.print_axioms output_file ast
    else
      let ast = Reader.read_program input_file in
      match output_format with
	Pretty -> Pretty.print_program output_file ast
      | Typed -> Pretty.print_program output_file (Infer.infer ast)
      | Scheme -> Scheme.print_program output_file (Infer.infer ast)
      | Code -> Printer.print_program false output_file (Infer.infer ast)
      | Interface -> Printer.print_interface output_file (Infer.infer ast)
      | Debug -> Printer.print_program true output_file (Infer.infer ast)
  with Failure s ->
    prerr_string s;
    prerr_newline();
    exit 1

let _ =
  run (get_args())
