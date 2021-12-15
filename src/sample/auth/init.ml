open Unix
open Format
open Cppl

(* Principals *)
let me = "init"
let resp = "resp"
let ca = "ca"

let init () =
  let theory = Protocol._t in

  List.iter (assume theory) Init_axioms.axioms;

  (* Process command line arguments *)

  let sa = Unix.ADDR_INET (Unix.inet_addr_loopback, 0) in
  let responder = ref sa in		(* Silly default *)
  let cauth = ref sa in

  let get_keys = Load_key_store.load_key_store theory in

  (* set up Arg.parse *)

  let arg_spec =
    [("-r", Arg.String(fun s -> responder := Sockaddr.sockaddr s),
      "     - specify responder");
     ("-c", Arg.String(fun s -> cauth := Sockaddr.sockaddr s),
      "     - specify ca");
     ("--", Arg.Rest(get_keys),
      "     - treat remaining args as file names, where - means stdin")] in

  let usage_msg = "Usage: init [options] [ key-store ...]" in
  Arg.parse arg_spec get_keys usage_msg;
  Self.fact theory "at" [Text resp; Name !responder];
  Self.fact theory "at" [Text ca; Name !cauth];
  Self.fact theory "ca" [Text ca]

let _ =
  init ();
  printf "%s %s\n" me resp;

  let na, nb = Protocol.auth_init resp in
  printf "%s na " me;
  print_value (Nonce na);
  print_newline();
  printf "%s nb " me;
  print_value (Nonce nb);
  print_newline()
