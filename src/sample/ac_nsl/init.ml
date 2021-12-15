open Unix
open Format
open Cppl

(* Principals *)
let me = Globals.init
let resp = Globals.resp
let ca = Globals.ca

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
  Globals.fact theory "at" [Text resp; Name !responder];
  Globals.fact theory "at" [Text ca; Name !cauth];
  Globals.fact theory "ca" [Text ca]

let _ =
  init ();
  let d = "resource" in
  printf "%s b %s\n\n%s d %s\n" me resp me d;

  let n, q, v = Protocol.init (me, resp, d) in
  printf "%s n " me;
  print_value (Nonce n);
  printf "\n%s q %s\n%s v %s\n" me q me v
