open Format
open Cppl

let me = Globals.ca

let init () =
  let theory = Protocol._t in
  (* Process command line arguments *)
  let get_keys = Load_key_store.load_key_store theory in
  (* set up Arg.parse *)
  let arg_spec =
    [("-p", Arg.Int(set_self),
      "     - specify a port");
     ("--", Arg.Rest(get_keys),
      "     - treat remaining args as file names, where - means stdin")] in
  let usage_msg = "Usage: ca [options] [ key-store ...]" in
  Arg.parse arg_spec get_keys usage_msg

let _ =
  init ();
  printf "%s " me;
  print_value (Name (get_self()));
  print_newline();
  flush stdout;

  let n = Protocol.ca me in
  printf "%s n " me;
  print_value (Nonce n);
  print_newline()
