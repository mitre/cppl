open Format
open Cppl

let me = Globals.resp

let init () =
  let theory = Protocol._t in
  List.iter (assume theory) Resp_axioms.axioms;
  (* Process command line arguments *)
  let get_keys = Load_key_store.load_key_store theory in
  (* set up Arg.parse *)
  let arg_spec =
    [("-p", Arg.Int(set_self),
      "     - specify a port");
     ("--", Arg.Rest(get_keys),
      "     - treat remaining args as file names, where - means stdin")] in
  let usage_msg = "Usage: resp [options] [ key-store ...]" in
  Arg.parse arg_spec get_keys usage_msg

let _ =
  init ();
  printf "%s " me;
  print_value (Name (get_self()));
  print_newline();
  flush stdout;

  let a, c, d, v = Protocol.resp me in
  printf "%s a %s\n%s c %s\n%s d %s\n%s v %s\n" me a me c me d me v
