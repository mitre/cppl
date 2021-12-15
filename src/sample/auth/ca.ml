open Format
open Cppl

let me = "ca"

let set_self_port () =
  let arg_spec =
    [("-p", Arg.Int(set_self),
      "     - specify a port")] in

  let usage_msg = "Usage: ca [options]" in
  Arg.parse arg_spec (fun s -> raise (Arg.Bad s)) usage_msg

let init () =
  set_self_port ();
  let kb = new_pubkey() in

  let output = open_out_bin (me ^ ".ks") in
  save_pubkey_entry output me kb;
  close_out output;

  let theory = Protocol._t in
  Self.fact theory "owns" [Text me; Pub kb];
  List.iter (assume theory) Ca_axioms.axioms;
  Load_key_store.load_key_store theory "resp.ks";
  add_primitive "self" 0 (Self.self me)

let _ =
  init ();
  printf "%s " me;
  print_value (Name (get_self()));
  print_newline();
  flush stdout;

  let na = Protocol.auth_ca () in
  printf "%s n " me;
  print_value (Nonce na);
  print_newline()
