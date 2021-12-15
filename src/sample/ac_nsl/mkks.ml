(* Given a set of principals, this program creates a key store for
   each principal.  Each key store has one public-private key pair,
   and for all other principals, a binding of the principal to its
   public key.  The set of principals are specified on the program's
   command line. *)

open Cppl

let get_args () =
  let args = ref ([] : string list) in
  let args_fun str =
    args := str :: !args in
  (* set up Arg.parse *)
  let arg_spec =
    [("--", Arg.Rest(args_fun),
      "     - treat remaining args as names")] in
  let usage_msg = "Usage: mkks [names]..." in
  Arg.parse arg_spec args_fun usage_msg;
  List.rev !args

let mk_keystore name =
  let chan = open_out_bin (name ^ ".ks") in
  let pubkey = save_key_pair_entry chan name in
  chan, pubkey, name

let mk_cert self chan (_, pubkey, principal) =
  if principal <> self then
    save_pubkey_entry chan principal pubkey

let main() =
  let names = get_args() in
  let keystores = List.map mk_keystore names in
  let certs (chan, pubkey, self) =
    List.iter (mk_cert self chan) keystores in
  List.iter certs keystores

let _ =
  try
    main()
  with Failure s ->
    prerr_string "Fatal error: ";
    prerr_endline s;
    exit 1
