open Cppl

let load_key_store theory file_name =
  let input =
    if file_name = "-" then
      stdin
    else
      open_in_bin file_name in
  try
    while true do
      let p, k = load_key_store_entry input in
      let pubkey =
	mkliteral "owns"
	  [mkval (Text p); mkval (Pub k)] in
      assume theory (mkclause pubkey [])
    done
  with End_of_file -> close_in input
