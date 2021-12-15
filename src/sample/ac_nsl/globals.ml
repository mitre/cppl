open Cppl

(* Principals *)

let init = "init"
let resp = "resp"
let ca = "ca"

let fact theory pred terms =
  let literal = mkliteral pred (List.map mkval terms) in
  assume theory (mkclause literal [])
