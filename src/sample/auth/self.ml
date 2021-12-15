(* A Datalog primitive used the get oneself's name. *)

open Cppl

let self me n l =
  match n, l with
    1, [] ->
      Some [Text me]
  | _ -> None

let fact theory pred terms =
  let literal = mkliteral pred (List.map mkval terms) in
  assume theory (mkclause literal [])
