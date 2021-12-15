(* The CPPL runtime library.
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

(**  CPPL Runtime *)

module type T =
  sig

val version : string
exception Cppl_abort of string

type name = Unix.sockaddr
val get_self : unit -> name
val set_self : int -> unit
val name_equals : name -> name -> bool

type pubkey
type symkey
type nonce
type text = string
type msg
type message
val string_of_nonce : nonce -> string
val message_of_pubkey : pubkey -> message
val message_of_symkey : symkey -> message
val message_of_nonce : nonce -> message
val message_of_name : name -> message
val message_of_text : text -> message
val message_of_msg : msg -> message
val pubkey_equals : pubkey -> pubkey -> bool
val symkey_equals : symkey -> symkey -> bool
val nonce_equals : nonce -> nonce -> bool
val text_equals : text -> text -> bool
val msg_equals : msg -> msg -> bool
val message_equals : message -> message-> bool
val pubkey_of_message : message -> pubkey
val symkey_of_message : message -> symkey
val nonce_of_message : message -> nonce
val name_of_message : message -> name
val text_of_message : message -> text
val msg_of_message : message -> msg
val concatenate : message -> message -> message
val separate : message -> message * message
val hash : message -> message
val pubkey_sign : message -> pubkey -> message
val pubkey_verify : message -> pubkey -> unit
val pubkey_signed_message : message -> message
val pubkey_encrypt : message -> pubkey -> message
val pubkey_decrypt : message -> pubkey -> message
val symkey_sign : message -> symkey -> message
val symkey_verify : message -> symkey -> unit
val symkey_signed_message : message -> message
val symkey_encrypt : message -> symkey -> message
val symkey_decrypt : message -> symkey -> message
val addtag : string -> message -> message
val untag : string -> message -> message
val new_nonce : unit -> nonce
val new_symkey : unit -> symkey
val new_pubkey : unit -> pubkey
val load_key_store_entry : in_channel -> text * pubkey
val save_key_pair_entry : out_channel -> text -> pubkey
val save_pubkey_entry : out_channel -> text -> pubkey -> unit

type channel
val connect : name -> channel
val accept : unit -> channel
val send : channel -> message -> unit
val receive : channel -> message
type v =
    Pub of pubkey
  | Sym of symkey
  | Nonce of nonce
  | Text of text
  | Name of name
type value = v
val value_equals : value -> value -> bool
val mk_pubkey : pubkey -> value
val mk_symkey : symkey -> value
val mk_nonce : nonce -> value
val mk_text : text -> value
val mk_name : name -> value
val to_pubkey : value -> pubkey
val to_symkey : value -> symkey
val to_nonce : value -> nonce
val to_text : value -> text
val to_name : value -> name
val string_of_value : value -> string
type term
val mkvar : string -> term
val mkval : value -> term
val spreadterm : (string -> 'a) -> (value -> 'a) -> term -> 'a
type literal
val mkliteral : string -> term list -> literal
val getpred : literal -> string
val getterms : literal -> term list
type clause
val mkclause : literal -> literal list -> clause
val gethead : clause -> literal
val getbody : clause -> literal list
type primitive = int -> value list -> value list option
val add_primitive : string -> int -> primitive -> unit
type theory
val create : int -> theory
val copy : theory -> theory
exception Unsafe_clause
val assume : theory -> clause -> unit
val retract : theory -> clause -> unit
val prove : theory -> literal -> literal list
val print_value : value -> unit
val trace_assume : bool -> theory -> clause -> unit
val trace_retract : bool -> theory -> clause -> unit
val prove_once : bool -> theory -> literal -> value list option
val trace : string -> exn -> unit
val size : int

val pubkey_same : pubkey -> pubkey -> unit
val symkey_same : symkey -> symkey -> unit
val nonce_same : nonce -> nonce -> unit
val text_same : text -> text -> unit
val name_same : name -> name -> unit
val msg_same : msg -> msg -> unit
val message_same : message -> message -> unit

end

module Make(Crypto: CpplSig.Crypto):
    T with type pubkey = Crypto.pubkey
    and type symkey = Crypto.symkey
    and type nonce = Crypto.nonce =
  struct

open Unix
open Sexpr
include Version
include Crypto

let debug = false (* true *)

(* Exception used to signal a failed computational path. *)

exception Cppl_abort of string

(* Names and their operations. *)

type name = sockaddr

let default_port = 27752

let myself =
  try
    let name = gethostname () in
    let { h_addr_list = addr_list; } = gethostbyname name in
      Array.get addr_list 0
  with
      Not_found -> inet_addr_loopback

let self =
   ref (ADDR_INET (myself, default_port))

let get_self () =
  !self

let set_self port =
  let port =
    if port < 0 then
      default_port
    else
      port in
  self := ADDR_INET (myself, port)

let me port =
  let name = gethostname () in
  let { h_addr_list = addr_list; } = gethostbyname name in
  ADDR_INET (Array.get addr_list 0, port)

let name_equals = (=)

let string_of_name name =
  let sexpr =
    match name with
      ADDR_INET (addr, port) ->
	List [ String "ADDR_INET"; String (string_of_inet_addr addr);
	       String (string_of_int port) ]
    | ADDR_UNIX s ->
	List [ String "ADDR_UNIX"; String s ] in
  string_of_sexpr sexpr

let name_of_string string =
  try
    let sexpr = sexpr_of_string string in
    match sexpr with
      List [ String "ADDR_INET"; String addr; String port ] ->
	ADDR_INET (inet_addr_of_string addr, int_of_string port)
    | List [ String "ADDR_UNIX"; String s ] ->
	ADDR_UNIX s
    | _ ->
	raise (Cppl_abort "name_of_string")
  with _ ->
    raise (Cppl_abort "name_of_string")

(* Transport layer. *)

type channel = {
    fd : file_descr;
    input : in_channel;
    output : out_channel
  }

let timeout = 30.0			(* timeout in seconds *)

let connect addr =
  let domain = domain_of_sockaddr addr in
  let fd = socket domain SOCK_STREAM 0 in
  connect fd addr;
  setsockopt_float fd SO_RCVTIMEO timeout;
  let input = in_channel_of_descr fd in
  let output = out_channel_of_descr fd in
  { fd = fd; input = input; output = output }

type server =
    Unbound
  | Bound of file_descr

let server = ref Unbound

let link () =
  match !server with
    Bound fd -> fd
  | Unbound ->
      let self = get_self() in
      let domain = domain_of_sockaddr self in
      let fd = socket domain SOCK_STREAM 0 in
      bind fd self;
      listen fd 5;
      setsockopt_optint fd SO_LINGER (Some 1);
      server := Bound fd;
      fd

let accept () =
  let fd, _ = accept (link ()) in
  setsockopt_float fd SO_RCVTIMEO timeout;
  let input = in_channel_of_descr fd in
  let output = out_channel_of_descr fd in
  { fd = fd; input = input; output = output }

let rec print_sexpr s =
  match s with
    String str ->
      Printf.eprintf "%S" str
  | List sl ->
      Printf.eprintf "(";
      begin match sl with
	[] ->
	  ()
      | s :: sl ->
	  print_sexpr s;
	  List.iter (fun s -> (Printf.eprintf " "; print_sexpr s)) sl
      end;
      Printf.eprintf ")"

let send_sexpr { output = output } s =
  if debug then begin
    Printf.eprintf "Message output\n";
    print_sexpr s;
    Printf.eprintf "\n"
  end;
  output_sexpr output s;
  flush output

let receive_sexpr { input = input } =
  try
    let s = input_sexpr input in
    if debug then begin
      Printf.eprintf "Message input\n";
      print_sexpr s;
      Printf.eprintf "\n"
    end;
    s
  with Unix_error (e, s, _) ->
    let msg = Printf.sprintf "%s: %s" s (error_message e) in
    raise (Cppl_abort msg)

(* Cryptography and message syntax. *)

type text = string

type message =
    Str of string
  | Name of name
  | Nonce of nonce
  | Pub of pubkey
  | Sym of symkey
  | Concatenate of message * message
  | Hash of string
  | Pub_signed of string * message
  | Sym_signed of string * message
  | Pub_encrypted of string * string
  | Sym_encrypted of string
  | Tag of string * message

let message_type msg =
  match msg with
    Str _ -> "str"
  | Name _ -> "name"
  | Nonce _ -> "nonce"
  | Pub _ -> "pub"
  | Sym _ -> "sym"
  | Concatenate _ -> "concatenate"
  | Hash _ -> "hash"
  | Pub_signed _ -> "pub_signed"
  | Sym_signed _ -> "sym_signed"
  | Pub_encrypted _ -> "pub_encrypted"
  | Sym_encrypted _ -> "sym_encrypted"
  | Tag (tag, _) -> "tag " ^ tag

let expect s m =
  let msg =
    Printf.sprintf
      "Expecting %s, but got %s"
      s (message_type m) in
  raise (Cppl_abort msg)

(* A more efficient implementation would equate a message with a
   sexpr, and not use the above data structure, however its use makes
   the module much easier to understand. *)

type msg = Submsg of message

let rec sexpr_of_message m =
  match m with
    Str s ->
      List [ String "Str"; String s ]
  | Name (ADDR_INET (addr, port)) ->
      List [ String "ADDR_INET"; String (string_of_inet_addr addr);
	     String (string_of_int port) ]
  | Name (ADDR_UNIX s) ->
      List [ String "ADDR_UNIX"; String s ]
  | Nonce n ->
      List [ String "Nonce"; String (string_of_nonce n) ]
  | Pub pk ->
      List [ String "Pub"; String (string_of_pubkey pk) ]
  | Sym s ->
      List [ String "Sym"; String (string_of_symkey s) ]
  | Concatenate (m1, m2) ->
      List [ String "Concatenate"; sexpr_of_message m1; sexpr_of_message m2 ]
  | Hash h ->
      List [ String "Hash"; String h ]
  | Pub_signed (signature, message) ->
      List [ String "Pub_signed"; String signature; sexpr_of_message message ]
  | Sym_signed (signature, message) ->
      List [ String "Sym_signed"; String signature; sexpr_of_message message ]
  | Pub_encrypted (s, d) ->
      List [ String "Pub_encrypted"; String s ; String d ]
  | Sym_encrypted s ->
      List [ String "Sym_encrypted"; String s ]
  | Tag (s, m) ->
      List [ String "Tag"; String s; sexpr_of_message m ]

let rec message_of_sexpr s =
  match s with
    List [ String "Str"; String s ] ->
      Str s
  | List [ String "ADDR_INET"; String addr; String port ] ->
      Name (ADDR_INET (inet_addr_of_string addr, int_of_string port))
  | List [ String "ADDR_UNIX"; String s ] ->
      Name (ADDR_UNIX s)
  | List [ String "Nonce"; String n ] ->
      Nonce (nonce_of_string n)
  | List [ String "Pub"; String pk ] ->
      Pub (pubkey_of_string pk)
  | List [ String "Sym"; String s ] ->
      Sym (symkey_of_string s)
  | List [ String "Concatenate"; s1; s2 ] ->
      Concatenate (message_of_sexpr s1, message_of_sexpr s2)
  | List [ String "Hash"; String h ] ->
      Hash h
  | List [ String "Pub_signed"; String signature; s1 ] ->
      Pub_signed (signature, message_of_sexpr s1)
  | List [ String "Sym_signed"; String signature; s1 ] ->
      Sym_signed (signature, message_of_sexpr s1)
  | List [ String "Pub_encrypted"; String s ; String d ] ->
      Pub_encrypted (s, d)
  | List [ String "Sym_encrypted"; String s ] ->
      Sym_encrypted s
  | List [ String "Tag"; String tag; s ] ->
      Tag (tag, message_of_sexpr s)
  | _ -> raise (Cppl_abort "message parsing failed")

(* The routines that convert between the wire protocol and messages. *)

let to_string message =
  string_of_sexpr (sexpr_of_message message)

let from_string string =
  message_of_sexpr (sexpr_of_string string)

let receive chan =
  message_of_sexpr (receive_sexpr chan)

let send chan message =
  send_sexpr chan (sexpr_of_message message)

let message_equals = (=)
let text_equals = (=)
let msg_equals = (=)

let message_of_text s =
  Str s

let message_of_msg (Submsg m) =
  m

let message_of_name p =
  Name p

let message_of_pubkey k =
  Pub k

let message_of_symkey k =
  Sym k

let message_of_nonce n =
  Nonce n

let text_of_message m =
  match m with
    Str s -> s
  | _ -> expect "text" m

let msg_of_message m =
  Submsg m

let name_of_message m =
  match m with
    Name p -> p
  | _ -> expect "name" m

let symkey_of_message m =
  match m with
    Sym k -> k
  | _ -> expect "symkey" m

let pubkey_of_message m =
  match m with
    Pub k -> k
  | _ -> expect "pubkey" m

let nonce_of_message m =
  match m with
    Nonce n -> n
  | _ -> expect "nonce" m

let concatenate m1 m2 =
  Concatenate (m1, m2)

let separate m =
  match m with
    Concatenate (m1, m2) ->
      (m1, m2)
  | _ ->
      expect "concatenate" m

let hash m =
  Hash (Crypto.hash (to_string m))

let key_pairs = Hashtbl.create 32   (* Maps public keys -> full keys *)

let get_key_pair k =		     (* Get the full key previously *)
  try					(* stored here *)
    Hashtbl.find key_pairs k
  with Not_found ->
    raise (Cppl_abort "private portion of key not known")

let store_key_pair k =		       (* Store the full key in the *)
  let stripped = pubkey_of k in	     (* hash table, and return just *)
  Hashtbl.replace key_pairs stripped k;	(* the public part *)
  stripped

let new_pubkey () =
  store_key_pair (new_keypair())

let addtag tag m =
  Tag (tag, m)

let untag tag m =
  match m with
    Tag (tag', m') when tag' = tag ->
      m'
  | _ ->
      expect ("tag " ^ tag) m

let pubkey_sign m k =
  let priv_key = get_key_pair k in
  Pub_signed (Crypto.pub_sign (to_string m) priv_key, m)

let pubkey_verify m k =
  match m with
    Pub_signed (signature, message) ->
      let valid =
	try
	  Crypto.pub_verify signature (to_string message) k
	with _ ->
	  raise (Cppl_abort "Error during signature validation") in
      if not valid then
	raise (Cppl_abort "invalid signature")
  | _ ->
      expect "pub_signed" m

let pubkey_signed_message m =
  match m with
    Pub_signed (signature, message) ->
      message
  | _ ->
      expect "pub_signed" m

let pubkey_encrypt m k =
  let secret, data = Crypto.pub_encrypt (to_string m) k in
  Pub_encrypted (secret, data)

let pubkey_decrypt m k =
  match m with
  | Pub_encrypted (secret, data) ->
      let priv_key = get_key_pair k in
      let contents =
	try
	  Crypto.pub_decrypt (secret, data) priv_key
	with _ ->
	    raise (Cppl_abort "Error during asymmetric decryption") in
      from_string contents
  | _ ->
      expect "pub_encrypt" m

let symkey_sign m k =
  let h = Crypto.hash (to_string m) in
  Sym_signed (Crypto.sym_encrypt h k, m)

let symkey_verify m k =
  match m with
    Sym_signed (signature, message) ->
      let h = Crypto.hash (to_string m) in
      if h <> Crypto.sym_decrypt signature k then
	raise (Cppl_abort "invalid signature")
  | _ ->
      expect "sym_signed" m

let symkey_signed_message m =
  match m with
    Sym_signed (signature, message) ->
      message
  | _ ->
      expect "sym_signed" m

let symkey_encrypt m k =
  Sym_encrypted (Crypto.sym_encrypt (to_string m) k)

let symkey_decrypt m k =
  match m with
    Sym_encrypted data ->
      let contents =
	try
	  Crypto.sym_decrypt data k
	with _ ->
	  raise (Cppl_abort "Error during symmetric decryption") in
      from_string contents
  | _ ->
      expect "sym_encrypt" m

let save_key_pair_entry output name =
  let key = new_keypair() in
  let str = string_of_keypair key in
  let sexpr = List [ String "key pair"; String name; String str] in
  output_sexpr output sexpr;
  pubkey_of key

let save_pubkey_entry output name k =
  let str = string_of_pubkey k in
  let sexpr = List [ String "pubkey"; String name; String str] in
  output_sexpr output sexpr

let load_key_store_entry input =
  match input_sexpr input with
    List [ String "key pair"; String name; String str] ->
      let keypair = keypair_of_string str in
      name, store_key_pair keypair
  | List [ String "pubkey"; String name; String str] ->
      name, pubkey_of_string str
  | _ -> failwith "load_key_store"

(* Values used by the trust manage system. *)

type v =
    Pub of pubkey
  | Sym of symkey
  | Nonce of nonce
  | Text of text
  | Name of name

let value_equals v v' =
  match v, v' with
    Pub x, Pub x' ->
      pubkey_equals x x'
  | Sym x, Sym x' ->
      symkey_equals x x'
  | Nonce x, Nonce x' ->
      nonce_equals x x'
  | Text x, Text x' ->
      text_equals x x'
  | Name x, Name x' ->
      name_equals x x'
  | _ -> false

let mk_pubkey x = Pub x
let mk_symkey x = Sym x
let mk_nonce x = Nonce x
let mk_text x = Text x
let mk_name x = Name x

let value_type msg =
  match msg with
    Pub _ -> "pub"
  | Sym _ -> "sym"
  | Nonce _ -> "nonce"
  | Text _ -> "text"
  | Name _ -> "name"

let expect s v =
  let msg =
    Printf.sprintf
      "Expecting %s, but got %s"
      s (value_type v) in
  raise (Cppl_abort msg)

let to_pubkey v =
  match v with
    Pub k -> k
  | _ -> expect "pub" v

let to_symkey v =
  match v with
    Sym k -> k
  | _ -> expect "sym" v

let to_nonce v =
  match v with
    Nonce k -> k
  | _ -> expect "nonce" v

let to_text v =
  match v with
    Text k -> k
  | _ -> expect "text" v

let to_name v =
  match v with
    Name n -> n
  | _ -> expect "name" v

let string_of_value value =
  match value with
    Pub key -> "K" ^ string_of_pubkey key
  | Sym key -> "S" ^ string_of_symkey key
  | Nonce nonce -> "N" ^ string_of_nonce nonce
  | Text text -> "T" ^ text
  | Name name -> "P" ^ string_of_name name

include Datalog.Make(struct
  type t = v
  let equal = value_equals
  let hash = Hashtbl.hash
end)

(* Display values *)

let abbrev = 40
    (* Set to zero to turn off abbreviations *)

let print_value value =
  let str = Printf.sprintf "%S" (string_of_value value) in
  let len = String.length str in
  if abbrev > 0 && len > abbrev then
    begin
      Format.print_string (String.sub str 0 abbrev);
      Format.print_string "...\""
    end
  else
    Format.print_string str

(* Formula printing for debugging *)

let print_term term =
  spreadterm Format.print_string print_value term

let print_literal literal =
  let symbol = getpred literal in
  let terms = getterms literal in
  match symbol, terms with
    "=", [a; b] ->                      (* print equality *)
      Format.open_box 2;
      print_term a;
      Format.print_space();
      Format.print_string "= ";
      print_term b;
      Format.close_box()
  | _ ->                                (* print predicate *)
      Format.print_string symbol;
      match terms with
	[] -> ()
      | term :: terms ->
	  Format.print_char '(';
	  Format.open_box 0;
	  print_term term;
	  let print_rest term =
	    Format.print_char ',';
	    Format.print_space();
	    print_term term in
	  List.iter print_rest terms;
	  Format.close_box();
	  Format.print_char ')'

let print_formula clause =
  let head = gethead clause in
  let body = getbody clause in
  Format.open_hvbox 4;
  print_literal head;
  begin match body with
    [] -> ()
  | literal :: literals ->
      Format.print_string " :-";
      Format.print_space();
      print_literal literal;
      let print_rest literal =
	Format.print_char ',';
	Format.print_space();
	print_literal literal in
      List.iter print_rest literals
  end;
  Format.close_box();
  Format.print_char '.'

(* Traced version of datalog functions *)

let trace_assume debug theory clause =
  if debug then begin
    Format.print_newline();
    Format.print_string "Asserted:";
    Format.print_newline();
    print_formula clause;
    Format.print_newline()
  end;
  assume theory clause

let trace_retract debug theory clause =
  if debug then begin
    Format.print_newline();
    Format.print_string "Retracted:";
    Format.print_newline();
    print_formula clause;
    Format.print_newline()
  end;
  retract theory clause

let getval v =
  let bad_term _ =
    failwith "variable should not be in term" in
  spreadterm bad_term (fun v -> v) v

(* Extracts values from an answer in an order that matches the
   introduction of variables in the query. *)

let mkbinding ins outs =
  let rec loop acc vars ins outs =
    match ins, outs with
      [], [] ->
	List.rev acc
    | i :: ins, o :: outs ->
	let whenvar s =
	  if List.mem s vars then
	    loop acc vars ins outs
	  else
	    loop (getval o :: acc) (s :: vars) ins outs in
	let whenval v =
	  loop acc vars ins outs in
	spreadterm whenvar whenval i
    | _ ->
	failwith "internal error" in
  loop [] [] ins outs

(* Returns None on failure, otherwise returns a list of values
   corresponding to the variables in the query.  The order of the
   values extracted from an answer is an order that matches the
   introduction of variables in the query. *)

let prove_once debug theory query =
  if debug then begin
    Format.print_newline();
    Format.print_string "Query:";
    Format.print_newline();
    print_literal query;
    Format.print_newline()
  end;
  let answers = prove theory query in
  if debug then begin
    Format.print_newline();
    Format.print_string "Answers:";
    Format.print_newline();
    List.iter (fun s -> print_literal s; Format.print_newline()) answers
  end;
  match answers with
    [] -> None
  | a :: _ -> Some (mkbinding (getterms query) (getterms a))

(* Print an exception trace when in debugging mode. *)

let trace p e =
  match e with
    Cppl_abort s ->
      print_string p;
      print_endline s
  | Failure s ->
      print_string p;
      print_endline s
  | _ -> ()

(* An initial size for a theory *)

let size = 31

(* Raise Cppl_abort on unequal values. *)

let pubkey_same x y =
  if not (pubkey_equals x y) then
    raise (Cppl_abort "differing pub keys")

let symkey_same x y =
  if not (symkey_equals x y) then
    raise (Cppl_abort "differing sym keys")

let nonce_same x y =
  if not (nonce_equals x y) then
    raise (Cppl_abort "differing nonces")

let text_same x y =
  if not (text_equals x y) then
    raise (Cppl_abort "differing texts")

let name_same x y =
  if not (name_equals x y) then
    raise (Cppl_abort "differing names")

let msg_same x y =
  if not (msg_equals x y) then
    raise (Cppl_abort "differing msgs")

let message_same x y =
  if not (message_equals x y) then
    raise (Cppl_abort "differing messages")

end
