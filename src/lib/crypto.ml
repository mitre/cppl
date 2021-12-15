(* Cryptography.
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

open Sexpr
open Cryptokit

type pubkey = RSA.key

type keypair = RSA.key

type symkey = string

type nonce = string

let nonce_size = 128			(* Size of a nonce in bytes *)

(* Random number generator used to generate nonces and keys *)
let rand = Random.pseudo_rng (Random.string Random.secure_rng 20)

let new_nonce () =
  let buf = String.create nonce_size in
  rand#random_bytes buf 0 nonce_size;
  buf

let pubkey_of k =
  { RSA.size = k.RSA.size;
    RSA.n = k.RSA.n;
    RSA.e = k.RSA.e;
    RSA.d = "";
    RSA.p = "";
    RSA.q = "";
    RSA.dp = "";
    RSA.dq = "";
    RSA.qinv = "";
  }

let asym_key_size = 1024	   (* always a 1024 bit key for RSA *)

let new_keypair () =
  RSA.new_key ~rng:rand asym_key_size

let sym_key_size = 16

let new_symkey () =		    (* always a 128-bit key for AES *)
  let buf = String.create sym_key_size in
  rand#random_bytes buf 0 sym_key_size;
  buf

let pubkey_equals = (=)
let symkey_equals = (=)
let nonce_equals = (=)

let hash s =
  let hasher = Hash.sha1() in
  hasher#add_string s;
  let result = hasher#result in
  hasher#wipe;
  result

let hash_size =
  String.length (hash "Tell me your size")

let last_of size buf =
  let len = String.length buf in
  if len > size then
    String.sub buf (len - size) size
  else
    buf

let pub_sign c k =
  RSA.sign k (hash c)

let pub_verify s c k =
  let digest = last_of hash_size (RSA.unwrap_signature k s) in
  let h = hash c in
  h = digest

let one_use_cipher key direction blob =
  let cipher = Cipher.aes ~pad:Padding.length key direction in
  cipher#put_string blob;
  cipher#finish;
  let b = cipher#get_string in
  cipher#wipe;
  b

let sym_encrypt c k =
  one_use_cipher k Cipher.Encrypt c

let pub_encrypt c k =
  let sym_key = new_symkey () in
  let secret = RSA.encrypt k sym_key in
  let data = sym_encrypt c sym_key in
  secret, data

let sym_decrypt data k =
  one_use_cipher k Cipher.Decrypt data

let pub_decrypt (secret, data) k =
  let sym_key = last_of sym_key_size (RSA.decrypt k secret) in
  sym_decrypt data sym_key

let string_of_pubkey key =
  let { RSA.size = size; RSA.n = n; RSA.e = e } = key in
  let size = string_of_int size in
  let sexpr =
    List [ String "Pub"; String size; String n; String e ] in
  string_of_sexpr sexpr

let pubkey_of_string string =
  try
    match sexpr_of_string string with
      List [ String "Pub"; String size; String n; String e ] ->
	{ RSA.size = int_of_string size; RSA.n = n; RSA.e = e;
	  RSA.d = ""; RSA.p = ""; RSA.q = "";
	  RSA.dp = ""; RSA.dq = ""; RSA.qinv = "";
	}
    |  _ ->
	failwith "pubkey_of_string"
  with _ ->
    failwith "pubkey_of_string"

let string_of_keypair key =
  let { RSA.size = size; RSA.n = n; RSA.e = e;
	RSA.d = d; RSA.p = p; RSA.q = q;
	RSA.dp = dp; RSA.dq = dq; RSA.qinv = qinv } =
    key in
  let sexpr =
    List [ String "key pair"; String (string_of_int size);
	   String n; String e;
	   String d; String p; String q;
	   String dp; String dq; String qinv ] in
  string_of_sexpr sexpr

let keypair_of_string string =
  try
    match sexpr_of_string string with
     List [ String "key pair"; String size;
	     String n; String e;
	   String d; String p; String q;
	   String dp; String dq; String qinv ] ->
	       { RSA.size = int_of_string size;
		 RSA.n = n; RSA.e = e;
		 RSA.d = d; RSA.p = p; RSA.q = q;
		 RSA.dp = dp; RSA.dq = dq; RSA.qinv = qinv }
    | _ ->
	failwith "keypair_of_string"
  with _ ->
    failwith "keypair_of_string"

let string_of_symkey key =
  let sexpr = List [ String "Sym"; String key ] in
  string_of_sexpr sexpr

let symkey_of_string string =
  try
    let sexpr = sexpr_of_string string in
    match sexpr with
      List [ String "Sym"; String s ] -> s
    | _ ->
	failwith "symkey_of_string"
  with _ ->
    failwith "symkey_of_string"

let string_of_nonce n = n

let nonce_of_string s = s
