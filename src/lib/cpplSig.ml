(* Cryptography signature.
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

module type Crypto =
  sig
    type pubkey
    type keypair
    type symkey
    type nonce
    val pubkey_of : keypair -> pubkey
    val pubkey_equals : pubkey -> pubkey -> bool
    val symkey_equals : symkey -> symkey -> bool
    val nonce_equals : nonce -> nonce -> bool
    val hash : string -> string
    val pub_sign : string -> keypair -> string
    val pub_verify : string -> string -> pubkey -> bool
    val pub_encrypt : string -> pubkey -> string * string
    val pub_decrypt : string * string -> keypair -> string
    val sym_encrypt : string -> symkey -> string
    val sym_decrypt : string -> symkey -> string
    val new_keypair : unit -> keypair
    val new_symkey : unit -> symkey
    val new_nonce : unit -> nonce
    val string_of_pubkey : pubkey -> string
    val pubkey_of_string : string -> pubkey
    val string_of_keypair : keypair -> string
    val keypair_of_string : string -> keypair
    val string_of_symkey : symkey -> string
    val symkey_of_string : string -> symkey
    val string_of_nonce : nonce -> string
    val nonce_of_string : string -> nonce
  end
