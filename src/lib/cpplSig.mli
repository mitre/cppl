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

(** Cryptography *)

module type Crypto =
  sig
    (** Cryptography *)

    (** Keys *)

    (** Asymmetric keys *)

    type pubkey

    type keypair

	  (** Symmetric keys *)

    type symkey

	  (** An opaque object that can only be checked for equality. *)
    type nonce

    val pubkey_of : keypair -> pubkey

    val pubkey_equals : pubkey -> pubkey -> bool

    val symkey_equals : symkey -> symkey -> bool

    val nonce_equals : nonce -> nonce -> bool

    val hash : string -> string
	(** Create a hash of a string. *)

    val pub_sign : string -> keypair -> string
	(** [pub_sign c k] creates a signature for content c.  *)

    val pub_verify : string -> string -> pubkey -> bool
	(** [pub_verify s c k] verifies signature s for content c. *)

    val pub_encrypt : string -> pubkey -> string * string
	(** [pub_encrypt c k] creates an encrypted symmetric key, and
	   uses the key to bulk encrypt content c.  It returns the
	   encrypted key and content in that order. *)

    val pub_decrypt : string * string -> keypair -> string
	(** [pub_decrypt (s, c) k] decrypts the semmetric key s and
	   uses it to decrypt the content c.  The content is returned.
	   *)

    val sym_encrypt : string -> symkey -> string
	(** [sym_encrypt c k] encrypts the content c. *)

    val sym_decrypt : string -> symkey -> string
	(** [sym_decrypt c k] decrypts the content c. *)

    val new_keypair : unit -> keypair
    val new_symkey : unit -> symkey
    val new_nonce : unit -> nonce
	(** Creators of new secrets. *)

    val string_of_pubkey : pubkey -> string
	(** Return the string representation of a public key. *)

    val pubkey_of_string : string -> pubkey
	(** Convert the given string to a key. Raise [Failure
	   "pubkey_of_string"] if the given string is not a valid
	   representation of a key. *)

    val string_of_keypair : keypair -> string
	(** Return the string representation of a public key. *)

    val keypair_of_string : string -> keypair
	(** Convert the given string to a key. Raise [Failure
	   "keypair_of_string"] if the given string is not a valid
	   representation of a key. *)

    val string_of_symkey : symkey -> string
	(** Return the string representation of a symmetric key. *)

    val symkey_of_string : string -> symkey
	(** Convert the given string to a key. Raise [Failure
	   "symkey_of_string"] if the given string is not a valid
	   representation of a key. *)

    val string_of_nonce : nonce -> string

    val nonce_of_string : string -> nonce

  end
