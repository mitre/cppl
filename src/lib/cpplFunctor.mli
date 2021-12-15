(* A library linker.
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

(** Library Linker *)

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

module Make (C : CpplSig.Crypto) :
    T with type pubkey = C.pubkey
    and type symkey = C.symkey
    and type nonce = C.nonce
(** Functor building an implementation of the CPPL library. *)
