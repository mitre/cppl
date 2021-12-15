(*pp ../../comp/cpplc -d *)

(* Initiator *)

auth_init (b:text) (na, nb)
  call _ --> maybe_get_public_key (b) (kb) owns(b, kb)
  call at(b, bs) --> _
  let chan = connect(bs) in
  let na = new nonce in
  send _ --> chan na
  receive chan [na, nb:nonce, b] kb --> _
  return
end

maybe_get_public_key (b:text) (kb:pubkey)
    proves  owns(b, kb)
  call cases
    owns(b, kb) --> _
    return
  | ca(c:text) and owns(c, kc:pubkey)
      --> get_public_key (b, c, kc) (kb)
      owns(b, kb)
    return
  end
end

get_public_key (b:text, c:text, kc) (kb:pubkey)
    assumes owns(c, kc)
    proves  owns(b, kb)
  call at(c, cs) --> _
  let chan = connect(cs) in
  let na = new nonce in
  send _ --> chan na, b
  receive chan [na, c, b, kb] kc
    --> says_owns(c, b, kb)
  return
end

(* Responder *)

auth_resp () (na, nb)
  let chan = accept in
  let nb = new nonce in
  let m = receive (chan) in
  match m na:nonce --> _
  send self(b:text) and owns(b, kb)
    --> chan [na, nb, b] kb
  return
end

(* Certificate Authority *)

auth_ca () (n:nonce)
    proves  request_filled(n)
  call self(c:text) and owns(c, k:pubkey)
    --> auth_ca_service(c, k) (n)
    request_filled(n)
  return
end

auth_ca_service (c:text, kc) (n)
    assumes self(c) and owns(c, kc)
    proves  request_filled(n)
  let chan = accept in
  receive chan n:nonce, b:text --> request(b, n)
  send owns(b, kb:pubkey)
    --> chan [n, c, b, kb] kc    % Send back kb
  return
end
