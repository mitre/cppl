(*pp ../../comp/cpplc -d *)

(* Initiator *)

init(a:text, b:text, d:text) (n, q, v)
    proves  val(n, d, q:text, v)
  call owns(a, ka) --> _
  call _ --> maybe_get_public_key(b) (kb) owns(b, kb)
  call at(b, bs) --> _
  let chan = connect (bs) in
  let n = new nonce in
  send _ --> chan {n, a, d} kb
  receive chan {n, k, b} ka --> _
  send _ --> chan {k} kb
  receive chan cases
    {|High, v:text|} k --> says_curr_val(b, n, d, v)
    return
  | {|Low, v:text|} k --> says_approx_val(b, n, d, v)
    return
  end
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

resp(b:text) (a, q, d, v)
    proves  supplied(a, q:text, d, v)
  call owns(b, kb) --> _
  let chan = accept in
  receive chan {n:nonce, a:text, d:text} kb --> _
  let k = new symkey in
  send owns(a, ka) --> chan {n, k, b} ka
  receive chan {k} kb --> says_requests(a, d, n)
  send cases
    high_quality_allowed(a, d)
      and curr_val(d, v:text)
      --> chan {|High, v|} k
    return
  | low_quality_allowed(a, d)
      and approx_val(d, v:text)
      --> chan {|Low, v|} k
    return
  end
end

(* Certificate Authority *)

ca (c:text) (n:nonce)
  call owns(c, kc) --> _
  let chan = accept in
  receive chan n:nonce, b:text --> _
  send owns(b, kb:pubkey)
    --> chan [n, c, b, kb] kc    % Send back kb
  return
end
