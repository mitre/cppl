(*pp ../../comp/cpplc -a *)

request_filled(N) :-
  request(P, N),
  owns(P, K).
