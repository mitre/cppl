(*pp ../../comp/cpplc -a *)

low_quality_allowed(P, "resource") :-
  owns(P, K).

approx_val("resource", "nearly").

supplied(A, "high quality", D, V) :-
  high_quality_allowed(A, D),
  curr_val(D, V).

supplied(A, "low quality", D, V) :-
  low_quality_allowed(A, D),
  approx_val(D, V).
