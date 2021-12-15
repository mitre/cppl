(*pp ../../comp/cpplc -a *)

owns(B, KB) :-
  says_owns(C, B, KB),
  owns (C, KC).

val(N, D, "high quality", V) :-
  says_curr_val(B, N, D, V).

val(N, D, "low quality", V) :-
  says_approx_val(B, N, D, V).
