(*pp ../../comp/cpplc -a *)

owns(B, KB) :-
  says_owns(C, B, KB),
  owns (C, KC).
