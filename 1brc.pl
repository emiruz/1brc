:- initialization(parse, main).
:- dynamic rec/5.

parse :-
    phrase_from_file(lines, "measurements.txt"),
    findall(_,( rec(Key, Min, Max, Sum, Count),
	        Mean is Sum / Count,
	        format('~w = ~1f, ~1f, ~1f~n', [Key,Min,Max,Mean])), _).

lines([],[]).
lines(In,Out) :-
    append(Ks, [0';|Rest],In),!,
    append(Vs, [0'\n|Out0], Rest),!,
    number_codes(V,Vs),
    atom_codes(A,Ks),
    update_rec(A,V),
    lines(Out0, Out).

update_rec(A,V) :-
  (   retract(rec(A, Min0, Max0, Sum0, Count0))
  ->  Min is min(V, Min0), Max is max(V, Max0),
      Sum is V + Sum0, Count is Count0 + 1,
      assert(rec(A, Min, Max, Sum, Count))
  ;   assert(rec(A, V, V, V, 1))).
