:- use_module(library(dcg/basics)).
:- initialization(parse, main).

parse :-
    ht_new(HashTable),
    phrase_from_file(lines(HashTable), "measurements.txt"),
    findall(_,( ht_gen(HashTable, Key, (Min,Max,Sum,Count)),
	        Mean is Sum / Count,
		string_codes(Name, Key),
	        format('~w = ~1f, ~1f, ~1f~n', [Name,Min,Max,Mean])), _).

line(H)  --> string(K), ";", number(V), { update_hash(H,K,V) }.
lines(_) --> [].
lines(H) --> line(H), "\n", !, lines(H).

update_hash(H, K, V) :-
    (   ht_update(H, K, (Min0, Max0, Sum0, Count0), New)
    ->  ( Min is min(V, Min0), Max is max(V, Max0),
	  Sum is V + Sum0, Count is Count0 + 1,
	  New = (Min, Max, Sum, Count))
    ;   ht_put(H, K, (V,V,V,1))).
