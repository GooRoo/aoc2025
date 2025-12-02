:- use_module(library(clpfd)).
:- initialization(main).

read_input(Filename, Ranges) :-
	open(Filename, read, Stream),
	read_ranges(Stream, Ranges),
	close(Stream).

read_ranges(Stream, Ranges) :-
	read_line_to_string(Stream, Line),
	split_string(Line, ",", "", Parts),
	maplist(parse_range, Parts, Ranges).

parse_range(Str, Start-End) :-
	split_string(Str, "-", "", [StartStr, EndStr]),
	number_string(Start, StartStr),
	number_string(End, EndStr).

invalid_ids(From-To, Xs) :-
	findall(X,
			invalid_id(From, To, X),
		Xs).

invalid_id(From, To, X) :-
	between(From, To, X),
	number_string(X, XStr),
	is_repeating2(XStr).

is_repeating1(Str) :-
	string_length(Str, Length),
	0 is Length mod 2,
	Half is Length//2,
	sub_string(Str, 0, Half, _, FirstHalf),
	sub_string(Str, Half, Half, 0, SecondHalf),
	FirstHalf == SecondHalf.

is_repeating2(Str) :-
	string_length(Str, Len),
	between(1, Len, PrefixLen),
	PrefixLen < Len,
	0 is Len mod PrefixLen,
	sub_string(Str, 0, PrefixLen, _, Prefix),
	RepeatCount is Len//PrefixLen,
	repeat_string(Prefix, RepeatCount, Str),
	!.

repeat_string(S, 1, S) :-
	!.
repeat_string(S, N, R) :-
	N > 1,
	N1 is N - 1,
	repeat_string(S, N1, Rest),
	string_concat(S, Rest, R).

solve_ranges(Ranges, Result) :-
	maplist(invalid_ids, Ranges, AllXs),
	append(AllXs, FlatXs),
	sum_list(FlatXs, Result).

solve :-
	read_input("day02/data/task.example", Ranges),
	solve_ranges(Ranges, Result),
	format("~w~n", [Result]).

main :-
	solve.
