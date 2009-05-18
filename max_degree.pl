:- module(max_degree, [max_degree/2, max_degree/3, max_degree/4]).

:- use_module(library(lists)).
:- use_module(util).

%casovy okamzik
max_degree(G, Time, V) :-
	quadruples_to_events(G, Seq),
	split_events(Seq, Time, Events, _),
	events_to_edges(Events, InitEdges),
	max_degree_seq(InitEdges, [], V).

%interval
max_degree(G, Start, End, V) :-
	quadruples_to_events(G, AllSeq),
	split_events(AllSeq, Start, PreStart, PostStart),
	split_events(PostStart, End, Seq, _),
	events_to_edges(PreStart, InitEdges),
	max_degree_seq(InitEdges, Seq, V).

%celkove
max_degree(G, V) :-
	quadruples_to_events(G, Seq),
	max_degree_seq([], Seq, V).

% max_degree_seq(+InitEdges, +Seq, -MaxVertice)
% InitEdges - hrany ktere uz v grafu jsou
% Seq - sekvence pridani/odebrani hran
% MaxVertice - vrchol s maximalnim stupnem
max_degree_seq(InitEdges, Seq, MaxVertice) :-
	init_maxdegree_table(InitEdges, Seq, Table),
	table_maximum(Table, InitVertice-InitDegree),
	scan_sequence(Seq, Table, InitVertice-InitDegree, MaxVertice-_MaxDegree).

table_maximum([V-Num], V-Num) :- !.
table_maximum([V-Num|T], V-Num) :-
	table_maximum(T, _-Num1),
	Num >= Num1,
	!.
table_maximum([_|T], V1-Num1) :-
	table_maximum(T, V1-Num1).

scan_sequence([], _, V-D, V-D).
scan_sequence([del(X-Y,_)|T], Table, V-D, MV-MD) :-
	table_lookup(Table, X, XDeg),
	NewXDeg is XDeg - 1,
	table_update(Table, X, NewXDeg, Table1),
	table_lookup(Table1, Y, YDeg),
	NewYDeg is YDeg - 1,
	table_update(Table1, Y, NewYDeg, Table2),
	% maximalni se odebranim hrany nezmeni
	scan_sequence(T, Table2, V-D, MV-MD).

scan_sequence([add(X-Y,_)|T], Table, V-D, MV-MD) :-
	table_lookup(Table, X, XDeg),
	NewXDeg is XDeg + 1, % nahradit za if nebo ->;
	(
		NewXDeg > D,
		V1 = X,
		D1 = NewXDeg,
		!
	;
		V1 = V,
		D1 = D
	),
	table_update(Table, X, NewXDeg, Table1),
	table_lookup(Table1, Y, YDeg),
	NewYDeg is YDeg + 1,
	(
		NewYDeg > D1,
		V2 = Y,
		D2 = NewYDeg,
		!
	;
		V2 = V1,
		D2 = D1
	),
	table_update(Table1, Y, NewYDeg, Table2),
	scan_sequence(T, Table2, V2-D2, MV-MD).

% Vezme seznam hran pocatecniho grafu ve tvaru [x-y] a sekvenci
% pridani/odebrani [add(...),del(...),...] a vrati tabulku
% [vrchol-nejvetsistupen] se vsemi vrcholy, kde stupne jsou vypocitany
% jenom nad pocatecnim grafem
init_maxdegree_table(InitEdges, Seq, Table) :-
	vertices(InitEdges, Seq, Vertices),
	count_degrees(Vertices, InitEdges, Table).
