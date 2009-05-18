:- module(util, [
	% testovaci vstupy
	test_input/1,
	test_input2/1,
	edges_test/1,
	domecek/1,

	% manipulace s 'events' (seznam termu add(Time, Hrana), del(Time, Hrana)
	quadruples_to_events/2,
	split_events/4,
	events_to_edges/2,

	% tabulky
	table_lookup/3,
	table_update/4,

	% manipulace se seznamy hran
	vertices/3,
	edges_neighbors/3, % neighbors kolidovalo s nejakou std funkci
	connected/1,
	edges_to_ugraph/2,
	ugraph_to_edges/2,
	count_degrees/3,
	components/2,

	% operace nad seznamy
	filter/3,
	partition/4,
	multidelete/3,

	% packedevents
	quadruples_to_packedevents/2,
	split_packedevents/4,
	packedevents_to_edges/2,
	apply_packedevent_to_edges/3,

	% ruzne
	even/1,
	max/3
]).

:- use_module(library(lists)).   % ruzne funkce pro seznamy
:- use_module(library(samsort)). % razeni podle dodaneho predikatu
:- use_module(library(ugraphs)). % knihovna pro praci s grafy

test_input(G) :-
	G = [
		[a-b, 0, 2],
		[a-e, 1, 7],
		[b-c, 0, 2],
		[c-d, 2, 3],
		[c-e, 1, 3],
		[b-d, 0, 4],
		[d-e, 1, 4],
		[b-e, 1, 2],
		[a-x, 3, 6],
		[x-y, 6, 7],
		[y-z, 6, 8],
		[z-x, 7, 8]

		%,[d-u, 1, 3] %odkomentovanim techto radek bude graf v okamziku 2 eulerovsky
		%,[c-u, 1, 3]

		%,[v-x, 4, 4]
		%,[v-b, 4, 4]
	].

test_input2(G) :-
	G = [
		[a-b, 1, 15],
		[b-c, 1, 5],
		[a-c, 1, 10],
		[c-d, 5, 15],
		[b-d, 5, 10],
		[a-d, 10, 15],
		[b-c, 10, 15]
	].

edges_test(G) :-
	G = [
		a-b,
		b-e,
		e-d,
		d-a,
		a-c,
		b-c,
		d-c,
		e-c
	].

domecek(G) :-
	G = [
		a-b,
		b-e,
		e-d,
		d-a,
		a-e,
		b-d,
		a-c,
		c-b
	].

% Prevede (serazeny) seznam udalosti na seznam hran, ktery vznikne
% po provedeni techto udalosti.
% e.g. events_to_edges([add(a-b,1),add(b-c,2),del(a-b,3)],[b-c]).
events_to_edges(Events, Result) :- events_to_edges(Events, [], Result).
events_to_edges([], G, G).
events_to_edges([add(X-Y, _) | Tail], CurGraph, Result) :-
	events_to_edges(Tail, [X-Y|CurGraph], Result).
events_to_edges([del(X-Y, _) | Tail], CurGraph, Result) :-
	delete(CurGraph, X-Y, NewGraph),
	events_to_edges(Tail, NewGraph, Result).

% Vezme seznam (serazenych) udalosti a rozdeli je podle hodnoty Time
% na seznam udalosti co se staly pred Time a co se staly po Time.
% Pokud se nejaka udalost stane v Time a je to pridani hrany, je
% to jako by se stala predtim, pokud je to del, je to jako potom.
split_events([], _, [], []).
split_events([add(X-Y,Time) | T], Time, [add(X-Y,Time) | NPre], NPost) :-
	!,
	split_events(T, Time, NPre, NPost).
split_events([del(X-Y,Time) | T], Time, NPre, [del(X-Y,Time)|NPost]) :-
	!,
	split_events(T, Time, NPre, NPost).
split_events([Ev|T], Time, [], [Ev,T]) :-
	arg(2, Ev, EvTime),
	EvTime > Time,
	!.
split_events([H|T], Time, [H|NPre], NPost) :-
	split_events(T, Time, NPre, NPost).

% Prevede seznam ctveric (tak jak je popsany v predbezne zprave) na
% serazeny seznam udalosti add(Vrchol-Vrchol, Cas), del(Vrchol-Vrchol, Cas).
quadruples_to_events(G, Seq) :-
	to_unsorted_events(G, Events),
	samsort(before, Events, Seq).

to_unsorted_events([], []).
to_unsorted_events([[A-B, Start, End] | Tail], [add(A-B, Start), del(A-B, End) | NTail]) :-
	to_unsorted_events(Tail,NTail).

shift_deletions([], []).
shift_deletions([add(E, T)|Tail], [add(E,T)|NTail]) :- shift_deletions(Tail, NTail).
shift_deletions([del(E, T)|Tail], [del(E,T1)|NTail]) :-
	T1 is T + 1,
	shift_deletions(Tail, NTail).

% porovnani druheho argumentu obou termu.
before(T1, T2) :-
	arg(2, T1, Val1),
	arg(2, T2, Val2),
	Val1 < Val2.

% table_lookup(+Table, +Key, -Value)
% V tabulce dvojic Table vyhleda hodnotu klice Key.
table_lookup([K-V|_], K, V) :- !.
table_lookup([_|T], K, V) :- table_lookup(T,K,V).

% table_update(+Table, +Key, +NewValue, -NewTable).
% Nahradi hodnotu klice Key hodnotou NewValue a vrati takto zmenenou tabulku.
table_update([K-_ | T], K, NV, [K-NV | T]) :- !.
table_update([H | T], K, NV, [H | NT]) :-
	table_update(T, K, NV, NT).

% vertices(+InitEdges, +Seq, -VerticeList)
% Pro zadany seznam hran a posloupnost udalosti (jedno z toho muze byt prazdne)
% vrati seznam unikatnich vrcholu.
vertices(InitEdges, Seq, VerticeList) :-
	seq_strip_times(Seq, SeqEdges),
	append(InitEdges, SeqEdges, UnsortedEdges),
	break_edges(UnsortedEdges, UnsortedVertices),
	remove_dups(UnsortedVertices, VerticeList).

break_edges([], []).
break_edges([A-B | T], [A, B | NT]) :- break_edges(T,NT).

seq_strip_times([],[]).
seq_strip_times([add(A-B, _) | T], [A-B | NT]) :- seq_strip_times(T, NT).
seq_strip_times([del(A-B, _) | T], [A-B | NT]) :- seq_strip_times(T, NT).

%%% po nahrani na web %%%

% prevede seznam hran na ugraph tak jak je popsany v library(ugraphs)
edges_to_ugraph(Edges, Ugraph) :-
	vertices_edges_to_ugraph([], Edges, Dgraph),
	symmetric_closure(Dgraph, Ugraph).

% prevede ugraph na seznam hran
ugraph_to_edges(Ugraph, Edges) :-
	edges(Ugraph, DoubleEdges),
	desymmetrify(DoubleEdges, Edges).

desymmetrify([], []).
desymmetrify([X-Y|Tail], [X-Y|NTail]) :-
	delete(Tail, Y-X, Tail1),
	desymmetrify(Tail1, NTail).

%symmetrify(Edges, SymmEdges) :-
%	symm(Edges, E),
%	remove_dups(E, SymmEdges).
%
%symm([],[]).
%symm([X-Y | T], [X-Y, Y-X | NT]) :-
%	symm(T, NT).

% count_degrees(+Vertices, +Edges, -DegreeTable)
% Pro dany seznam vrcholu a seznam hran spocita stupne vrcholu
% do tabulky [vrchol-stupen].
count_degrees([], _, []).
count_degrees([V|T], Edges, [V-Num | NT]) :- 
	count_degree(V, Edges, Num),
	count_degrees(T, Edges, NT).

count_degree(_, [], 0).
count_degree(V, [X-Y | T], N) :-
	V \= X,
	V \= Y,
	!,
	count_degree(V, T, N).
count_degree(V, [_ | T], N) :-
	count_degree(V, T, N1),
	N is N1 + 1.

even(Number) :- 0 is Number mod 2.

% connected(+Edges).
% Uspeje, pokud mnozina hran tvori souvisly graf.
connected([]).
connected(Edges) :-
	Edges = [V-_ | _],
	edges_to_ugraph(Edges, UGraph),
	reachable(V, UGraph, Reachable),
	vertices(Edges, [], Vertices),
	sort(Reachable, SortedReachable),
	sort(Vertices, SortedVertices),
	SortedReachable = SortedVertices.

% filter a partition - delaji to same co v haskellu
%filter(_, [], []).
%filter(Pred, [Head | Tail], [Head | Filtered]) :-
%	call(Pred, Head),
%	!,
%	filter(Pred, Tail, Filtered).
%filter(Pred, [_ | Tail], Filtered) :-
%	filter(Pred, Tail, Filtered).
filter(P, L, R) :- include(P, L, R).

partition(_, [], [], []).
partition(Pred, [H|T], [H|A], B) :-
	call(Pred, H),
	!,
	partition(Pred, T, A, B).
partition(Pred, [H|T], A, [H|B]) :-
	partition(Pred, T, A, B).

% multidelete(List, KillList, Residue)
% ze seznamu List odstrani vsechny prvky ktere jsou v seznamu KillList
multidelete(List, [], List).
multidelete(List, [Kill|Rest], Residue) :-
	delete(List, Kill, Killed),
	multidelete(Killed, Rest, Residue).

%%% "packed_events"
%%% Dalsi datovy format - tentokrat seznam udalosti tvaru
%%% ev(Time, AddEdges, DelEdges), Time je ces, AddEdges seznam
%%% hran ktere byly pridany, DelEdges seznam hran ktere byly odebrany

quadruples_to_packedevents(G, PackedSeq) :-
	to_unsorted_events(G, Seq),
	shift_deletions(Seq, ShiftedSeq),
	pack(ShiftedSeq, UnsortedPackedSeq),
	samsort(packed_before, UnsortedPackedSeq, PackedSeq).
	%nl, print('packed sequence: '), print(PackedSeq).

packed_before(ev(T1, _, _), ev(T2, _, _)) :- T1 < T2.

pack([], []).
pack([Head | Tail], Result) :-
	arg(2, Head, Time),
	pack_edges(Time, [Head|Tail], [], [], [], AddEdges, DelEdges, Rest),
	pack(Rest, NResult),
	Result = [ ev(Time, AddEdges, DelEdges) | NResult ].

pack_edges(_, [], AddEdges, DelEdges, Rest, AddEdges, DelEdges, Rest).
pack_edges(Time, [Head | Tail], AccAddEdges, AccDelEdges, AccRest, AddEdges, DelEdges, Rest) :-
	arg(2, Head, Time),
	!,
	(
		Head = add(Edge, Time),
		AccAdd1 = [Edge|AccAddEdges],
		AccDel1 = AccDelEdges
	;
		Head = del(Edge, Time),
		AccAdd1 = AccAddEdges,
		AccDel1 = [Edge|AccDelEdges]
	),
	pack_edges(Time, Tail, AccAdd1, AccDel1, AccRest, AddEdges, DelEdges, Rest).
pack_edges(Time, [Head | Tail], AccAddEdges, AccDelEdges, AccRest, AddEdges, DelEdges, Rest) :-
	AccRest1 = [Head | AccRest],
	pack_edges(Time, Tail, AccAddEdges, AccDelEdges, AccRest1, AddEdges, DelEdges, Rest).

% Dela to same co split_events, ale nad 'packedevents'.
split_packedevents([], _, [], []).
%split_packedevents([ev(Time, AddEdges, DelEdges) | T], Time, [ev(Time,AddEdges,[])|NPre], [ev(Time,[],DelEdges)|NPost]) :-
%	!,
%	split_packedevents(T, Time, NPre, NPost).
split_packedevents([Ev|T], Time, [], [Ev|T]) :-
	arg(1, Ev, EvTime),
	EvTime > Time,
	!.
split_packedevents([H|T], Time, [H|NPre], NPost) :-
	split_packedevents(T, Time, NPre, NPost).

% apply_packedevent_to_edges(+Graph, +PackedEvent, -NewGraph).
% Aplikuje udalost PackedEvent na seznam hran Graph.
apply_packedevent_to_edges(Edges, ev(_Time, AddEdges, DelEdges), NewEdges) :-
	multidelete(Edges, DelEdges, NewGraph1),
	append(NewGraph1, AddEdges, NewEdges).
	%nl, nl, print('time: '), print(Time), print(' new graph: '), print(NewEdges).

% Dela to same co events_to_edges, ale nad 'packedevents'.
packedevents_to_edges(Events, Result) :- packedevents_to_edges(Events, [], Result).
packedevents_to_edges([], G, G).
packedevents_to_edges([Event | Tail], CurGraph, Result) :-
	apply_packedevent_to_edges(CurGraph, Event, NewGraph),
	packedevents_to_edges(Tail, NewGraph, Result).

max(A, B, A) :- A >= B, !.
max(A, B, B) :- B > A.

% Pro dany seznam hran a dany vrchol vrati seznam jeho sousedu.
edges_neighbors(Edges, Vertice, Neighbors) :-
	edges_neighbors(Edges, Vertice, [], UnsortedNeighbors),
	remove_dups(UnsortedNeighbors, Neighbors).

edges_neighbors([], _Vertice, Neighbors, Neighbors).
edges_neighbors([V-N | Tail], V, AccNeighbors, Neighbors) :-
	!,
	Acc1 = [N | AccNeighbors],
	edges_neighbors(Tail, V, Acc1, Neighbors).
edges_neighbors([N-V | Tail], V, AccNeighbors, Neighbors) :-
	!,
	Acc1 = [N | AccNeighbors],
	edges_neighbors(Tail, V, Acc1, Neighbors).
edges_neighbors([_E | Tail], V, AccNeighbors, Neighbors) :-
	edges_neighbors(Tail, V, AccNeighbors, Neighbors).

% components(+Edges, -Components)
% Rozlozi seznam hran Edges na seznam seznamu hran Components,
% kteryzto reprezentuje jednotlive souvisle komponenty grafu.
components([], []) :- !.
components([E], [[E]]) :- !.
components(Edges, Components) :-
	length(Edges, NumEdges),
	Half is NumEdges // 2,
	append_length(First, Second, Edges, Half),
	components(First, Compo1),
	components(Second, Compo2),
	merge_components(Compo1, Compo2, Components).

merge_components([], C, C) :- !.
merge_components([H|T], C2s, Result) :-
	merge_component(H, C2s, NewC2),
	merge_components(T, NewC2, Result).

merge_component(C1, [], [C1]) :- !.
merge_component(C1, [C2|T], Result) :-
	vertices(C1, [], C1Verts),
	vertices(C2, [], C2Verts),
	multidelete(C1Verts, C2Verts, Difference),
	(
		C1Verts == Difference,
		!,
		merge_component(C1, T, Res1),
		Result = [C2|Res1]
	;
		%komponenty maji spolecny vrchol -> spojime
		append(C1, C2, NewC),
		merge_component(NewC, T, Result)
	).
