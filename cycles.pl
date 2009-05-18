:- module(cycles, [cycles/2, cycles/3, cycles/4]).

:- use_module(library(lists)).
:- use_module(util).

cycles(G, NumCycles) :-
	quadruples_to_packedevents(G, Seq),
	seq_ncycles([], Seq, 0, NumCycles).

cycles(G, Time, NumCycles) :-
	quadruples_to_packedevents(G, Seq),
	split_packedevents(Seq, Time, Events, _),
	packedevents_to_edges(Events, Edges),
	ncycles(Edges, NumCycles).

% urci _nejvetsi_ pocet cyklu v intervalu Start, End
cycles(G, Start, End, NumCycles) :-
	quadruples_to_packedevents(G, Seq),
	split_packedevents(Seq, Start, Pre, Post),
	split_packedevents(Post, End, Events, _),
	packedevents_to_edges(Pre, InitEdges),
	ncycles(InitEdges, InitNumCycles),
	%nl, print('graph: '), print(InitEdges), print(' cycles: '), print(InitNumCycles), 
	seq_ncycles(InitEdges, Events, InitNumCycles, NumCycles).

seq_ncycles(_Edges, [], Cycles, Cycles).
seq_ncycles(Edges, [Event|Tail], Acc, Cycles) :-
	apply_packedevent_to_edges(Edges, Event, NewEdges),
	ncycles(NewEdges, CurCycles),
	%nl, print('graph: '), print(NewEdges), print(' cycles: '), print(CurCycles), 
	max(Acc, CurCycles, NewMax),
	seq_ncycles(NewEdges, Tail, NewMax, Cycles).

ncycles(Graph, Cycles) :-
	vertices(Graph, [], Vertices),
	ncycles(Vertices, [], Graph, 0, NCycles),
	% kazdy cyklus je zapocitan dvakrat (obema smery)
	% jak se toho zbavit?
	Cycles is NCycles // 2.

ncycles([], _Banned, _Graph, Cycles, Cycles).
ncycles([V|Rest], Banned, Graph, Acc, Cycles) :-
	cycles_from_vertex([V], Graph, Banned, SubCycles),
	Acc1 is Acc + SubCycles,
	ncycles(Rest, [V|Banned], Graph, Acc1, Cycles).


% nasli jsme cyklus
cycles_from_vertex([CurVert | Stack], _Graph, _Banned, Cycles) :-
	last(_, CurVert, Stack),
	length(Stack, SLen),
	SLen > 2, %cykly typu a-b-a nas nezajimaji
	!,
	%nl, print('cycle: '), print([CurVert | Stack]),
	Cycles = 1.

% jsme ve vrcholu ve kterem uz jsme byli
cycles_from_vertex([CurVert | Stack], _Graph, _Banned, Cycles) :-
	memberchk(CurVert, Stack),
	!,
	Cycles = 0.

% nejsme v cyklu - rekurze, secist
cycles_from_vertex([CurVert | Stack], Graph, Banned, Cycles) :-
	edges_neighbors(Graph, CurVert, Neighbors),
	multidelete(Neighbors, Banned, Neighs1),
	walk_neighbors(Neighs1, [CurVert | Stack], Graph, Banned, 0, Cycles).

walk_neighbors([], _, _, _, Cycles, Cycles).
walk_neighbors([Neigh|Rest], Stack, Graph, Banned, AccCycles, Cycles) :-
	cycles_from_vertex([Neigh|Stack], Graph, Banned, NeighCycles),
	Acc1 is AccCycles + NeighCycles,
	walk_neighbors(Rest, Stack, Graph, Banned, Acc1, Cycles).
