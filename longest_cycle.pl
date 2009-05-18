:- module(longest_cycle, [longest_cycle/2, longest_cycle/3, longest_cycle/4]).

:- use_module(util).
:- use_module(library(lists)).

longest_cycle(G, MaxCycle) :-
	quadruples_to_packedevents(G, Seq),
	seq_maxcycle([], Seq, [], MaxCycleVertices),
	cycle_vertices_to_edges(MaxCycleVertices, MaxCycle).

longest_cycle(G, Time, MaxCycle) :-
	quadruples_to_packedevents(G, Seq),
	split_packedevents(Seq, Time, Events, _),
	packedevents_to_edges(Events, Edges),
	maxcycle(Edges, MaxCycleVertices),
	cycle_vertices_to_edges(MaxCycleVertices, MaxCycle).

%% urci nejdelsi cyklus v intervalu Start, End
longest_cycle(G, Start, End, MaxCycle) :-
	quadruples_to_packedevents(G, Seq),
	split_packedevents(Seq, Start, Pre, Post),
	split_packedevents(Post, End, Events, _),
	packedevents_to_edges(Pre, InitEdges),
	maxcycle(InitEdges, InitCycle),
	seq_maxcycle(InitEdges, Events, InitCycle, MaxCycleVertices),
	cycle_vertices_to_edges(MaxCycleVertices, MaxCycle).

seq_maxcycle(_Edges, [], MaxCycle, MaxCycle).
seq_maxcycle(Edges, [Event|Tail], Acc, MaxCycle) :-
	apply_packedevent_to_edges(Edges, Event, NewEdges),
	maxcycle(NewEdges, CurCycle),
	length(Acc, AccLen),
	length(CurCycle, CurLen),
	if(CurLen > AccLen, Acc1 = CurCycle, Acc1 = Acc),
	seq_maxcycle(NewEdges, Tail, Acc1, MaxCycle).

maxcycle(Graph, MaxCycle) :-
	vertices(Graph, [], Vertices),
	maxcycle(Vertices, [], Graph, [], MaxCycle).

maxcycle([], _Banned, _Graph, MaxCycle, MaxCycle).
maxcycle([V|Rest], Banned, Graph, Acc, MaxCycle) :-
	maxcycle_from_vertex([V], Graph, Banned, LocalCycle),
	length(Acc, AccLength),
	length(LocalCycle, LocalLength),
	if(LocalLength > AccLength, NewCycle = LocalCycle, NewCycle = Acc),
	maxcycle(Rest, [V|Banned], Graph, NewCycle, MaxCycle).

% nasli jsme cyklus
maxcycle_from_vertex([CurVert | Stack], _Graph, _Banned, Cycle) :-
	last(_, CurVert, Stack),
	length(Stack, SLen),
	SLen > 2, %cykly typu a-b-a nas nezajimaji
	!,
	Cycle = [CurVert | Stack].

% jsme ve vrcholu ve kterem uz jsme byli
maxcycle_from_vertex([CurVert | Stack], _Graph, _Banned, Cycle) :-
	memberchk(CurVert, Stack),
	!,
	Cycle = [].

% nejsme v cyklu - rekurze, secist
maxcycle_from_vertex([CurVert | Stack], Graph, Banned, MaxCycle) :-
	edges_neighbors(Graph, CurVert, Neighbors),
	multidelete(Neighbors, Banned, Neighs1),
	walk_neighbors(Neighs1, [CurVert | Stack], Graph, Banned, [], MaxCycle).

walk_neighbors([], _, _, _, MaxCycle, MaxCycle).
walk_neighbors([Neigh|Rest], Stack, Graph, Banned, AccCycle, MaxCycle) :-
	maxcycle_from_vertex([Neigh|Stack], Graph, Banned, NeighCycle),
	length(AccCycle, AccLength),
	length(NeighCycle, NeighLength),
	if(NeighLength > AccLength, Acc1 = NeighCycle, Acc1 = AccCycle),
	walk_neighbors(Rest, Stack, Graph, Banned, Acc1, MaxCycle).

%cycle_vertices_to_edges([], _) :- !, fail.
cycle_vertices_to_edges([_], []).
cycle_vertices_to_edges([X, Y | Tail], [X-Y | NTail]) :-
	cycle_vertices_to_edges([Y | Tail], NTail).
