/*
 *
 * Resitele: Ondrej Herman (xherman1),
 *           Martin Milata (xmilata)
 *   Zadani: 19. Dynamicky graf II
 *   Prolog: SICStus Prolog 4.0.2
 *
 * path.pl - cesta mezi vrcholy, sssp
 *
 */

:- module(path, [
	path/4,
	path/6,
	sssp/3,
	sssp/5
]).

:- use_module(util).

% uchovava informace o dosazitelnosti vrcholu
:- dynamic reached/3.
reached(V) :- reached(V, _, _).

% oznaci vsechny vrcholy jejich predchudcem a casem dosazeni
dijkstra(G, Vertex, StartTime, EndTime) :-
	dijkstra_init(G, Vertex, StartTime, EndTime, Events, InitEdges),
	dijkstra_processevents(Events, InitEdges).

dijkstra(G, Vertex) :- !,
	dijkstra_init(G, Vertex, Events),
	dijkstra_processevents(Events, []).

dijkstra_init(G, Vertex, Events) :- !,
	quadruples_to_packedevents(G, Events), !,
	assert(reached(Vertex, [], 0)).

dijkstra_init(G, Vertex, StartTime, EndTime, Events, InitEdges) :-
	quadruples_to_packedevents(G, Seq), !,
	split_packedevents(Seq, StartTime, Pre, Post),
	split_packedevents(Post, EndTime, Events, _),
	packedevents_to_edges(Pre, InitEdges), !,
	mark_paths([Vertex], InitEdges, [], StartTime).

% updatuje seznam hran a zkousi dosazitelnost novych vrcholu
dijkstra_processevents([], _).
dijkstra_processevents([Event|Events], CurEdges) :-
	apply_packedevent_to_edges(CurEdges, Event, NewEdges),
	dijkstra_step(Event, NewEdges),
	dijkstra_processevents(Events, NewEdges).

	
dijkstra_step(ev(Time, AddEdges, _DelEdges), Edges) :- 
	dijkstra_processedges(Time, AddEdges, Edges).

% pokud je mozne se po hrane nekam dostat, zaznamena se tato informace
% do reached/3
dijkstra_processedges(_, [], _) :- !.
dijkstra_processedges(Time, [A-B|AddEdges], Edges) :- !,
	(
		reached(A),
		assert(reached(B, A, Time)),
		mark_paths([B], Edges, A, Time), !
	;
		reached(B),
		assert(reached(A, B, Time)),
		mark_paths([A], Edges, B, Time), !
	;
		true
	),
	!,
	dijkstra_processedges(Time, AddEdges, Edges)
	.

% rekurzivne znaci dosazitelne vrcholy v aktualnim grafu
mark_paths([], _, _, _) :- !.
mark_paths([Vertex|Vertices], Edges, Through, Time) :-
	(reached(Vertex, _, _) ->
		mark_paths(Vertices, Edges, Through, Time)
		;
		assert(reached(Vertex, Through, Time)),
		mark_paths(Vertices, Edges, Through, Time),
		edges_neighbors(Edges, Vertex, Neighbors),
		mark_paths(Neighbors, Edges, Vertex, Time)
	), !.

% obnovi cisty stav programu
dijkstra_cleanup :- retractall(reached(_,_,_)).

% zpetne sestroji cestu do Dest pomoci reached/3
construct_path([], []).
construct_path(Dest, Path) :-
	reached(Dest, Through, Time),
	construct_path(Through, PartPath),
	Path = [Dest/Time|PartPath].

% hleda nejkratsi cesty do kazdeho vrcholu
sssp(G, Source, Paths) :-
	dijkstra(G, Source),
	bagof(A-B/T, reached(A, B, T), Paths),
	dijkstra_cleanup, !.

sssp(G, Source, StartTime, EndTime, Paths) :-
	dijkstra(G, Source, StartTime, EndTime),
	bagof(A-B/T, reached(A, B, T), Paths),
	dijkstra_cleanup, !.
	
% hleda cestu z Source do Dest. Jako dijkstra, ale skonci po dosazeni Dest.
path(G, Source, Dest, Path) :-
	dijkstra_init(G, Source, Events),
	path_processevents(Events, Dest, []),
	construct_path(Dest, Path),
	dijkstra_cleanup, !.

path(G, Source, Dest, StartTime, EndTime, Path) :-
	dijkstra_init(G, Source, StartTime, EndTime, Events, Edges),
	path_processevents(Events, Dest, Edges),	
	construct_path(Dest, Path),
	dijkstra_cleanup, !.

% jako dijkstra_processevents, ale kontroluje dosazenost cile
path_processevents([], _, _) :- !.
path_processevents([Event|Events], Dest, Edges) :-
	apply_packedevent_to_edges(Edges, Event, NewEdges),
	dijkstra_step(Event, NewEdges),
	(reached(Dest) ->
		true;
		path_processevents(Events, Dest, NewEdges)
	), !.

