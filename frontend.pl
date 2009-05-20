/*
 *
 * Resitele: Ondrej Herman (xherman),
 *           Martin Milata (xmilata)
 *   Zadani: 19. Dynamicky graf II
 *   Prolog: SICStus Prolog 4.0.2
 *
 * frontend.pl - interakce s uzivatelem
 *
 */

:- module(frontend, [
	graph_load/1,
	graph_store/1,
	graph_random/4,
	set_time/2,
	clear_time/0,
	show/0,
	cycles/0,
	clean/0,
	eulerian/0,
	longest_cycle/0,
	max_degree/0,
	max_subgraph/0,
	spanning_tree/1,
	path/2,
	sssp/1
]).

:- use_module(util).
:- use_module(generator).
:- use_module(cycles).
:- use_module(eulerian).
:- use_module(longest_cycle).
:- use_module(max_degree).
:- use_module(max_subgraph).
:- use_module(spanning_tree).
:- use_module(path).

:- dynamic cur_time/2.
:- dynamic cur_graph/1.
initialized :-
	cur_graph(_), !
	;
	write('No graph loaded.'),
	nl.

timed :- cur_time(_, _).

load(Term, File) :-
	seeing(CurIn),
	see(File),
	read(Term),
	seen,
	see(CurIn).
	
store(Term, File) :-
	telling(CurOut),
	tell(File),
	write(Term),
	write('.'),
	told,
	tell(CurOut).

clean :-
	retractall(cur_graph(_)),
	retractall(cur_time(_,_)).
	
clear_time :-
	retractall(cur_time(_,_)).

graph_store(File) :-
	initialized,
	cur_graph(G),
	store(G, File).

graph_load(File) :-
	clean,
	load(G, File),
	assert(cur_graph(G)), !.

graph_random(Size, Steps, Stride, EdgeProbability) :-
	clean,
	random_graph(Size, Steps, 0, Stride, EdgeProbability, G),
	assert(cur_graph(G)),
	show.

set_time(F, T) :-
	retractall(cur_time/2),
	F =< T,
	assert(cur_time(F, T)).

show :-
	initialized,
	cur_graph(G),
	write('Current graph:'), nl,
	write(G), nl,
	length(G, L), write(L), write(' events.'), nl,
	(cur_time(A, B) -> 
		write('Current time interval: '),
		write(A), write(' to '), write(B), nl
		;
		true
	), flush_output, !.


	
cycles :- 
	initialized, cur_graph(G),
	(cur_time(F, T) ->
		cycles(G, F, T, NCycles);
		cycles(G, NCycles)
	),
	write('Graph contains at most '),
	write(NCycles), write(' cycles.'),
	nl, flush_output, !.


eulerian :-
	initialized, cur_graph(G),
	(cur_time(F, T) ->
		eulerian(G, F, T);
		eulerian(G)
	), nl, flush_output, !.


longest_cycle :-
	initialized, cur_graph(G),
	(cur_time(F, T) ->
		longest_cycle(G, F, T, MaxCycle);
		longest_cycle(G, MaxCycle)
	),
	write('Longest cycle is '), write(MaxCycle), write('.'),
	nl, flush_output, !.


max_degree :-
	initialized, cur_graph(G),
	(cur_time(F, T) ->
		max_degree(G, F, T, MaxDegree);
		max_degree(G, MaxDegree)
	),
	write('A vertex with maximal degree is '),
	write(MaxDegree), write('.'), nl, flush_output, !.


max_subgraph :-
	initialized, cur_graph(G),
	(cur_time(F, T) ->
		max_subgraph(G, F, T, MaxSubgraph);
		longest_cycle(G, MaxSubgraph)
	),
	write('Maximal complete subgraph is '),
	write(MaxSubgraph), write('.'), nl, flush_output, !.



spanning_tree(Time) :-
	initialized, cur_graph(G),
	spanning_tree(G, Time, S),
	write(S), nl, flush_output, !.

path(Source, Dest) :-
	initialized, cur_graph(G),
	(cur_time(F, T) ->
		path(G, Source, Dest, F, T, Path_B);
		path(G, Source, Dest, Path_B)
	),
	reverse(Path_B, Path),
	write('Shortest path is '), write(Path), nl, flush_output, !.

sssp(Source) :-
	initialized, cur_graph(G),
	(cur_time(F, T) ->
		sssp(G, Source, F, T, Vert);
		sssp(G, Source, Vert)
	),
	write('Reachability of vertices: '), write(Vert), nl, flush_output, !.

