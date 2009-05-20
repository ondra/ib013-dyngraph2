/*
 *
 * Resitele: Ondrej Herman (xherman),
 *           Martin Milata (xmilata)
 *   Zadani: 19. Dynamicky graf II
 *   Prolog: SICStus Prolog 4.0.2
 *
 * spanning_tree.pl - minimalni kostra/les grafu
 *
 */

:- module(generator, [
	add_times/4,
	random_events/6,
	random_graph/6,
	events_to_quadruples/2,
	event_comp/2,
	make_quadruples/2,
	empty_ugraph/2
]).

:- use_module(util).
:- use_module(library(random)).
:- use_module(library(ugraphs)).
:- use_module(library(samsort)).

event_comp(E1, E2) :-
        arg(1, E1, C1),
        arg(1, E2, C2),
	(C1 == C2 ->
		arg(2, E1, T1),
       		arg(2, E2, T2),
      		T1 @=< T2
		;
		C1 @=< C2
	)
	.

events_to_quadruples(Events, Quadruples) :-
        samsort(event_comp, Events, SortedEvents),
        make_quadruples(SortedEvents, Quadruples).

make_quadruples([],[]).
make_quadruples([add(X-Y, TC), del(X-Y, TD)|Events], [[X,Y,TC,TD]|Quadruples]) :-
        make_quadruples(Events, Quadruples).

random_graph(NVertices, NGraphs, Time, Stride, EdgeProbability, Quadruples) :-
	random_events(NVertices, NGraphs, Time, Stride, EdgeProbability, Events),
	events_to_quadruples(Events, Quadruples).

random_events(NVertices, NGraphs, Time, Stride, EdgeProbability, Events) :-
	random_ugraph_chain(NVertices, NGraphs, EdgeProbability, Graphs),
	empty_ugraph(NVertices, E),
	append([E|Graphs], [E], Chain),
	gen_events(Chain, NVertices, Time, Stride, Events), !.

gen_events(G, NumVertices, Time, Stride, Events) :- gen_events(G, NumVertices, Time, Stride, [], Events).
gen_events([E], NumVertices, _, _, Accum, Accum) :- empty_ugraph(NumVertices,E), !.
gen_events([G1,G2|Graphs], NumVertices, Time, Stride, Accum, Events) :- !,
	ugraph_diff_to_modification(G1, G2, NumVertices, EAdd, EDel),
	EndTime is Time + Stride,
	add_times(EAdd, Time, EndTime, Add),
	del_times(EDel, Time, EndTime, Del),
	append(Add, Accum, Accum2),
	append(Del, Accum2, Accum3),
	gen_events([G2|Graphs], NumVertices, EndTime, Stride, Accum3, Events), !.


random_ugraph_chain(_, 0, _, []) :- !.
random_ugraph_chain(NumVertices, NumGraphs, EdgeProbability, [G|Graphs]) :-
	random_ugraph(EdgeProbability, NumVertices, GAsym),
	symmetric_closure(GAsym, G),
	N is NumGraphs - 1, !,
	random_ugraph_chain(NumVertices, N, EdgeProbability, Graphs).
	
ugraph_diff_to_modification(G1, G2, NumVertices, Add, Del) :-
	ugraph_diff(G1, G2, GDel),
	ugraph_diff(G2, G1, GAdd),
	ugraph_to_edges(GDel, Del),
	ugraph_to_edges(GAdd, Add), !.
	
add_times(A, StartTime, EndTime, Out) :- add_times(A, StartTime, EndTime, [], Out).
add_times([], _, _, Accum, Accum).
add_times([A|TA], StartTime, EndTime, Accum, Out) :-
	random(StartTime, EndTime, Time),
	add_times(TA, StartTime, EndTime, [add(A, Time) | Accum], Out).
	
del_times(D, StartTime, EndTime, Out) :- del_times(D, StartTime, EndTime, [], Out).
del_times([], _, _, Accum, Accum).
del_times([D|TD], StartTime, EndTime, Accum, Out) :-
	random(StartTime, EndTime, Time),
	del_times(TD, StartTime, EndTime, [del(D, Time) | Accum], Out).

empty_ugraph(N, E) :- empty_ugraph(N, E, 0).
empty_ugraph(N, [], N):-!.
empty_ugraph(N, [C-[]|G], N1) :- C is N1+1, empty_ugraph(N, G, C), !.

