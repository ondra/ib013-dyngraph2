/*
 *
 * Resitele: Ondrej Herman (xherman1),
 *           Martin Milata (xmilata)
 *   Zadani: 19. Dynamicky graf II
 *   Prolog: SICStus Prolog 4.0.2
 *
 * max_degree.pl - uzel s nejvice spojenimi
 *
 */

:- module(max_degree, [max_degree/2, max_degree/3, max_degree/4]).

:- use_module(library(lists)).
:- use_module(util).

%casovy okamzik
max_degree(G, Time, V) :-
	quadruples_to_packedevents(G, Seq),
	split_packedevents(Seq, Time, Events, _),
	packedevents_to_edges(Events, InitEdges),
	max_degree_edges(InitEdges, V-D),
	D > 0.

%interval
max_degree(G, Start, End, V) :-
	quadruples_to_packedevents(G, AllSeq),
	split_packedevents(AllSeq, Start, PreStart, PostStart),
	split_packedevents(PostStart, End, Seq, _),
	packedevents_to_edges(PreStart, InitEdges),
	max_degree_edges(InitEdges, InitDegree),
	max_degree_seq(InitEdges, Seq, InitDegree, V-_D).

%celkove
max_degree(G, V) :-
	quadruples_to_packedevents(G, Seq),
	max_degree_seq([], Seq, 0-0, V-_D).

% max_degree_seq(+InitEdges, +Seq, -MaxVertice)
% InitEdges - hrany ktere uz v grafu jsou
% Seq - sekvence pridani/odebrani hran
% MaxVertice - vrchol s maximalnim stupnem
max_degree_seq(_, [], V-D, V-D) :- D > 0.
max_degree_seq(Edges, [Event|Tail], AccV-AccD, MaxVertice) :-
	apply_packedevent_to_edges(Edges, Event, NewEdges),
	max_degree_edges(NewEdges, Vertice-Degree),
	if(Degree > AccD, Acc1 = Vertice-Degree, Acc1 = AccV-AccD),
	max_degree_seq(NewEdges, Tail, Acc1, MaxVertice).

max_degree_edges(Edges, MaxVertice) :-
	vertices(Edges, [], Vertices),
	max_degree_edges(Vertices, Edges, 0-0, MaxVertice).

max_degree_edges([], _, MaxDegree, MaxDegree).
max_degree_edges([V|T], Edges, AccV-AccD, MaxDegree) :-
	count_degree(V, Edges, Degree),
	if(Degree > AccD, Acc1 = V-Degree, Acc1 = AccV-AccD),
	max_degree_edges(T, Edges, Acc1, MaxDegree).
