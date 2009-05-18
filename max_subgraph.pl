:- module(max_subgraph, [max_subgraph/2, max_subgraph/3, max_subgraph/4]).

:- use_module(util).
:- use_module(library(lists)).

max_subgraph(G, MaxSubgraph) :-
	quadruples_to_packedevents(G, Seq),
	seq_msubgraph([], Seq, [], MaxSubgraph).

max_subgraph(G, Time, MaxSubgraph) :-
	quadruples_to_packedevents(G, Seq),
	split_packedevents(Seq, Time, Events, _),
	packedevents_to_edges(Events, Edges),
	msubgraph(Edges, MaxSubgraph).

max_subgraph(G, Start, End, MaxSubgraph) :-
	quadruples_to_packedevents(G, Seq),
	split_packedevents(Seq, Start, Pre, Post),
	split_packedevents(Post, End, Events, _),
	packedevents_to_edges(Pre, InitEdges),
	msubgraph(InitEdges, InitSubgraph),
	seq_msubgraph(InitEdges, Events, InitSubgraph, MaxSubgraph).

seq_msubgraph(_Edges, [], MaxSubgraph, MaxSubgraph).
seq_msubgraph(Edges, [Event|Tail], Acc, MaxSubgraph) :-
	apply_packedevent_to_edges(Edges, Event, NewEdges),
	msubgraph(NewEdges, Subgraph),
	if(less_graph_size(Acc,Subgraph), Acc1 = Subgraph, Acc1 = Acc),
	seq_msubgraph(NewEdges, Tail, Acc1, MaxSubgraph).

msubgraph([], []).
msubgraph(Graph, MaxSubgraph) :-
	components(Graph, Components),
	max_member(less_graph_size, MaxSubgraph, Components).

% porovnavaci predikat pro velikost grafu
% porovnavame pocet vrcholu, pokud je stejny
% tak porovname pocet hran
less_graph_size(G1, G2) :-
	vertices(G1, [], V1),
	vertices(G2, [], V2),
	length(V1, NVertices1),
	length(V2, NVertices2),
	length(G1, NEdges1),
	length(G2, NEdges2),
	(
		NVertices1 < NVertices2,
		!
	;
		NVertices1 == NVertices2,
		NEdges1 < NEdges2
	).

