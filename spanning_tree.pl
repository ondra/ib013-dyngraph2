:- module(spanning_tree, [spanning_tree/3]).

:- use_module(util).
:- use_module(library(lists)).   % maplist
:- use_module(library(ugraphs)). % min_tree

spanning_tree(G, Time, SpanningForest) :-
	quadruples_to_packedevents(G, Seq),
	split_packedevents(Seq, Time, Events, _),
	packedevents_to_edges(Events, Edges),
	spanning_forest(Edges, SpanningForest).

spanning_forest(Edges, Forest) :-
	components(Edges, Components),
	maplist(component_to_spanningtree, Components, Forest).

component_to_spanningtree(Graph, Tree) :-
	edges_to_ugraph(Graph, Ugraph),
	min_tree(Ugraph, GTree, _),
	ugraph_to_edges(GTree, Tree).
