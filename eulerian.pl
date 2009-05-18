:- module(eulerian, [eulerian/1, eulerian/2, eulerian/3]).

:- use_module(util).

% prevest na:
% [ add(cas, [hrana, hrana, hrana]), del(cas, [hrana, hrana, hrana]), ... ]
% [ ev(cas, [hrana hrana], [hrana hrana]), ... ]

% je graf v okamzik Time eulerovsky?
eulerian(G, Time) :-
	quadruples_to_events(G, Seq),
	split_events(Seq, Time, Events, _),
	sequence_to_edgelist(Events, Edges),
	eulerian_edges(Edges).

% je graf eulerovsky v casovem useku Start-End?
% (modifikace ve stejny cas se povazuji za atomicke)
eulerian(G, Start, End) :-
	quadruples_to_packedevents(G, Events),
	%trace,
	split_packedevents(Events, Start, Pre, Post),
	split_packedevents(Post, End, Seq, _),
	packedevents_to_edges(Pre, InitEdges),
	eulerian_edges(InitEdges),
	eulerian_sequence(InitEdges, Seq).

% je graf porad eulerovsky?
eulerian(G) :-
	quadruples_to_packedevents(G, Seq),
	eulerian_sequence([], Seq).

eulerian_edges(Edges) :-
	%notrace,
	%nl, print('trying eulerian on graph '), print(Edges),
	connected(Edges),
	vertices(Edges, [], Vertices),
	count_degrees(Vertices, Edges, Table),
	%trace,
	even_degrees(Table).
	%nl, print('eulerian!').

eulerian_sequence(_, []).
eulerian_sequence(InitEdges, [Event|Tail]) :-
	apply_packedevent_to_edges(InitEdges, Event, NewEdges),
	eulerian_edges(NewEdges),
	eulerian_sequence(NewEdges, Tail).

even_degrees([]).
even_degrees([_-Deg | Tail]) :-
	even(Deg),
	even_degrees(Tail).
