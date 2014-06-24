:- ensure_loaded([sheetDesign,domainKnowledge]).
:- use_module(library(semweb/sparql_client)).
:- use_module(library(semweb/rdf_db)).
:- ensure_loaded(plsheet/test).

:- rdf_register_prefix(sheet, 'http://vu.nl/sheet/').

label('carbohydrates').
label('cultivated').
label('dry').
label('forest').
label('grass').
label('industrial').
label('manure').
label('oil').
label('organic').
label('residues').
label('starch').
label('vegetable').
label('waste').
label('wet').
label('wood').


%label('fuel').
%label('gas').
%label('wind').
%label('energy').
%label('nuclear').
%label('power').
%label('geothermal').
%label('methane').
%label('heating').
%label('CO2').
%label('hydrogen').
%label('electricity').
%label('uranium').
%label('coal').
%label('biomass').
%label('oil').
%label('heat').

db_hierarch_pred(Predicate):-
	Predicate = 'skos:broader';
	Predicate ='rdfs:subClassOf'.

% Check whether graph already exists, if so: stop, if not: load
load_graph(Graph):- rdf_graph(Graph),!.
load_graph(Graph):- catch(rdf_load(Graph), E,
			  (   print_message(warning, E),
			      fail
			  )).


% Load graphs of associated, broader (up to 3 steps) resources
load_context(R) :-
	load_context(R, [], 3).
load_context(_, _, 0) :- !.
load_context(R, Tried, _) :-
	member(R, Tried), !.
load_context(R, Tried, Depth) :-
	debug(context, 'Loading ~q', [R]),
	load_graph(R),
	SubDepth is Depth - 1,
	db_hierarch_pred(Predicate),
	forall(rdf(R, Predicate, B, R),
	       load_context(B, [R|Tried], SubDepth)).


% Rewrite label: first letter uppercase, rest downcase
label_first_upper(Label,NewLabel):-
	sub_atom(Label,0,1,_,First),
	upcase_atom(First,FirstUp),
	sub_atom(Label,1,_,0,Rest),
	atom_concat(FirstUp,Rest,NewLabel).


% Build sparql query for english labels
%build_dbresource_query(Label,Query):-
	%atomic_list_concat( [ 'SELECT * WHERE {?s rdfs:label "',Label,'"@en.}'],Query).

% Build sparql query for english labels
build_dbresource_query(Label,Query):-
	atomic_list_concat( [ 'SELECT * WHERE {?s rdfs:label "',Label,'"@en.}']
					       ,Query).
%build_dbresource_query(Label,Query):-
%	atomic_list_concat( [ 'SELECT * WHERE {?s rdfs:label
%						"',Label,'"@en.
%					       ?s dcterms:subject
%					      ?o}'],Query).




% Find all db resources associated with all labels; store labels&resources as triples in rdf database
find_dbresource(Label,Resource):-
	rdf(sheet:searched_labels, sheet:searched, literal(Label)), !,
	rdf(Resource, sheet:label, literal(Label)).
find_dbresource(Label,Resource):-
	forall(find_resource_on_dbpedia(Label, Resource),
	       rdf_assert(Resource, sheet:label, literal(Label), sheet_labels)),
	rdf_assert(sheet:searched_labels, sheet:searched, literal(Label), sheet_labels),
	rdf(Resource, sheet:label, literal(Label)).


% Find db resource associated with label
find_resource_on_dbpedia(Label,Resource):-
	downcase_atom(Label,Label1),
	% NB dbpedia is case sensitive
	(   SearchLabel = Label1
	;   label_first_upper(Label1,SearchLabel)
	),
	build_dbresource_query(SearchLabel,Query),
	sparql_query(Query,row(Resource),
	[host('dbpedia.org'),path('/sparql/')]).

%	sparql_query(Query,row(S,O),
%	[host('dbpedia.org'),path('/sparql/')]), member(Resource,
%	[S,O]).


% Load resource graphs and associated broader graphs for all labels
load_all_dbinfo(Label):-
	forall(
	    (label(Label),
	     find_dbresource(Label,Resource),
	     load_context(Resource))
	      ,true).

parent(X,Y):- rdf(X,skos:broader,Y).


ancestor(X,Y)  :-parent(X,Z), rdf_reachable(Z, skos:broader, Y).


ancestor(X,Y,parent(X)) :- parent(X,Y).
ancestor(X,Y,parent(Z2)) :- parent(Z,Y), ancestor(X,Z,Z2).


labeled_resource(Resource):-
	rdf(Resource,sheet:label,_,sheet_labels).


labeled_ancestor(Descendant,Ancestor):-
	ancestor(Descendant,Ancestor),
	labeled_resource(Descendant).

num_labeled_descendants(Ancestor,DescList,NumDesc):-
	setof(Descendant,labeled_ancestor(Descendant,Ancestor),DescList),
	length(DescList,NumDesc).


numsort_labeled_ancestors(Pairs):-
	findall(NumDesc-Ancestor,
		num_labeled_descendants(Ancestor,_,NumDesc),Pairs0),
	keysort(Pairs0,Pairs1),
	reverse(Pairs1,Pairs).

list_average( List, Avg ):-
	sumlist( List, Sum ),
	length( List, Length),
	Length >0,
	Avg is Sum/Length.


distance_labeled_ancestor(Ancestor,DescList,Avg):-
	setof(Descendant,labeled_ancestor(Descendant,Ancestor),DescList),
	maplist(distance(Ancestor), DescList, Distances),
	list_average(Distances, Avg).

dist_sort_labeled_ancestors(Pairs):-
	findall(AvgDistance-Ancestor,
		distance_labeled_ancestor(Ancestor,_,AvgDistance),Pairs0),
	keysort(Pairs0,Pairs).


distance(Ancestor, Descendant, Distance) :-
	rdf_reachable(Descendant,skos:broader,Ancestor,1000,Distance).


ancestor_score(Ancestor,DescIndex,DistIndex,Score):-
	numsort_labeled_ancestors(DescendantList),
	nth1(DescIndex,DescendantList,Ancestor),
	dist_sort_labeled_ancestors(DistanceList),
	nth1(DistIndex,DistanceList,Ancestor),
	Score is DescIndex + DistIndex.


lcp(R1, R2, P, Distance) :-
	between(1, 10, Distance),
	rdf_reachable(R1, skos:broader, P, Distance, _),
	rdf_reachable(R2, skos:broader, P, Distance, _).


label_inside_DS(Label,Label2):-
	cell_value(Sheet,X,Y,Label),
	cell_property(Sheet,X,Y, block(Block)),
	block(Block,_,DS),
	ds_inside(DS,X2,Y2),
	X2 \== X, Y2 \== Y,
	cell_value(Sheet, X2, Y2, Label2).

sheet_block_label(Sheet, Block, Label) :-
	sheet_object(Sheet, block, block(Block, string, DS)),
	ds_inside(DS, X, Y),
	cell_value(Sheet, X, Y, Label).


label_siblings(Label,Sheet,Sibling):-
	downcase_atom(Label,Label1),
	(   SearchLabel = Label1
	;   label_first_upper(Label1,SearchLabel)
	),
	label_inside_DS(SearchLabel,DS),
	ds_inside(DS,X,Y),
	cell_value(Sheet,X,Y,Sibling).
