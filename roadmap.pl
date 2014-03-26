:- use_module(library(semweb/sparql_client)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_litindex)).
:- use_module(library(aggregate)).
:- if(exists_source(library(ods/table))).
:- use_module(library(ods/sheet)).
:- use_module(library(ods/recognise)).
:- use_module(library(ods/table)).
:- use_module(library(ods/datasource)).
:- use_module(library(ods/data)).
:- use_module(labels).
:- else.
:- ensure_loaded(plsheet/test).
:- endif.

:- rdf_register_prefix(sheet, 'http://vu.nl/sheet/').
:- rdf_register_prefix(om,'http://www.wurvoc.org/vocabularies/om-1.8/').

:- dynamic
	config/2.

config(in_scheme, 'http://aims.fao.org/aos/agrovoc').

%%	candidate_concept(+Resource) is semidet.
%
%	True if Resource is a candidate concept

candidate_concept(S) :-
	(   config(in_scheme, Scheme)
	*-> rdf(S, skos:inScheme, Scheme)
	;   config(graph, Graph)
	*-> rdf(S, skos:prefLabel, _, Graph)
	;   true
	), !.


		 /*******************************
		 *	CELL LABELS	*
		 *******************************/


% Retrieve label from a string block in a spreadsheet
sheet_block_label(Sheet, Block, Label) :-
	sheet_object(Sheet, block, block(Block, string, DS)),
	ds_inside(DS, X, Y),
	cell_value(Sheet, X, Y, Label).

%label_blockset(Label,BlockSet):-
%	cell_value(_,_,_,Label),
%	findall(Block,
%		sheet_block_label(_,Block,Label),
%		BlockList),
%	sort(BlockList,BlockSet).

label(Label) :-
	findall(Label, sheet_block_label(_,_Block,Label), Labels),
	sort(Labels, Unique),
	member(Label, Unique).

		 /*******************************
		 *	 MATCH WITH AGROVOC	*
		 *******************************/

% Find AgroVoc concepts by searching for their preferred and their
% alternative label.
agro_label(Label,AgroConcept):-
	(   rdf(AgroConcept,skos:prefLabel,literal(lang(en,Label)))
	;   rdf(AgroConcept,skos:altLabel,literal(lang(en,Label)))
	),
	candidate_concept(AgroConcept).


% For translating AgroVoc concept-URI back to AgroVoc label, use this
% function instead of "agro_label", to avoid redundant results.
agro_pref_label(Label,AgroConcept):-
	rdf(AgroConcept,skos:prefLabel,literal(lang(en,Label))),
	candidate_concept(AgroConcept).


% Preprocess label (stem+tokenize) and find matching AgroVoc concepts
agro_candidate(Label, AgroConcept) :-
	tokenize_atom(Label, Tokens),
	member(Token, Tokens),
	atom(Token),
	rdf_find_literals(stem(Token), Literals),
	member(Literal, Literals),
	(   rdf(AgroConcept,skos:prefLabel,literal(lang(en,Literal)))
	;   rdf(AgroConcept,skos:altLabel,literal(lang(en,Literal)))
	),
	candidate_concept(AgroConcept).


% For a given cell label calculate isubdistance with respect to a
% AgroVoc concept, select the best match (max isub).
isub_distance(Label, AgroConcept,  MaxDistance) :-
	aggregate_all(max(Distance),
		      ( agro_label(AgroLabel, AgroConcept),
			isub(Label, AgroLabel, true, Distance)
		      ),
		      MaxDistance).


% Find all possible matching AgroVoc concepts for a Label and sort them
% based on best match (highest isub, NB minimum isub is 0.7).
label_agro_concept(Label, Distance, AgroConcept) :-
	findall(C, agro_candidate(Label, C), Candidates0),
	sort(Candidates0, Candidates),
	map_list_to_pairs(isub_distance(Label), Candidates, Pairs),
	keysort(Pairs, SortedPairs),
	reverse(SortedPairs, BestFirst),
	member(Distance-AgroConcept, BestFirst),
	Distance > 0.7.


% Assert matching AgroVoc concepts to spreadsheet labels.
% Store combinations as triples in rdf database
assert_agro_concepts:-
	forall(label(Label),
	       forall(label_agro_concept(Label,_,AgroConcept),
		    assert_agro_concept(Label,AgroConcept))).

assert_agro_concept(Label,AgroConcept):-
	rdf(AgroConcept, sheet:agroConceptOf, literal(Label),sheet_labels), !.
assert_agro_concept(Label,AgroConcept):-
	rdf_assert(AgroConcept, sheet:agroConceptOf, literal(Label), sheet_labels).


		 /*******************************
		 *  FIND COMMON AGROVOC PARENTS *
		 *******************************/

parent(X,Y):- rdf(X,skos:broader,Y).

ancestor(X,Y)  :-parent(X,Z), rdf_reachable(Z, skos:broader, Y).


% Find all Agrovoc ancestors (concept URI) associated with the Agrovoc
% concepts of a cell label
label_ancestor(Label,AgroAncestor):-
	findall(Ancestor,
		(rdf(AgroConcept, sheet:agroConceptOf, literal(Label),sheet_labels),
		 ancestor(AgroConcept,Ancestor)),AncestorList),
	sort(AncestorList,AncestorSet),
	member(AgroAncestor,AncestorSet).


% Assert AgroVoc ancestors to spreadsheet labels.
% Store combinations as triples in rdf database
assert_agro_ancestors:-
	forall(label(Label),
	       forall(label_ancestor(Label,AgroAncestor),
		    assert_agro_ancestor(Label,AgroAncestor))).

assert_agro_ancestor(Label,AgroAncestor):-
	rdf(AgroAncestor, sheet:agroAncestorOf, literal(Label),sheet_labels), !.
assert_agro_ancestor(Label,AgroAncestor):-
	rdf_assert(AgroAncestor, sheet:agroAncestorOf, literal(Label), sheet_labels).


% Assert AgroVoc ancestors to spreadsheet labels.
% Store combinations as triples in rdf database
%assert_agro_ancestor(Label,AgroAncestor):-
%	rdf(sheet:searched_labels, sheet:searched_Ancestor, literal(Label)), !,
%	rdf(AgroAncestor, sheet:agroAncestor, literal(Label)).
%assert_agro_ancestor(Label,AgroAncestor):-
%	label_ancestor_set(Label,AncestorSet),
%	forall(member(AgroAncestor,AncestorSet),
%	       rdf_assert(AgroAncestor, sheet:agroAncestor, literal(Label), sheet_labels)),
%	rdf_assert(sheet:searched_labels, sheet:searched_Ancestor, literal(Label), sheet_labels),
%	rdf(AgroAncestor, sheet:agroAncestor, literal(Label)).


		 /*******************************
		 *	 FIND BLOCK PARENTS	*
		 *******************************/
% Find an ancestor associated with a label in a block
block_label_ancestor(Block,Label,Ancestor):-
	sheet_block_label(_,Block,Label),
	rdf(Ancestor,sheet:agroAncestorOf,literal(Label),sheet_labels).


% Find all unique ancestors associated with the labels in a block
block_ancestors(Block,AncestorSet):-
	block(Block,_,_),
	findall(Ancestor,
		block_label_ancestor(Block,_Label,Ancestor),
		AncestorList),
	\+ AncestorList == [],
	sort(AncestorList, AncestorSet).

% Find and count all unique labels in a block that are associated with a
% particular ancestor from that same block
block_descendants(Block,Ancestor,Descendants,NumDesc):-
	block_ancestors(Block,AncestorSet),
	member(Ancestor, AncestorSet),
	findall(Label,
		block_label_ancestor(Block,Label,Ancestor),
		DescList),
	sort(DescList,Descendants),
	length(Descendants,NumDesc).

% Select the block ancestor with the highest number of descendants in
% that same block
block_best_ancestor(Block,BestAncestor):-
	block(Block,_,_),
	findall(NumDesc-Ancestor,
		block_descendants(Block,Ancestor,_,NumDesc)
	       ,Pairs2),
	keysort(Pairs2,Pairs1),
	reverse(Pairs1,Pairs),
	max_member(_-BestAncestor,Pairs).


assert_block_ancestors :-
	forall(block_best_ancestor(Block,BlockAncestor),
	       forall(sheet_block_label(_, Block, Label),
		      assert_block_ancestor(Label, BlockAncestor))).

assert_block_ancestor(Label, BlockAncestor) :-
	rdf(BlockAncestor, sheet:blockAncestorOf, literal(Label), sheet_labels), !.
assert_block_ancestor(Label, BlockAncestor) :-
	rdf_assert(BlockAncestor, sheet:blockAncestorOf, literal(Label), sheet_labels).



		 /*******************************
		 *	 SELECT LABEL CONCEPTS	*
		 *******************************/

% Find AgroVoc concepts associated with a label in a block that are also
% related to the best ancestor of that block.
label_select_concept(Label, SelectConcept) :-
	rdf(SelectConcept,sheet:agroConceptOf,literal(Label),sheet_labels),
	rdf(BlockAncestor, sheet:blockAncestorOf, literal(Label), sheet_labels),
	ancestor(SelectConcept,BlockAncestor).


% Assert best AgroVoc concepts to spreadsheet labels.
% Store combinations as triples in rdf database
assert_select_concepts :-
	forall(label(Label),
	       forall(label_select_concept(Label, SelectConcept),
		      assert_select_concept(Label,SelectConcept))).

assert_select_concept(Label,SelectConcept) :-
	rdf(SelectConcept, sheet:selectAgroConceptOf, literal(Label), sheet_labels), !.
assert_select_concept(Label,SelectConcept) :-
	rdf_assert(SelectConcept, sheet:selectAgroConceptOf, literal(Label), sheet_labels).





%assert_best_concept(Label,BestConcept):-
%	rdf(sheet:searched_labels, sheet:searched_bestConcept, literal(Label)), !,
%	rdf(BestConcept, sheet:bestConcept, literal(Label)).
%assert_best_concept(Label,BestConcept):-
%	forall(label_best_concept(Label,BestConcept),
%	       rdf_assert(BestConcept, sheet:bestConcept, literal(Label), sheet_labels)),
%	rdf_assert(sheet:searched_labels, sheet:searched_bestConcept, literal(Label), sheet_labels),
%	rdf(BestConcept, sheet:bestConcept, literal(Label)).

