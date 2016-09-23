:- use_module(library(semweb/sparql_client)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_litindex)).
:- use_module(library(aggregate)).
:- ensure_loaded(initialBlocks).
:- ensure_loaded(annotateBlocks).
:- ensure_loaded(translatedTerms).

:- rdf_register_prefix(sheet, 'http://vu.nl/sheet/').
:- rdf_register_prefix(val,'http://www.foodvoc.org/page/Valerie/').


% After annotation of blocks, annotate individual terms in the three
% types of blocks:
% 1) Unit terms in Unit blocks,
% 2) Quantity terms in Quantity blocks,
% 3) Domain terms and Block terms in Phenomenon blocks.


sheet_block_label(Sheet, Block, Type, Label) :-
	sheet_object(Sheet, block, block(Block, Type, DS)),
	ds_inside(DS, X, Y),
	cell_value(Sheet, X, Y, Label).


term_label(Unique,Type) :-
	findall(Label, sheet_block_label(_,_Block,Type,Label),Labels),
	sort(Labels, Set),
	member(Unique, Set).


		 /********************************
	          *   ANNOTATE UNIT TERMS *
		  *******************************/
assert_unit_terms:-
	forall(term_label(Label,unit),
		forall(unit_label(Label,_,Unit),
		       assert_unit_term(Label,Unit))).
assert_unit_term(Label,Unit):-
	rdf(Unit, sheet:unitOf, literal(Label),sheet_labels), !.
assert_unit_term(Label,Unit):-
	rdf_assert(Unit, sheet:unitOf, literal(Label),sheet_labels).


		 /********************************
	          *      ANNOTATE QUANTITY TERMS     *
		  *******************************/
assert_quantity_terms:-
	forall(term_label(Label,quantity),
		forall(find_label_quantity(Label,OMQuantity),
		       assert_quantity_term(Label,OMQuantity))).

assert_quantity_term(Label,OMQuantity):-
	rdf(OMQuantity, sheet:quantityOf, literal(Label),sheet_labels), !.
assert_quantity_term(Label,OMQuantity):-
	rdf_assert(OMQuantity, sheet:quantityOf, literal(Label),sheet_labels).


find_label_quantity(Label,Quantity):-
	findall(Q,label_quantity_concept(Label,_,0.85,Q),QList),
	sort(QList,Quantities),
	member(Quantity,Quantities),!.
find_label_quantity(Label,Quantity):-
	findall(Q,deduce_label_quantity(Label,Q),QList),
	sort(QList,Quantities),
	member(Quantity,Quantities).

% Deduce quantity concept from corresponding unit, which is located in
% either neighbouring block, or in same term
neighbour_unit_label(QuantityLabel,UnitLabel):-
	sheet_block_label(Sheet,Block,quantity,QuantityLabel),
	cell_value(Sheet,X,Y,QuantityLabel),
	block(Block, quantity,QuantityDS),
	aligned_unit_ds(_,UnitDS,QuantityDS),
	ds_inside(UnitDS,X1,Y1),
	(   X1 == X
	;   Y1 == Y),
	cell_value(Sheet, X1, Y1, UnitLabel).


deduce_label_quantity(Label,Quantity):-
	neighbour_unit_label(Label,UnitLabel),
	unit_label(UnitLabel,_,OMUnit),
	rdf(Quantity,om:commonlyHasUnit,OMUnit),
	rdf(om:commonApplicationArea, om:usesQuantity, Quantity).

deduce_label_quantity(Label,Quantity):-
	quantity_unit_label(Label,_,OMUnit),
	rdf(Quantity,om:commonlyHasUnit,OMUnit),
	rdf(om:commonApplicationArea, om:usesQuantity, Quantity).


		 /********************************
	          *   ANNOTATE DOMAIN TERMS *
		  *******************************/
valVoc('file:///home/mvs246/Dropbox/WORK/Analyses/Vocabularies/Valerie-2.0.rdf').

% Assert matching vocabulary concepts to spreadsheet labels.
% Store combinations as triples in rdf database
% Threshold is lower once block types are known
assert_domain_terms:-
	forall(term_label(Label,phenomenon),
	       forall(label_dist_domainconcept(Label,_,0.85,Concept),
		    assert_domain_term(Label,Concept))).

assert_domain_term(Label,Concept):-
	rdf(Concept, sheet:domainConceptOf, literal(Label),sheet_labels), !.
assert_domain_term(Label,Concept):-
	rdf_assert(Concept, sheet:domainConceptOf, literal(Label), sheet_labels).



% Find all possible matching vocabulary concepts for a cell label and
% sort them based on best match (highest isub, NB minimum isub is 0.8).
label_dist_domainconcept(Label, Distance, Threshold, Concept) :-
	translate_term(Label,Translated),
	findall(C, domain_candidate(Translated,C), Candidates0),
	sort(Candidates0, Candidates),
	map_list_to_pairs(isub_distance(Translated), Candidates, Pairs),
	keysort(Pairs, SortedPairs),
	reverse(SortedPairs, BestFirst),
	member(Distance-Concept, BestFirst),
	Distance > Threshold.


% Preprocess label (stem+tokenize) and find matching AgroVoc concepts
domain_candidate(Label, Concept) :-
	tokenize_atom(Label, Tokens),
	member(Token, Tokens),
	atom(Token),
	rdf_find_literals(stem(Token), Literals),
	member(Literal, Literals),
	label_domain_concept(Literal,Concept).

% Translate Dutch terms to English
translate_term(Label,Translated):-
	translated(Label,Translated),!.
translate_term(Label,Label).


% For a given cell label calculate isubdistance with respect to a
% vocabulary concept, select the best match (max isub).
isub_distance(Label, Concept,  MaxDistance) :-
	aggregate_all(max(Distance),
		      ( label_domain_concept(DomainLabel, Concept),
			isub(Label, DomainLabel, true, Distance)
		      ),
		      MaxDistance).

% label_concept(+Label,-Concept)
% For a preprocessed cell label find matching domain vocabulary concepts
% by searching for their preferred and their alternative label.
label_domain_concept(Label,Concept):-
	valVoc(ValVoc),
	    (   rdf(Concept,skos:prefLabel,literal(Label))
	    ;   rdf(Concept,skos:altLabel,literal(Label))
	    ),
	    rdf(Concept,_,_,ValVoc).



% For translating the vocabluaryconcept-URI back to the
% human-readable label, use this function after label_concept"
domain_pref_label(Concept,PrefLabel):-
	valVoc(ValVoc),
	rdf(Concept,skos:prefLabel,literal(PrefLabel)),
	rdf(Concept,_,_,ValVoc).


		 /**********************************
		  *  ANNOTATE RELATED DOMAIN TERMS  *
	          **********************************/

% Assert AgroVoc ancestors to spreadsheet labels.
% Store combinations as triples in rdf database
assert_domain_related:-
	forall(term_label(Label,phenomenon),
	       forall(label_related(Label,RelatedConcept),
		    assert_domain_related2(Label,RelatedConcept))).

assert_domain_related2(Label,RelatedConcept):-
	rdf(RelatedConcept, sheet:domainRelatedOf, literal(Label),sheet_labels), !.
assert_domain_related2(Label,RelatedConcept):-
	rdf_assert(RelatedConcept, sheet:domainRelatedOf, literal(Label), sheet_labels).


% Find all vocabulary concepts that are related to a given cell label
label_related(Label,RelatedConcept):-
	findall(Related,
		(rdf(Concept, sheet:domainConceptOf, literal(Label),sheet_labels),
		 indirect_related(Concept,Related)),RelatedList),
	sort(RelatedList,RelatedSet),
	member(RelatedConcept,RelatedSet).

% direct_related(-X,-Y)
% Find vocabulary concepts that are directly related to each other
% by a broader or related relationship
direct_related(X,Y):-
		      (  rdf(X,skos:broader,Y)
		      ;  rdf(X,skos:related,Y)
		      ).

% indirect_related(-X,-Z)
% Find vocabulary concepts that are indirectly related to each other
% by a broader or related relationship
indirect_related(X,Z)  :-
	                 direct_related(X,Z),
	                 (   rdf_reachable(Z, skos:broader, Y)
			 ;   rdf_reachable(Z,skos:related,Y)
			 ).


		 /****************************************
		 *	    ANNOTATE BLOCK TERMS	 *
		 *****************************************/
assert_block_terms :-
	assert_domain_related,
	forall(block_best_related(Block,BlockTerm),
	       forall(sheet_block_label(_, Block, phenomenon, Label),
		      assert_block_term(Label, BlockTerm))).

assert_block_term(Label, BlockTerm) :-
	rdf(BlockTerm, sheet:blockTermOf, literal(Label), sheet_labels), !.
assert_block_term(Label, BlockTerm) :-
	rdf_assert(BlockTerm, sheet:blockTermOf, literal(Label), sheet_labels).


% Select the related concept from a block with the highest number of
% descendants in that same block.
% The number of descendants should be at least 30% of all cells in the
% block
block_best_related(Block,BestRelated):-
	block(Block,phenomenon,DS),
	ds_cell_count(DS, Cells),
	findall(NumDesc-Related,
		( block_descendants(Block,Related,_,NumDesc),
		  NumDesc/Cells > 0.3)
	       ,Pairs2),
	keysort(Pairs2,Pairs1),
	reverse(Pairs1,Pairs),
	max_member(_-BestRelated,Pairs).

% Find and count all unique labels in a block that are associated with a
% particular related concept from that same block
block_descendants(Block,Related,Descendants,NumDesc):-
	block_related(Block,RelatedSet),
	member(Related, RelatedSet),
	findall(Label,
		block_label_related(Block,Label,Related),
		DescList),
	sort(DescList,Descendants),
	length(Descendants,NumDesc).


% Find a related concept associated with a label in a block
block_label_related(Block,Label,Related):-
	sheet_block_label(_,Block,phenomenon,Label),
	rdf(Related,sheet:domainRelatedOf,literal(Label),sheet_labels).


% Find all unique related concepts associated with the labels
% in a block
block_related(Block,RelatedSet):-
	block(Block,phenomenon,_),
	findall(Related,
		block_label_related(Block,_Label,Related),
		RelatedList),
	\+ RelatedList == [],
	sort(RelatedList, RelatedSet).

