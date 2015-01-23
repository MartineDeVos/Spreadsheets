:- ensure_loaded(roadmap).
:- ensure_loaded(blockEvaluation).
:- ensure_loaded(blockOverlap).
:- ensure_loaded(manualAnnotatedTextBlocks).
:- ensure_loaded(manualAnnotatedCategories).
:- rdf_meta (my_annotation(o,r,r,r)).


		 /*******************************
		 *      MAPPING CONCEPTS	*
		 *******************************/

manual_block_label(DS,Sheet,Label):-
	manual_block(DS,Sheet,string,_),
	ds_inside(DS, X, Y),
	cell_value(Sheet, X, Y, Label).

perfectblock_label(Label) :-
	findall(Label, manual_block_label(_,_,Label)
		,Labels),
	sort(Labels, Unique),
	member(Label, Unique).

assert_perfectblock_agro_concepts:-
	forall(perfectblock_label(Label),
	       forall(label_agro_concept(Label,_,AgroConcept),
		    assert_perfectblock_agro_concept(Label,AgroConcept))).

assert_perfectblock_agro_concept(Label,AgroConcept):-
	rdf(AgroConcept, sheet:agroConceptOf, literal(Label),perfectblocks), !.
assert_perfectblock_agro_concept(Label,AgroConcept):-
	rdf_assert(AgroConcept, sheet:agroConceptOf, literal(Label),perfectblocks).


		 /*******************************
		 *      MAPPING ANCESTORS	*
		 *******************************/


assert_perfectblock_agro_ancestors:-
	forall(perfectblock_label(Label),
	       forall(label_ancestor(Label,AgroAncestor),
		    assert_perfectblock_agro_ancestor(Label,AgroAncestor))).

assert_perfectblock_agro_ancestor(Label,AgroAncestor):-
	rdf(AgroAncestor, sheet:agroAncestorOf, literal(Label),perfectblocks), !.
assert_perfectblock_agro_ancestor(Label,AgroAncestor):-
	rdf_assert(AgroAncestor, sheet:agroAncestorOf, literal(Label), perfectblocks).


		 /*******************************
		 *      MAPPING BLOCKPARENTS	*
		 *******************************/

% Find an ancestor associated with a label in a block
perfectblock_label_ancestor(ManualBlock,Label,Ancestor):-
	manual_block_label(ManualBlock,_,Label),
	rdf(Ancestor,sheet:agroAncestorOf,literal(Label),perfectblocks).


% Find all unique ancestors associated with the labels in a block
perfectblock_ancestors(ManualBlock,AncestorSet):-
	manual_block(ManualBlock,_,string,_),
	findall(Ancestor,
		perfectblock_label_ancestor(ManualBlock,_Label,Ancestor),
		AncestorList),
	\+ AncestorList == [],
	sort(AncestorList, AncestorSet).

perfectblock_descendants(ManualBlock,Ancestor,Descendants,NumDesc):-
	perfectblock_ancestors(ManualBlock,AncestorSet),
	member(Ancestor, AncestorSet),
	findall(Label,
		perfectblock_label_ancestor(ManualBlock,Label,Ancestor),
		DescList),
	sort(DescList,Descendants),
	length(Descendants,NumDesc).

% Select the block ancestor with the highest number of descendants in
% that same block
perfectblock_best_ancestor(ManualBlock,BestAncestor):-
	manual_block(ManualBlock,_,string,_),
	findall(NumDesc-Ancestor,
		perfectblock_descendants(ManualBlock,Ancestor,_,NumDesc)
	       ,Pairs2),
	keysort(Pairs2,Pairs1),
	reverse(Pairs1,Pairs),
	max_member(_-BestAncestor,Pairs).


assert_perfectblock_ancestors :-
	forall(perfectblock_best_ancestor(ManualBlock,BlockAncestor),
	       forall(manual_block_label(ManualBlock,_,Label),
		      assert_perfectblock_ancestor(Label, BlockAncestor))).

assert_perfectblock_ancestor(Label, BlockAncestor) :-
	rdf(BlockAncestor, sheet:blockAncestorOf, literal(Label), perfectblocks), !.
assert_perfectblock_ancestor(Label, BlockAncestor) :-
	rdf_assert(BlockAncestor, sheet:blockAncestorOf, literal(Label), perfectblocks).


		 /*******************************
		 *      SELECT CONCEPTS		*
		 *******************************/


label_select_perfectblock_concept(Label, SelectConcept) :-
	rdf(SelectConcept,sheet:agroConceptOf,literal(Label),perfectblocks),
	rdf(BlockAncestor, sheet:blockAncestorOf, literal(Label), perfectblocks),
	ancestor(SelectConcept,BlockAncestor).


% Assert best AgroVoc concepts to spreadsheet labels.
% Store combinations as triples in rdf database
assert_select_perfectblock_concepts :-
	forall(perfectblock_label(Label),
	       forall(label_select_perfectblock_concept(Label, SelectConcept),
		      assert_select_perfectblock_concept(Label,SelectConcept))).

assert_select_perfectblock_concept(Label,SelectConcept) :-
	rdf(SelectConcept, sheet:selectAgroConceptOf, literal(Label),perfectblocks), !.
 assert_select_perfectblock_concept(Label,SelectConcept) :-
	rdf_assert(SelectConcept, sheet:selectAgroConceptOf, literal(Label),perfectblocks).




assert_perfectblocks:-
	rdf_retractall(_,_,_,perfectblocks),
	assert_perfectblock_agro_concepts,
	assert_perfectblock_agro_ancestors,
	assert_perfectblock_ancestors,
	assert_select_perfectblock_concepts.



		 /*******************************
		 *        EVALUATION		*
		 *******************************/

perfectblock_text_map_agree(Sheet,X,Y,Term,AgroConcept):-
	cell_value(Sheet,X,Y,Term),
	my_t_mapping(literal(Term),_,AgroConcept,ground_truth),
	rdf(AgroConcept,sheet:agroConceptOf,literal(Term),perfectblocks).

perfectblock_text_map_exact_agree(Sheet,X,Y,Term,AgroConcept):-
	cell_value(Sheet,X,Y,Term),
	(   my_t_mapping(literal(Term),skos:exactMatch,AgroConcept,ground_truth)
	 ;  my_t_mapping(literal(Term),skos:closeMatch,AgroConcept,ground_truth)
	),
	rdf(AgroConcept,sheet:agroConceptOf,literal(Term),perfectblocks).

perfectblock_annotate_agree(BlockDS,BlockTerm):-
	my_annotation(BlockId,_,BlockTerm,ground_truth),
	block_id_cell_range(BlockId, BlockDS),
	perfectblock_best_ancestor(BlockDS,BlockTerm).


perfectblock_annotate_compare(BlockDS,AgroConceptName,BestAncestorName):-
	my_annotation(BlockId,_,AgroConcept,ground_truth),
	block_id_cell_range(BlockId, BlockDS),
	perfectblock_best_ancestor(BlockDS,BestAncestor),
	agro_pref_label(AgroConceptName,AgroConcept),
	agro_pref_label(BestAncestorName,BestAncestor).


		 /*******************************
		 *    UNIT MAPPING	       *
		 *******************************/

manual_block_unit(DS,Sheet,Label):-
	manual_block(DS,Sheet,unit,_),
	ds_inside(DS, X, Y),
	cell_value(Sheet, X, Y, Label).

perfectblock_unit(Label) :-
	findall(Label, manual_block_unit(_,_,Label)
		,Labels),
	sort(Labels, Unique),
	member(Label, Unique).

assert_perfectblock_OM_units:-
	forall(perfectblock_unit(Label),
		forall(om_label(Label,_,OMUnit),
		       assert_perfectblock_OM_unit(Label,OMUnit))).

assert_perfectblock_OM_unit(Label,OMUnit):-
	rdf(OMUnit, sheet:omUnitOf, literal(Label),perfectblocks), !.
assert_perfectblock_OM_unit(Label,OMUnit):-
	rdf_assert(OMUnit, sheet:omUnitOf, literal(Label),perfectblocks).



perfectblock_unit_map_agree(Sheet,X,Y,Term,OMUnit):-
	cell_value(Sheet,X,Y,Term),
	my_u_mapping(literal(Term),_,OMUnit,ground_truth),
	rdf(OMUnit,sheet:omUnitOf,literal(Term),perfectblocks).

perfectblock_unit_map_exact_agree(Sheet,X,Y,Term,OMUnit):-
	cell_value(Sheet,X,Y,Term),
	(   my_u_mapping(literal(Term),skos:exactMatch,OMUnit,ground_truth)
	 ;  my_u_mapping(literal(Term),skos:closeMatch,OMUnit,ground_truth)
	),
	rdf(OMUnit,sheet:omUnitOf,literal(Term),perfectblocks).

