:- ensure_loaded(manualAnnotatedTerms).
:- ensure_loaded(manualAnnotatedCategories).
:- ensure_loaded(roadmap).
:- ensure_loaded(blockEvaluation).
:- ensure_loaded(blockOverlap).

:- rdf_register_prefix(skos,'http://www.w3.org/2004/02/skos/core#').
:- rdf_register_prefix(sheet, 'http://vu.nl/sheet/').
:- rdf_register_prefix(om,'http://www.wurvoc.org/vocabularies/om-1.8/').
:- rdf_register_prefix(agro,'http://aims.fao.org/aos/agrovoc/').
:- rdf_meta (my_t_mapping(o,r,r,r)).
:- rdf_meta (my_u_mapping(o,r,r,r)).
:- rdf_meta (my_annotation(o,r,r,r)).

		 /*******************************
		 *   TEXT MAPPING EVALUATION	*

		 *******************************/

% Terms that are included in manual annotation of text terms
% NB irrespective of blocks
% NB remove manually recognized unit terms
unique_term_for_man_annot(Sheet,X,Y,Term):-
	findall(Term,
		(cell_value(Sheet,X,Y,Term),
		 \+number(Term),
		 \+my_u_mapping(literal(Term),_,_,ground_truth)
		), Terms),
	sort(Terms, Unique),
	member(Term, Unique).

term_for_man_annot(Sheet,X,Y,Term):-
		cell_value(Sheet,X,Y,Term),
		\+number(Term),
		\+my_u_mapping(literal(Term),_,_,ground_truth).

% Terms that are included in manual, but not in automatic annotation
term_outside_block(Sheet,X,Y,Term):-
	cell_value(Sheet,X,Y,Term),
	\+number(Term),
	\+sheet_block_label(_,_,Term),
	\+my_u_mapping(literal(Term),_,_,ground_truth).

% Terms that are included in automatic annotation of text terms
% NB should be in block, but correct for block overlap
% NB some text terms are false positives, i.e., ae actually units
term_for_auto_annot(Sheet,X,Y,Term):-
	sheet_object(Sheet, block, block(_, string, DS)),
	ds_inside(DS, X, Y),
	cell_value(Sheet, X, Y, Term).
%	\+my_u_mapping(literal(Term),_,_,ground_truth).

manual_map_term(Sheet, X, Y, Term):-
	cell_value(Sheet, X, Y, Term),
	my_t_mapping(literal(Term),_,_,ground_truth).

manual_exact_map_term(Sheet, X, Y, Term):-
	cell_value(Sheet, X, Y, Term),
	(   my_t_mapping(literal(Term),skos:exactMatch,_,ground_truth)
	 ;  my_t_mapping(literal(Term),skos:closeMatch,_,ground_truth)
	).

auto_map_term(Sheet, X, Y, Term,AgroConcept):-
	sheet_object(Sheet, block, block(_, string, DS)),
	ds_inside(DS, X, Y),
	cell_value(Sheet, X, Y, Term),
	rdf(AgroConcept,sheet:agroConceptOf,literal(Term),sheet_labels).

text_map_agree(Sheet,X,Y,Term,AgroConcept):-
	sheet_object(Sheet, block, block(_, string, DS)),
	ds_inside(DS, X, Y),
	cell_value(Sheet, X, Y, Term),
	my_t_mapping(literal(Term),_,AgroConcept,ground_truth),
	rdf(AgroConcept,sheet:agroConceptOf,literal(Term),sheet_labels).

term_outside_recall(Sheet,X,Y,Term):-
	cell_value(Sheet, X, Y, Term),
	my_t_mapping(literal(Term),_,AgroConcept,ground_truth),
	\+ text_map_agree(Sheet,X,Y,Term,AgroConcept).

term_outside_precision(Sheet,X,Y,Term):-
	cell_value(Sheet, X, Y, Term),
	rdf(AgroConcept,sheet:agroConceptOf,literal(Term),sheet_labels),
	\+  text_map_agree(Sheet,X,Y,Term,AgroConcept).

text_map_exact_agree(Sheet,X,Y,Term,AgroConcept):-
	sheet_object(Sheet, block, block(_, string, DS)),
	ds_inside(DS, X, Y),
	cell_value(Sheet, X, Y, Term),
	rdf(AgroConcept,sheet:agroConceptOf,literal(Term),sheet_labels),
	(   my_t_mapping(literal(Term),skos:exactMatch,AgroConcept,ground_truth)
	 ;  my_t_mapping(literal(Term),skos:closeMatch,AgroConcept,ground_truth)
	).

% After iteration with block terms
auto_selectmap_term(Sheet, X, Y, Term,AgroConcept):-
	sheet_object(Sheet, block, block(_, string, DS)),
	ds_inside(DS, X, Y),
	cell_value(Sheet, X, Y, Term),
	rdf(AgroConcept,sheet:selectAgroConceptOf,literal(Term),sheet_labels).

text_selectmap_agree(Sheet,X,Y,Term,AgroConcept):-
	sheet_object(Sheet, block, block(_, string, DS)),
	ds_inside(DS, X, Y),
	cell_value(Sheet, X, Y, Term),
	my_t_mapping(literal(Term),_,AgroConcept,ground_truth),
	rdf(AgroConcept,sheet:selectAgroConceptOf,literal(Term),sheet_labels).

text_selectmap_exact_agree(Sheet,X,Y,Term,AgroConcept):-
	sheet_object(Sheet, block, block(_, string, DS)),
	ds_inside(DS, X, Y),
	cell_value(Sheet, X, Y, Term),
	(   my_t_mapping(literal(Term),skos:exactMatch,AgroConcept,ground_truth)
	 ;  my_t_mapping(literal(Term),skos:closeMatch,AgroConcept,ground_truth)
	),
	rdf(AgroConcept,sheet:selectAgroConceptOf,literal(Term),sheet_labels).

% False negative: annotation that is correct, but thrown out after
% iteration
text_select_false_negative(Sheet,X,Y,Term,AgroConcept):-
	text_map_agree(Sheet,X,Y,Term,AgroConcept),
	\+rdf(AgroConcept,sheet:selectAgroConceptOf,literal(Term),sheet_labels).

% True negatives: false initial annotation throw out after iteration
text_select_true_negative(Sheet,X,Y,Term,AgroConcept):-
	rdf(AgroConcept,sheet:agroConceptOf,literal(Term),sheet_labels),
	\+text_map_agree(Sheet,X,Y,Term,AgroConcept),
	\+rdf(AgroConcept,sheet:selectAgroConceptOf,literal(Term),sheet_labels).


		/*******************************
		 *   UNIT MAPPING EVALUATION	*
		 *******************************/
man_unit_map(Sheet, X, Y, Term):-
	cell_value(Sheet, X, Y, Term),
	my_u_mapping(literal(Term),_,_,ground_truth).

unit_for_auto_annot(Sheet,X,Y,Term):-
	sheet_object(Sheet, block, block(_,_, DS)),
	ds_inside(DS, X, Y),
	cell_value(Sheet, X, Y, Term),
	is_om_label(Term).

auto_unit_map(Sheet,X,Y,Term):-
	sheet_object(Sheet, block, block(_,_, DS)),
	ds_inside(DS, X, Y),
	cell_value(Sheet, X, Y, Term),
	rdf(_,sheet:omUnitOf,literal(Term),sheet_labels).

man_exact_unit_map(Sheet,X,Y,Term):-
	cell_value(Sheet,X,Y, Term),
	(   my_u_mapping(literal(Term),skos:exactMatch,_,ground_truth)
	;   my_u_mapping(literal(Term),skos:closeMatch,_,ground_truth)
	).

unit_map_agree(Sheet,X,Y,Term,OMConcept):-
	sheet_object(Sheet, block, block(_,_, DS)),
	ds_inside(DS, X, Y),
	cell_value(Sheet, X, Y, Term),
	my_u_mapping(literal(Term),_,OMConcept,ground_truth),
	rdf(OMConcept,sheet:omUnitOf,literal(Term),sheet_labels).


unit_map_exact_agree(Sheet,X,Y,Term,OMConcept):-
	sheet_object(Sheet, block, block(_,_, DS)),
	ds_inside(DS, X, Y),
	cell_value(Sheet, X, Y, Term),
	(   my_u_mapping(literal(Term),skos:exactMatch,OMConcept,ground_truth)
	 ;  my_u_mapping(literal(Term),skos:closeMatch,OMConcept,ground_truth)
	),
	rdf(OMConcept,sheet:omUnitOf,literal(Term),sheet_labels).

% After iteration with unit blocks
unit_for_auto_refined_annot(Sheet,X,Y,Term):-
	sheet_object(Sheet, block, block(_,unit, DS)),
	ds_inside(DS, X, Y),
	cell_value(Sheet, X, Y, Term),
	is_om_label(Term).

auto_unit_refined_map(Sheet,X,Y,Term):-
	sheet_object(Sheet, block, block(_,unit, DS)),
	ds_inside(DS, X, Y),
	cell_value(Sheet, X, Y, Term),
	rdf(_,sheet:omUnitOf,literal(Term),sheet_labels).

unit_refined_map_agree(Sheet,X,Y,Term,OMConcept):-
	sheet_object(Sheet, block, block(_,unit, DS)),
	ds_inside(DS, X, Y),
	cell_value(Sheet, X, Y, Term),
	my_u_mapping(literal(Term),_,OMConcept,ground_truth),
	rdf(OMConcept,sheet:omUnitOf,literal(Term),sheet_labels).

unit_refined_map_exact_agree(Sheet,X,Y,Term,OMConcept):-
	sheet_object(Sheet, block, block(_,unit, DS)),
	ds_inside(DS, X, Y),
	cell_value(Sheet, X, Y, Term),
	(   my_u_mapping(literal(Term),skos:exactMatch,OMConcept,ground_truth)
	 ;  my_u_mapping(literal(Term),skos:closeMatch,OMConcept,ground_truth)
	),
	rdf(OMConcept,sheet:omUnitOf,literal(Term),sheet_labels).

% False negatives: first annotation includes all string labels,
% refined annotaion includes only unit block terms. So unit
% terms in string blocks are missed.
refined_false_negative(Sheet,X,Y,Term):-
	sheet_object(Sheet, block, block(_,string, DS)),
	ds_inside(DS, X, Y),
	cell_value(Sheet, X, Y, Term),
	auto_unit_map(Sheet,X,Y,Term).



		 /*******************************
		 * BLOCK ANNOTATION EVALUATION  *
		 *******************************/

block_annotate_agree(AgroConcept,AgroConceptName):-
	my_annotation(BlockId,_,AgroConcept,ground_truth),
	block_id_cell_range(BlockId, BlockDS),
	block(BlockId2,_,BlockDS),
	block_best_ancestor(BlockId2,AgroConcept),
	agro_pref_label(AgroConceptName,AgroConcept).


block_annotate_compare(BlockDS,AgroConceptName,BestAncestorName):-
	my_annotation(BlockId,_,AgroConcept,ground_truth),
	block_id_cell_range(BlockId, BlockDS),
	block(BlockId2,_,BlockDS),
	block_best_ancestor(BlockId2,BestAncestor),
	agro_pref_label(AgroConceptName,AgroConcept),
	agro_pref_label(BestAncestorName,BestAncestor).


test_ancestor(BlockId,Pairs,BestAncestor):-
	block_id_cell_range(BlockId, BlockDS),
	block(BlockId2,_,BlockDS),
	block(BlockId2,string,_),
	findall(NumDesc-Ancestor,
		block_descendants(BlockId2,Ancestor,_,NumDesc)
	       ,Pairs2),
	keysort(Pairs2,Pairs1),
	reverse(Pairs1,Pairs),
	max_member(_-BestAncestor,Pairs).



		 /*******************************
		 *     NO-BLOCK MAPPING	       *
		 *******************************/
string_label(Label) :-
	findall(Label, (cell_value(_,_,_,Label),
		       \+number(Label)),
		Labels),
	sort(Labels, Unique),
	member(Label, Unique).

assert_noblock_agro_concepts:-
	forall(string_label(Label),
	       forall(label_agro_concept(Label,_,AgroConcept),
		    assert_noblock_agro_concept(Label,AgroConcept))).

assert_noblock_agro_concept(Label,AgroConcept):-
	rdf(AgroConcept, sheet:agroConceptOf, literal(Label),noblocks), !.
assert_noblock_agro_concept(Label,AgroConcept):-
	rdf_assert(AgroConcept, sheet:agroConceptOf, literal(Label),noblocks).


assert_noblock_OM_units:-
	forall(string_label(Label),
		forall(om_label(Label,_,OMUnit),
		       assert_noblock_OM_unit(Label,OMUnit))).

assert_noblock_OM_unit(Label,OMUnit):-
	rdf(OMUnit, sheet:omUnitOf, literal(Label),noblocks), !.
assert_noblock_OM_unit(Label,OMUnit):-
	rdf_assert(OMUnit, sheet:omUnitOf, literal(Label),noblocks).

noblock_text_map_agree(Sheet,X,Y,Term,AgroConcept):-
	cell_value(Sheet,X,Y,Term),
	my_t_mapping(literal(Term),_,AgroConcept,ground_truth),
	rdf(AgroConcept,sheet:agroConceptOf,literal(Term),noblocks).

noblock_text_map_exact_agree(Sheet,X,Y,Term,AgroConcept):-
	cell_value(Sheet,X,Y,Term),
	(   my_t_mapping(literal(Term),skos:exactMatch,AgroConcept,ground_truth)
	 ;  my_t_mapping(literal(Term),skos:closeMatch,AgroConcept,ground_truth)
	),
	rdf(AgroConcept,sheet:agroConceptOf,literal(Term),noblocks).

noblock_unit_map_agree(Sheet,X,Y,Term,OMUnit):-
	cell_value(Sheet,X,Y,Term),
	my_u_mapping(literal(Term),_,OMUnit,ground_truth),
	rdf(OMUnit,sheet:omUnitOf,literal(Term),noblocks).

noblock_unit_map_exact_agree(Sheet,X,Y,Term,OMUnit):-
	cell_value(Sheet,X,Y,Term),
	(   my_u_mapping(literal(Term),skos:exactMatch,OMUnit,ground_truth)
	 ;  my_u_mapping(literal(Term),skos:closeMatch,OMUnit,ground_truth)
	),
	rdf(OMUnit,sheet:omUnitOf,literal(Term),noblocks).


