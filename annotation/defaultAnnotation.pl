:- rdf_register_prefix(om,'http://www.ontology-of-units-of-measure.org/vocabularies/om-2/').
:- rdf_register_prefix(val,'http://www.foodvoc.org/page/Valerie/').



string_label(S,X,Y,Label) :-
	cell_value(S,X,Y,Label),
	atom(Label).


unique_string_label(S,X,Y,Unique) :-
	findall(Label,string_label(S,X,Y,Label),Labels),
	sort(Labels, Set),
	member(Unique, Set).


		 /********************************
	          *   DEFAULT II: ANNOTATE TERMS *
		  *******************************/
% Annotate all (NB: also comments) spreadsheet terms by using
% string matching and unit/quantity grammar. Terms are
% annotated with sets of concepts. assumption is that the overlap
% between the vocs is negligible, so terms are either annotated with a
% set of OM or a set of Valerie concepts.

assert_default2_units:-
	forall(unique_string_label(_,_,_,Label),
		forall((unit_label(Label,_,Unit)),
		       assert_default2_unit(Label,Unit))).
assert_default2_unit(Label,Unit):-
	rdf(Unit, sheet:unitOf, literal(Label),default2_annotation), !.
assert_default2_unit(Label,Unit):-
	rdf_assert(Unit, sheet:unitOf, literal(Label),default2_annotation).


assert_default2_quantities:-
	forall(unique_string_label(_,_,_,Label),
		forall((quantity_unit_label(Label,_,_),\+unit_label(Label,_,_),
			unique_label_quantity(Label,OMQuantity,0.85)),
		       assert_default2_quantity(Label,OMQuantity))).

assert_default2_quantity(Label,OMQuantity):-
	rdf(OMQuantity, sheet:quantityOf, literal(Label),default2_annotation), !.
assert_default2_quantity(Label,OMQuantity):-
	rdf_assert(OMQuantity, sheet:quantityOf, literal(Label),default2_annotation).

unique_label_quantity(Label,Quantity,Threshold):-
	findall(Q,label_quantity_concept(Label,_,Threshold,Q),QList),
	sort(QList,Quantities),
	member(Quantity,Quantities),!.



assert_default2_domainterms:-
	forall(unique_string_label(_,_,_,Label),
	       forall(label_dist_domainconcept(Label,_,0.85,Concept),
		    assert_default2_domainterm(Label,Concept))).

assert_default2_domainterm(Label,Concept):-
	rdf(Concept, sheet:domainConceptOf, literal(Label),default2_annotation), !.
assert_default2_domainterm(Label,Concept):-
	rdf_assert(Concept, sheet:domainConceptOf, literal(Label),default2_annotation).




		 /********************************
	          *   DEFAULT I: ANNOTATE TERMS *
		  *******************************/
% Annotate all (NB: also comments) spreadsheet terms by using
% string matching. Terms are annotated with sets of concepts. assumption
% is that the overlap between the vocs is negligible, so terms are
% either annotated with a set of OM or a set of Valerie concepts.
% Isub threshold is set at 0.85

assert_default1_domain:-
	forall(unique_string_label(_,_,_,Label),
	       forall(label_dist_domainconcept(Label,_,0.85,Concept),
		    assert_default1_domainconcept(Label,Concept))).

assert_default1_domainconcept(Label,Concept):-
	rdf(Concept, sheet:domainConceptOf, literal(Label),default1_annotation), !.
assert_default1_domainconcept(Label,Concept):-
	rdf_assert(Concept, sheet:domainConceptOf, literal(Label), default1_annotation).


assert_default1_om:-
	forall(unique_string_label(_,_,_,Label),
	       forall(label_dist_omconcept(Label,_,0.85,Concept),
		    assert_default1_omconcept(Label,Concept))).

assert_default1_omconcept(Label,Concept):-
	rdf(Concept, sheet:omConceptOf, literal(Label),default1_annotation), !.
assert_default1_omconcept(Label,Concept):-
	rdf_assert(Concept, sheet:omConceptOf, literal(Label), default1_annotation).





		 /********************************
	          *   DEFAULT I OM MATCHING *
		  *******************************/

% Find all possible matching vocabulary concepts for a cell label and
% sort them based on best match (highest isub).
label_dist_omconcept(Label, Distance, Threshold, Concept) :-
	translate_term(Label,Translated),
	findall(C, om_candidate(Translated,C), Candidates0),
	sort(Candidates0, Candidates),
	map_list_to_pairs(isub_om_distance(Translated), Candidates, Pairs),
	keysort(Pairs, SortedPairs),
	reverse(SortedPairs, BestFirst),
	member(Distance-Concept, BestFirst),
	Distance > Threshold.


% Preprocess label (stem+tokenize) and find matching vocabulary concepts
om_candidate(Label, Concept) :-
	tokenize_atom(Label, Tokens),
	member(Token, Tokens),
	atom(Token),
	rdf_find_literals(stem(Token), Literals),
	member(Literal, Literals),
	label_om_concept(Literal,Concept).

% For a given cell label calculate isubdistance with respect to a
% vocabulary concept, select the best match (max isub).
isub_om_distance(Label, Concept,  MaxDistance) :-
	aggregate_all(max(Distance),
		      ( label_om_concept(OMLabel, Concept),
			isub(Label, OMLabel, true, Distance)
		      ),
		      MaxDistance).

% label_concept(+Label,-Concept)
% For a preprocessed cell label find matching OM vocabulary
% concepts by searching for their preferred and their alternative label.
label_om_concept(Label,Concept):-
	omVoc(OM),
        (   rdf(Concept,rdfs:label,literal(lang(en,Label)),OM)
	;   rdf(Concept,om:symbol,literal(Label),OM)
	;   rdf(Concept,om:alternative_symbol,literal(Label),OM)
	;   rdf(Concept,om:unofficialAbbreviation,literal(Label),OM)
	;   rdf(Concept,om:unofficialLabel,literal(Label),OM))
	.

omVoc('file:///home/mvs246/Dropbox/WORK/Analyses/Vocabularies/OM-2.0.rdf').



		 /*******************************
		 *   BLOCK EFFECT EVALUATION	*

		 *******************************/
annotate_blocks:-
	segment,
	forall(sheet(S,_),
	       (   assert_body(S),
		   assert_context(S))
	       ),
	 remove_unit_slices,
	 assert_quantity_phenomenon_blocks.


assert_default1b_domain:-
	forall(term_label(Label,phenomenon),
	       forall(label_dist_domainconcept(Label,_,0.85,Concept),
		    assert_default1b_domainconcept(Label,Concept))).

assert_default1b_domainconcept(Label,Concept):-
	rdf(Concept, sheet:domainConceptOf, literal(Label),default1b_annotation), !.
assert_default1b_domainconcept(Label,Concept):-
	rdf_assert(Concept, sheet:domainConceptOf, literal(Label), default1b_annotation).


assert_default1b_om:-
	forall(term_label(Label,unit),
	       forall(label_dist_omconcept(Label,_,0.85,Concept),
		    assert_default1b_omconcept(Label,Concept))),
	forall(term_label(Label,quantity),
	       forall(label_dist_omconcept(Label,_,0.85,Concept),
		    assert_default1b_omconcept(Label,Concept))).

assert_default1b_omconcept(Label,Concept):-
	rdf(Concept, sheet:omConceptOf, literal(Label),default1b_annotation), !.
assert_default1b_omconcept(Label,Concept):-
	rdf_assert(Concept, sheet:omConceptOf, literal(Label), default1b_annotation).



