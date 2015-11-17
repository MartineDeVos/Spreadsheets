:- rdf_register_prefix(om,'http://www.wurvoc.org/vocabularies/om-1.8/').
:- rdf_register_prefix(val,'http://www.foodvoc.org/page/Valerie/').



		 /********************************
	          *   ANNOTATE TERMS *
		  *******************************/
% Annotate all (NB: also comments) spreadsheet terms by using
% string matching. Terms are annotated with sets of concepts. assumption
% is that the overlap between the vocs is negligible, so terms are eithr
% annotated with a set of OM or a set of Valerie concepts.

assert_default_domain:-
	forall(cell_value(_,_,_,Label),
	       forall(label_dist_domainconcept(Label,_,Concept),
		    assert_default_concept(Label,Concept))).

assert_default_om:-
	forall(cell_value(_,_,_,Label),
	       forall(label_dist_omconcept(Label,_,Concept),
		    assert_default_concept(Label,Concept))).

assert_default_concept(Label,Concept):-
	rdf(Concept, sheet:conceptOf, literal(Label),default1_annotation), !.
assert_default_concept(Label,Concept):-
	rdf_assert(Concept, sheet:conceptOf, literal(Label), default1_annotation).





		 /********************************
	          *   DEFAULT OM MATCHING *
		  *******************************/

% Find all possible matching vocabulary concepts for a cell label and
% sort them based on best match (highest isub, NB minimum isub is 0.8).
label_dist_omconcept(Label, Distance, Concept) :-
	findall(C, om_candidate(Label,C), Candidates0),
	sort(Candidates0, Candidates),
	map_list_to_pairs(isub_om_distance(Label), Candidates, Pairs),
	keysort(Pairs, SortedPairs),
	reverse(SortedPairs, BestFirst),
	member(Distance-Concept, BestFirst),
	Distance > 0.8.


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
	;   rdf(Concept,om:alternative_symbol,literal(Label),OM)).

omVoc('http://www.wurvoc.org/vocabularies/om-1.8/').



		 /********************************
	          *   CALCULATE OVERLAP *
		  *******************************/

annotation_overlap(Label,OMConcept,ValConcept):-
	cell_value(_,_,_,Label),
	label_dist_domainconcept(Label,_, ValConcept),
	label_dist_omconcept(Label,_, OMConcept).


overlap_om_valerie(Label,OMConcept,ValConcept):-
	om_to_valerie(Label,OMConcept,ValConcept);
	valerie_to_om(Label,OMConcept,ValConcept).

om_to_valerie(Label,OMConcept,ValConcept):-
	label_om_concept(Label,OMConcept),
	label_dist_domainconcept(Label, Distance, ValConcept),
	Distance > 0.8.

valerie_to_om(Label,OMConcept,ValConcept):-
	label_domain_concept(Label,ValConcept),
	label_dist_omconcept(Label, Distance, OMConcept),
	Distance > 0.8.

