:- rdf_meta (manual_mapping(o,r,r,r)).
:- ensure_loaded(manualAnnotatedTerms_literature).


:- rdf_register_prefix(skos,'http://www.w3.org/2004/02/skos/core#').

		 /*******************************
		 *   TERM MAPPING EVALUATION	*

		 *******************************/
% The default annotations include more terms than the manual annotation.
% Evaluation is only performed at the overlapping set, which is equal
% to the manual annotation set.

term_map_agree(Term,Concept,Annotation):-
	manual_mapping(literal(Term),_,Concept,ground_truth),
	rdf(Concept,_,literal(Term),Annotation).


manual_unit(Term,Concept):-
	omVoc(OM),
	manual_mapping(literal(Term),_,Concept,ground_truth),
	rdf(Concept,rdf:type,Type,OM),
	rdf_reachable(Type, rdfs:subClassOf, om:'Unit').

manual_quantity(Term,Concept):-
	manual_mapping(literal(Term),_,Concept,ground_truth),
	rdf_reachable(Concept, rdfs:subClassOf, om:'Quantity').

recognize_grammar_unit(Term):-
	unique_string_label(_,_,_,Term),
	unit_label(Term,_,_),
	\+quantity_unit_label(Term,_,_).

recognize_grammar_quantity(Term):-
	unique_string_label(_,_,_,Term),
	quantity_unit_label(Term,_,_),
	\+unit_label(Term,_,_).


recognize_unit_baseline(Term,Concept,Graph):-
	omVoc(OM),
	rdf(Concept,sheet:omConceptOf,literal(Term),Graph),
	rdf(Concept,rdf:type,Type,OM),
	rdf_reachable(Type, rdfs:subClassOf, om:'Unit').

recognize_quantity_baseline(Term,Concept,Graph):-
	rdf(Concept,sheet:omConceptOf,literal(Term),Graph),
	rdf_reachable(Concept, rdfs:subClassOf, om:'Quantity').



		 /********************************
	          *   CALCULATE OVERLAP *
		  *******************************/
% Annotated with OM, while it shouldn't
false_om_annotation(Term,OMConcept,Annotation):-
	auto_om_annotation(Term,OMConcept,Annotation),
	\+manual_om_annotation(Term,_).

% Annotated with Valerie, while it shouldn't
false_valerie_annotation(Term,ValName,Annotation):-
	auto_valerie_annotation(Term,ValName,Annotation),
	\+manual_valerie_annotation(Term,_).

% Annotated with Valerie, while it shouldn't
false_valerie_annotation(Term,ValName,Annotation):-
	auto_valerie_annotation(Term,ValConcept,Annotation),
	\+manual_valerie_annotation(Term,_),
	domain_pref_label(ValConcept,ValName).

auto_annotation_overlap(Term,Annotation):-
	auto_valerie_annotation(Term,_,Annotation),
	auto_om_annotation(Term,_,Annotation).

manual_annotation_overlap(Term):-
	manual_valerie_annotation(Term,_),
	manual_om_annotation(Term,_).

auto_valerie_annotation(Term,ValName,Annotation):-
	rdf(ValConcept,_,literal(Term),Annotation),
	rdf(ValConcept,_,_,'file:///home/mvs246/Dropbox/WORK/Analyses/Vocabularies/Valerie-2.0.rdf'),
	domain_pref_label(ValConcept,ValName).


auto_om_annotation(Term,OMConcept,Annotation):-
	rdf(OMConcept,_,literal(Term),Annotation),
	rdf(OMConcept,_,_,'file:///home/mvs246/Dropbox/WORK/Analyses/Vocabularies/OM-2.0.rdf').


manual_valerie_annotation(Term,ValName):-
	manual_mapping(literal(Term),_,ValConcept,ground_truth),
	rdf(ValConcept,_,_,'file:///home/mvs246/Dropbox/WORK/Analyses/Vocabularies/Valerie-2.0.rdf'),
	domain_pref_label(ValConcept,ValName).


manual_om_annotation(Term,OMConcept):-
	manual_mapping(literal(Term),_,OMConcept,ground_truth),
	rdf(OMConcept,_,_,'file:///home/mvs246/Dropbox/WORK/Analyses/Vocabularies/OM-2.0.rdf').



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
