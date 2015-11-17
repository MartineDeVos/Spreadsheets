:- ensure_loaded("../plsheet/test").
:- ensure_loaded(initialBlocks).
:- ensure_loaded(annotateBlocks).
:- ensure_loaded(annotateTerms).

annotate_tables(File):-
	retractall(block(_,_,_)),
	rdf_retractall(_,_,_,sheet_labels),
	unload_file(File),
	load(File),
	segment,
	forall(sheet(S,_),
	       (   assert_body(S),
		   assert_context(S))
	       ),
	 remove_unit_slices,
	 assert_quantity_phenomenon_blocks,
	 assert_unit_terms,
	 assert_quantity_terms,
	 assert_domain_terms,
	 assert_block_terms.


% test functions: aantal termen per soort blok, aantal geannoteerde
% termen per soort blok

annotated_term(Sheet,Type,Label,ConceptName):-
	sheet_block_label(Sheet, _, Type, Label),
	rdf(Concept,_, literal(Label),sheet_labels),
	domain_pref_label(Concept,ConceptName).
