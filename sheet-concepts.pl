:- [roadmap].

sheet_concepts(Sheet, Ontology, Concepts, BlockAncestors) :-
	load_ontology(Ontology),
	ods_unload,
	ods_load(Sheet),
	segment,
	assert_agro_concepts,
	assert_agro_ancestors,
	assert_block_ancestors,
	assert_select_concepts,
	selected_concepts(Concepts),
	block_ancestors(BlockAncestors).

selected_concepts(Concepts) :-
	findall(Concept,
		rdf(Concept, sheet:selectAgroConceptOf, literal(_Label), sheet_labels),
		Concepts0),
	sort(Concepts0, Concepts).

block_ancestors(Concepts) :-
	findall(Concept,
		rdf(Concept, sheet:blockAncestorOf, literal(_Label), sheet_labels),
		Concepts0),
	sort(Concepts0, Concepts).

segment :-
	clean_data,
	assert_labels(_Sheet),
	assert_blocks(_Sheet1,_Type),
	color_sheets(_Sheet2, block).

