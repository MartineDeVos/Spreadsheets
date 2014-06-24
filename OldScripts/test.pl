:- ensure_loaded(main).


label('vegetable oil').
label('vegetable').
label('oil').
label('waste oil').
label('waste').
label('carbohydrates').
label('starch').
label('cultivated wood').
label('cultivated').
label('wood').
label('industrial wood waste').
label('wood waste').
label('forest residues').
label('forest').
label('cultivated grass').
label('grass').
label('manure').
label('organic').
label('residues').
label('dry organic waste').
label('organic waste').
label('dry').
label('wet organic waste').
label('wet').
label('fuel').
label('gas').
label('wind').
label('energy').
label('nuclear').
label('power').
label('geothermal').
label('methane').
label('heating').
label('CO2').
label('hydrogen').
label('electricity').
label('uranium').
label('coal').
label('biomass').
label('oil').
label('heat').



make_label_tokens(Sheet,Label,LabelTokens):-
	cell_value(Sheet,_,_,Label),
	atom(Label),
	tokenize_atom(Label,TokenList),
	include(atom, TokenList, AtomList),
	not(member(Label,AtomList)),
	append([Label],AtomList,LabelTokens).
make_label_tokens(Sheet,Label,LabelTokens):-
	cell_value(Sheet,_,_,Label),
	atom(Label),
	tokenize_atom(Label,TokenList),
	include(atom, TokenList, LabelTokens).


make_search_label(Label,SearchLabel):-
	downcase_atom(Label,Label1),
	(   SearchLabel = Label1
	;   label_first_upper(Label1,SearchLabel)
	).

make_search_tokens(Sheet,Label,SearchTokenList):-
	make_label_tokens(Sheet,Label,LabelTokens),
	findall(SearchToken,
		(member(Token,LabelTokens),
		 make_search_label(Token,SearchToken)),
		List),
	append(LabelTokens,List,SearchTokenList0),
	sort(SearchTokenList0,SearchTokenList).


agrovoc('file:///home/mvs246/src/ClioPatria/edesign/CPB_Microtax/agrovoc_20110223_v_1_3_onlyEN.rdf').



agro_label(Label,AgroConcept):-
	agrovoc(AgroVoc),
	( rdf(AgroConcept,skos:prefLabel,literal(lang(en,Label)), AgroVoc)
	;   rdf(AgroConcept,skos:altLabel,literal(lang(en,Label)), AgroVoc)
	).

%agro_label(Label,Concept):-
%	agrovoc(AgroVoc),
%	(SkosLabel= skos:prefLabel,
%	;SkosLabel= skos:altLabel),
%	rdf(Concept,SkosLabel, literal(lang(en,Label)), AgroVoc).



tokenlist_agro_match(Sheet,Label,AgroLabel,Concept):-
	make_search_tokens(Sheet,Label,SearchTokenList),
	member(AgroLabel,SearchTokenList),
	agro_label(AgroLabel,Concept).


%agro_broad_label(Label,BroadLabel,BroadConcept) :-
%	agrovoc(AgroVoc),
%	label_agro_match(Label,Concept),
%	rdf(Concept,skos:broader,BroadConcept,AgroVoc),
%	rdf(BroadConcept,skos:prefLabel,literal(lang(_,BroadLabel)),AgroVoc).


find_agro_concept(Label,Concept):-
	rdf(sheet:searched_labels, sheet:searched, literal(Label)), !,
	rdf(Concept, sheet:label, literal(Label)).
find_agro_concept(Label,Concept):-
	forall(tokenlist_agro_match(Label,Concept),
	       rdf_assert(Concept, sheet:label, literal(Label), sheet_labels)),
	rdf_assert(sheet:searched_labels, sheet:searched, literal(Label), sheet_labels),
	rdf(Concept, sheet:label, literal(Label)).



label_ancestor(Descendant,DLabel,Ancestor,ALabel):-
	ancestor(Descendant,Ancestor),
	agro_label(DLabel,Descendant),
	label(DLabel),
	agro_label(ALabel,Ancestor).

num_label_descendants(ALabel,DescList,NumDesc):-
	setof(DLabel,label_ancestor(_,DLabel,_,ALabel),
	      DescList),
	length(DescList,NumDesc).


numsort_label_ancestors(Pairs):-
	findall(NumDesc-Ancestor,
		num_label_descendants(Ancestor,_,NumDesc),Pairs0),
	keysort(Pairs0,Pairs1),
	reverse(Pairs1,Pairs).
