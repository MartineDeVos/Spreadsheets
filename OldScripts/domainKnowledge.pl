agrovoc('file:///home/mvs246/src/ClioPatria/edesign/CPB_Microtax/agrovoc_20110223_v_1_3_onlyEN.rdf').

agro_label(Atom, Concept) :-
	agrovoc(AgroVoc),
	rdf(Concept, skos:prefLabel, literal(lang(en,Atom)), AgroVoc).
edesign('file:///home/mvs246/src/ClioPatria/edesign/CPB_Microtax/EDesignModelv2.205sept2011_transl_marked.ttl').


cell_label(Cell,Label) :-
	edesign(Edesign),
	rdf(Cell,skos:prefLabel,literal(lang(en,Label)),Edesign).

sheet_label(Concept,SheetLabel):-
	edesign(Edesign),
	rdf(Concept,d2s:sheet,literal(SheetLabel),Edesign).

cell_label_tokens(Cell,Label,AtomList) :-
	cell_label(Cell,Label),
	tokenize_atom(Label,TokenList),
	include(atom, TokenList, AtomList).

edesign_tokens(Tokens) :-
	findall(TL, cell_label_tokens(_,_,TL), TLs),
	append(TLs, AllTokens),
	sort(AllTokens, Tokens).

cell_label_agro_match(Cell, Label, Concept, AgroLabel) :-
	cell_label_tokens(Cell,Label,TokenList),
	member(AgroLabel,TokenList),
	agro_label(AgroLabel,Concept).

agro_blabel(AgroLabel,BLabel,BConcept) :-
	agrovoc(AgroVoc),
	cell_label_agro_match(_,_,Concept,AgroLabel),
	rdf(Concept,skos:broader,BConcept,AgroVoc),
	rdf(BConcept,skos:prefLabel,literal(lang(_,BLabel)),AgroVoc).

agro_rlabel(AgroLabel,RLabel,RConcept) :-
	agrovoc(AgroVoc),
	cell_label_agro_match(_,_,Concept,AgroLabel),
	rdf(Concept,skos:related,RConcept,AgroVoc),
	rdf(RConcept,skos:prefLabel,literal(lang(_,RLabel)),AgroVoc).

agro_brlabel(AgroLabel,BRLabel) :-
	agrovoc(AgroVoc),
	agro_blabel(AgroLabel,_,BConcept),
	rdf(BConcept,skos:related,BRConcept,AgroVoc),
	rdf(BRConcept,skos:prefLabel,literal(lang(_,BRLabel)),AgroVoc).

matched_label(Label) :-
	cell_label_agro_match(_Cell, Label, _Concept, _AgroLabel).

write_label_list(Outfile):-
	% Nog wel als een regel; nl toevoegen
	setof(A,matched_label(A),List),
	open(Outfile,write,Stream),
	forall(member(Label, List),
	       (write(Stream,Label),nl(Stream))),
	close(Stream).


% Triples toevoegen aan dataset kan prima, is misschien zelfs handig,
% anders gaat prolog alles elke keer opnieuw uitrekenen.
% Maak van enriched data wel een aparte graph, dan kan je hem weggooien
% wanneer je wilt.
% rdf_create_graph('EnrichedEdesign').
% rdf_assert(a,b,c,'EnrichedEdesign').
% rdf_retractall(a,b,c,'EnrichedEdesign')
