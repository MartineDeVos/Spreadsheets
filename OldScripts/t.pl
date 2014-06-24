q(Label, BroadLabel, NarrowLabel) :-
	rdf(_A,_B,Label,'file:///home/mvs246/src/ClioPatria/edesign/CPB_Microtax/Dummy.ttl'),
	AgroVoc = 'file:///home/mvs246/src/ClioPatria/edesign/CPB_Microtax/agrovoc_20110223_v_1_3_onlyEN.rdf',
	(   rdf(C, _D, Label, AgroVoc),
	    rdf(C, skos:broader, E),
	    rdf(E, skos:prefLabel, BroadLabel)
	;   rdf(H, _I, Label, AgroVoc),
	    rdf(H, skos:narrower, J, AgroVoc),
	    rdf(J, skos:prefLabel, NarrowLabel)
	).

agrovoc('file:///home/mvs246/src/ClioPatria/edesign/CPB_Microtax/agrovoc_20110223_v_1_3_onlyEN.rdf').

dummy_label(Label) :-
	rdf(_A,skos:prefLabel,Label,'file:///home/mvs246/src/ClioPatria/edesign/CPB_Microtax/Dummy.ttl'),
	Label = literal(lang(en, _)).

edesign_label(Label) :-
	rdf(_A,skos:prefLabel,Label,'file:///home/mvs246/src/ClioPatria/edesign/CPB_Microtax/EDesignModelv2.205sept2011_transl_marked.ttl'),
	Label = literal(lang(en, _)).

best_agrovox_concept(Label, Concept) :-
	agrovoc(AgroVoc),
	rdf(Concept, skos:prefLabel, literal(Label), AgroVoc).

best_agrovox_concept(Label, AgroLabel, Pairs) :-
	findall(Distance-AgroLabel,
		agrovox_concept(Label, AgroLabel, _, Distance),
		Pairs0),
	keysort(Pairs0, Pairs1),
	reverse(Pairs1, Pairs).

agrovox_concept(Label, AgroLabel, Concept, Distance) :-
	agrovoc(AgroVoc),
	rdf(Concept, skos:prefLabel, literal(lang(_,AgroLabel)), AgroVoc),
	isub(Label, AgroLabel, true, Distance),
	Distance > 0.7.

agro_broadlabel(Label, BroadLabel) :-
	agrovoc(AgroVoc),
	rdf(C, skos:prefLabel, Label, AgroVoc),
	rdf(C, skos:broader, E),
	rdf(E, skos:prefLabel, BroadLabel).

agro_narrowlabel(Label, NarrowLabel) :-
	agrovoc(AgroVoc),
	rdf(H, skos:prefLabel, Label, AgroVoc),
	rdf(H, skos:narrower, J, AgroVoc),
	rdf(J, skos:prefLabel, NarrowLabel).

agro_relatedlabel(Label, NarrowLabel) :-
	agrovoc(AgroVoc),
	rdf(H, skos:prefLabel, Label, AgroVoc),
	rdf(H, skos:related, J, AgroVoc),
	rdf(J, skos:prefLabel, NarrowLabel).

q2(L, B, N, R) :-
	dummy_label(L),
	(   agro_broadlabel(L, B)
	;   agro_narrowlabel(L, N)
	;   agro_relatedlabel(L, R)
	).

row(C, Row) :-
	integer(Row), !,
	atom_number(RowA, Row),
	rdf(C, d2s:row, literal(type(xsd:integer, RowA))).
row(C, Row) :-
	rdf(C, d2s:row, literal(type(xsd:integer, RowA))),
	atom_number(RowA, Row).

col(C, Col) :-
	integer(Col), !,
	col_name_number(Name, Col),
	rdf(C, d2s:col, literal(Name)).
col(C, Col) :-
	rdf(C, d2s:col, literal(Name)),
	col_name_number(Name, Col).

col_name_number(Name, Number) :-
	atom(Name), !,
	atom_codes(Name, Codes),
	col_number(Codes, 0, Number).
col_name_number(Name, Number) :-
	col_number_codes(Number, Codes),
	atom_codes(Name, Codes).

col_number([], Num, Num).
col_number([H|T], Num0, Num) :-
	Num1 is Num0*26+H-(0'A-1),
	col_number(T, Num1, Num).

col_number_codes(N, [C]) :-
	N =< 26, !,
	C is N+0'A-1.
col_number_codes(N, List) :-
	N2 is N//26,
	col_number_codes(N2, L1),
	H is (N mod 26)+0'A-1,
	append(L1, [H], List).

left_of(C1, C2) :-
	row(C1, Row),
	row(C2, Row),
	col(C1, Col),
	ColLeft is Col - 1,
	col(C2, ColLeft).
