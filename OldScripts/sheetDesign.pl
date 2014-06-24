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

