:- ensure_loaded(roadmap).
:- rdf_register_prefix(om,'http://www.wurvoc.org/vocabularies/om-1.8/').


om_quantity_label(OMLabel,Quantity):-
	rdf(Quantity,rdfs:subClassOf,om:'Quantity'),
	rdf(Quantity,rdfs:label,literal(lang(en,OMLabel))).


% Preprocess label (stem+tokenize) and find matching OM quantities
% concepts
quantity_candidate(Label, Quantity) :-
	tokenize_atom(Label, Tokens),
	member(Token, Tokens),
	atom(Token),
	rdf_find_literals(stem(Token), Literals),
	member(Literal, Literals),
	om_quantity_label(Literal,Quantity).

% For a given cell label calculate isubdistance with respect to a
% AgroVoc concept, select the best match (max isub).
isub_dist(Label, Quantity,  MaxDistance) :-
	aggregate_all(max(Distance),
		      (om_quantity_label(OMLabel,Quantity) ,
		       isub(OMLabel, Label, true, Distance)
		      ),
		      MaxDistance).


% Find all possible matching AgroVoc concepts for a Label and sort them
% based on best match (highest isub, NB minimum isub is 0.7).
label_om_quantity(Label, Distance, Quantity) :-
	findall(C, quantity_candidate(Label, C), Candidates0),
	sort(Candidates0, Candidates),
	map_list_to_pairs(isub_dist(Label), Candidates, Pairs),
	keysort(Pairs, SortedPairs),
	reverse(SortedPairs, BestFirst),
	member(Distance-Quantity, BestFirst),
	Distance > 0.7.

