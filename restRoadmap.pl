
		 /*******************************
		 *	 SHEET DESIGN	*
		 *******************************/

label_inside_DS(Label,Label2):-
	cell_value(Sheet,X,Y,Label),
	cell_property(Sheet,X,Y, block(Block)),
	block(Block,_,DS),
	ds_inside(DS,X2,Y2),
	\+ (X2 == X, Y2 == Y),
	cell_value(Sheet, X2, Y2, Label2).

siblings(Label,SiblingSet):-
	findall(S,label_inside_DS(Label,S),SiblingList),
	sort(SiblingList,SiblingSet).

unique_sibling(Label,Sibling):-
	siblings(Label,SiblingSet),
	member(Sibling,SiblingSet).



frequent_label(Label,Occurrence,Max):-
	findall(L,
		(label_occurrence(L,Occurrence),
		 Occurrence > Max)
	       ,FrequentLabels),
	member(Label,FrequentLabels).

frequent_ancestor_occurrence(Ancestor,Occurrence):-
	findall(Ancestor,
	(frequent_label(Label,_,5)
	,rdf(A,sheet:blockAncestor,literal(Label))
	,agro_pref_label(Ancestor,A))
       ,List),
	countall(List,Ancestor,Occurrence).


full_sibling(Label,Sibling):-
	unique_sibling(Label,Sibling),
	label_occurrence(Label,_,BlockSet),
	label_occurrence(Sibling,_,BlockSet2),
	BlockSet == BlockSet2.


partial_sibling(Label,Sibling):-
	unique_sibling(Label,Sibling),
	not(full_sibling(Label,Sibling)).




		 /*******************************
		 *	 FIND BLOCK PARENTS	*
		 *******************************/
%
%unique_label_ancestor(Block, Ancestor) :-
%	find_all_label_ancestors(Block, AncestorSet),
%	member(Ancestor, AncestorSet).
%
%numsort_block_ancestors(Block,Pairs):-
%	findall(NumDesc-Ancestor,
%		find_block_descendants(Block,Ancestor,_,NumDesc)
%	       ,Pairs0),
%	sort(Pairs0,Pairs1),
%	keysort(Pairs1,Pairs2),
%	reverse(Pairs2,Pairs).
%
%best_block_ancestor(Block,BlockAncestor):-
%	numsort_block_ancestors(Block,Pairs),
%	max_member(_-BlockAncestor,Pairs).
%
%label_blockancestor_set(Label,BlockAncestorSet):-
%	label_presence(Label,_,BlockSet),
%	findall(BlockAncestor,
%		(	member(Block,BlockSet),
%			best_block_ancestor(Block,BlockAncestor)),
%			BlockAncestorList),
%	sort(BlockAncestorList,BlockAncestorSet).
%
%assert_block_ancestor(Label,BlockAncestor):-
%	rdf(sheet:searched_labels, sheet:searched_blockAncestor, literal(Label)), !,
%	rdf(BlockAncestor, sheet:blockAncestor, literal(Label)).
%assert_block_ancestor(Label,BlockAncestor):-
%	label_blockancestor_set(Label,BlockAncestorSet),
%	forall(member(BlockAncestor,BlockAncestorSet),
%	      rdf_assert(BlockAncestor, sheet:blockAncestor, literal(Label),sheet_labels)),
%	rdf_assert(sheet:searched_labels, sheet:searched_blockAncestor,literal(Label), sheet_labels),
%	rdf(BlockAncestor,sheet:blockAncestor, literal(Label)).
%
%
%label_block_ancestor(Label,Block,BlockAncestor):-
%	rdf(BlockAncestor,sheet:blockAncestor, literal(Label)),
%	sheet_block_label(_,Block,Label),
%	best_block_ancestor(Block,BlockAncestor).
%
%
%
%
%find_uncovered_label(Block,Ancestor,Label):-
%	find_block_descendants(Block,Ancestor,DescSet,_),
%	sheet_block_label(_,Block,Label),
%	not(member(Label,DescSet)).
%
%
%select_label_concept(Block,Label,AgroConcept):-
%	sheet_block_label(_,Block,Label),
%	rdf(AgroConcept,sheet:agroConcept,literal(Label),sheet_labels),
%	best_block_ancestor(Block,BlockAncestor),
%	ancestor(AgroConcept,BlockAncestor).





		 /*******************************
		 *	 MATCH WITH OM	*
		 *******************************/

% omVoc('file:///home/mvs246/src/ClioPatria/edesign/CPB_Microtax/OMVocabulary.owl').
%

%om_label(Label,OMConcept):-
%	omVoc(OM),
%	(   rdf(OMConcept,rdfs:label,literal(lang(en,Label)),OM)
%	;   rdf(OMConcept,om:symbol,literal(Label),OM)
%	;   rdf(OMConcept,om:alternative_symbol,literal(Label),OM)
%	).

make_label_tokens(Label,LabelTokens):-
	atom(Label),
	tokenize_atom(Label,TokenList),
	include(atom, TokenList, AtomList),
	not(member(Label,AtomList)),
	append([Label],AtomList,LabelTokens).
make_label_tokens(Label,LabelTokens):-
	atom(Label),
	tokenize_atom(Label,TokenList),
	include(atom, TokenList, LabelTokens).


om_candidate(Label, OMConcept) :-
	omVoc(OM),
	tokenize_atom(Label, Tokens),
	member(Token, Tokens),
	atomic_list_concat([*,Token,*], Pattern),
	Search = literal(like(Pattern), _),
	(   rdf(OMConcept,rdfs:label,Search,OM)
	;   rdf(OMConcept,om:symbol,Search,OM)
	;   rdf(OMConcept,om:alternative_symbol,Search,OM)
	).

om_isub_distance(Label, OMConcept,  MaxDistance) :-
	aggregate_all(max(Distance),
		      ( om_label(OMLabel, OMConcept),
			om_label_distance(Label, OMLabel, Distance)
		      ),
		      MaxDistance).

om_label_distance(L1, L2, D) :-
	atom_length(L1, Len1),
	atom_length(L2, Len2),
	Len1 < 4, Len2 < 4, !,
	(   L1 == L2
	->  D = 1.0
	;   downcase_atom(L1, D),
	    downcase_atom(L2, D)
	->  D = 0.7
	;   D = 0.0
	).
om_label_distance(L1, L2, D) :-
	isub(L1, L2, false, D),
	D > 0.5, !.
om_label_distance(L1, L2, D) :-
	isub(L1, L2, true, D).




label_om_concept(Label, Distance, OMConcept) :-
	findall(C, om_candidate(Label, C), Candidates0),
	sort(Candidates0, Candidates),
	map_list_to_pairs(om_isub_distance(Label), Candidates, Pairs),
	keysort(Pairs, SortedPairs),
	reverse(SortedPairs, BestFirst),
	member(Distance-OMConcept, BestFirst),
	Distance > 0.0.
