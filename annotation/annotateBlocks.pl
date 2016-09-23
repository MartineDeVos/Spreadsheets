:- use_module(library(dcg/basics)).
:- ensure_loaded(initialBlocks).
:- ensure_loaded(annotateTerms).


:- rdf_register_prefix(om,'http://www.ontology-of-units-of-measure.org/vocabularies/om-2/').

% Start from initial classification: body and context blocks
% The context blocks are further classified into
% 1) Unit blocks,
% 2) Quantity blocks,
% 3) Phenomenon blocks.


context_block(Unique) :-
	findall(Block, block(Block,context,_), Blocks),
	sort(Blocks, Set),
	member(Unique, Set).




		 /*******************************
		 *  1. ANNOTATE UNIT BLOCKS	*

		 *******************************/
% Recognize unit blocks in context blocks. Context blocks may consist
% entirely of units, or partly. If partly, the unit part is separately
% annotated and substracted from the original context block
remove_unit_slices :-
	forall(context_block(Block), remove_unit_slices(Block)).

% A unit block may consist of one cell, only if the unit slice is the
% same (size) as the original block
remove_unit_slices(Block) :-
	block(Block,context,DS),
	largest_unit_slice(Block, Slice),!,
	ds_subtract(Slice, DS, Rest),
	retract_block(Block),
	assert_ds(Slice, unit),
	forall(member(_Where-RestDS, Rest),
	       assert_ds(RestDS, context)).
remove_unit_slices(_).

assert_ds(DS, Type) :-
	ds_id(DS, Id, block),
	assert_block(block(Id,Type,DS)).

largest_unit_slice(Block, Slice) :-
	block(Block,_,_),
	findall(Size-Slice,
		(   unit_cell_slice(Block,Slice),
		    ds_cell_count(Slice, Size)
		), Pairs),
	sort(Pairs, Unique),
	keysort(Unique, BySize),
	last(BySize, Size-Slice).

% Find unit slices within a block, starting from unit cell candidates
% A slice is a unit slice when it has a cetrain amount of unit
% cells, and more mappings to OM than to the domain ontology. A unit
% slice is either horizontally or vertically oriented (assumption: unit
% slice is single row or single column)
unit_cell_slice(Block,cell_range(Sheet,X,SY, X,EY)):-
	block(Block,_,DS),
	ColSlice = cell_range(Sheet,X,SY, X,EY),
	ds_column_slice(DS,_,ColSlice),
	unit_cell_rate(ColSlice),
	ds_map_check(ColSlice).
unit_cell_slice(Block,cell_range(Sheet, SX,Y, EX,Y)):-
	block(Block,_,DS),
	RowSlice = cell_range(Sheet, SX,Y, EX,Y),
	ds_row_slice(DS,_,RowSlice),
	unit_cell_rate(RowSlice),
	ds_map_check(RowSlice).

% Check whether suggested slice contains more mappings to OM than
% to the domain ontology.
ds_map_check(cell_range(Sheet, SX,SY, EX,EY)):-
	Slice = cell_range(Sheet, SX,SY, EX,EY),
	aggregate_all(count,X-Y,
		      (ds_inside(Slice,X,Y),
		       unit_cell(Slice,X,Y)),
		      UnitMap),
	aggregate_all(count,X-Y,
		      (ds_inside(Slice,X,Y),
		       text_cell(Slice,X,Y)),
		      TextMap),
	UnitMap > TextMap,
	UnitMap > 1.

% Determine whether number of unit cells relative to total cells in
% a DS is above a certain (0.3) threshold.
unit_cell_rate(Slice):-
	aggregate_all(count,X-Y,
		      unit_cell(Slice,X,Y),
		      UnitCount),
	ds_cell_count(Slice, Cells),
	UnitCount/Cells > 0.3.


% If quantity-unit grammar applies, than the term is not a unit,
% but a quantity unit
unit_cell(Slice,X,Y):-
	ds_inside(Slice, X, Y),
	ds_sheet(Slice, Sheet),
	cell_value(Sheet,X,Y,Label),
	unit_label(Label,_,Unit),
	\+ quantity_unit_label(Label,_,_),
	common_unit(Unit).

text_cell(Slice,X,Y):-
	ds_inside(Slice, X, Y),
	ds_sheet(Slice, Sheet),
	cell_value(Sheet,X,Y,Label),
	label_dist_domainconcept(Label,_,0.85,_).


		 /***********************************************
		 *  2+3 ANNOTATE QUANTITY EN PHENOMENON BLOCKS	*

		 ***********************************************/
% Recognize QuantityBlocks among ContextBlocks, and annotate them
% accordingly. The remaining ContextBlocks are annotated as
% PhenomenonBlocks. NB: Check for empty blocks (result of merging text
% blocks + removing unit slices)
assert_quantity_phenomenon_blocks:-
	assert_quantity_ds,
	assert_phenomenon_ds.

assert_quantity_ds :-
	forall(context_block(Block), assert_quantity_ds2(Block)).

assert_quantity_ds2(Block) :-
	block(Block,context,DS),
	quantity_ds(DS),!,
	assert_ds(DS, quantity),
	retract_block(Block).
assert_quantity_ds2(_).

assert_phenomenon_ds:-
	forall(context_block(Block), assert_phenomenon_ds2(Block)).

% NB Check whether the block is not empty (i.e., at least one cell
% with content)
assert_phenomenon_ds2(Block) :-
	block(Block,context,DS),
	ds_sheet(DS,Sheet),
	ds_inside(DS, X, Y),
	cell_value(Sheet,X,Y,_),!,
        assert_ds(DS, phenomenon),
	retract_block(Block).
assert_phenomenon_ds2(_).


% A QuantityBlock is either aligned with a UnitBlock,
% or contains a minimum rate of quantity cells.
quantity_ds(QuantityDS):-
	block(_,context,QuantityDS),
	(   aligned_unit_ds(_,_,QuantityDS)
	->  true
	;   quantity_cell_rate(QuantityDS)
	).

% If a context block is located horizontally or vertially aligned with
% the UnitBlock and the TableBody, than this is a QuantityBlock
% (assumption).
aligned_unit_ds(BodyDS,UnitDS,QuantityDS):-
	block(_,body,BodyDS),
	block(_,unit,UnitDS),
	block(_,_,QuantityDS),
	(   (ds_top_aligned(Sheet,UnitDS,QuantityDS),
	     ds_top_aligned(Sheet,BodyDS,UnitDS))
	;   (ds_right_aligned(Sheet,BodyDS,UnitDS),
	     ds_left_aligned(Sheet,BodyDS,QuantityDS))
	;   (ds_left_aligned(Sheet,BodyDS,UnitDS),
	     ds_left_aligned(Sheet,UnitDS,QuantityDS))
	 ).

% Determine whether number of quantity cells relative to totall cells in
% a DS is above a certain (0.3) threshold.
quantity_cell_rate(DS):-
	aggregate_all(count,X-Y,
		      quantity_cell(DS,X,Y),
		      QuantityCount),
	ds_cell_count(DS, Cells),
	QuantityCount/Cells > 0.3.

% Quantity cells either match quantity-unit grammar, or can be matched
% to a Quantity concept from the OM vocabulary
% Isub for block reognition is high (comparable to default annotation)
quantity_cell(DS,X,Y):-
	ds_inside(DS, X, Y),
	ds_sheet(DS, Sheet),
	cell_value(Sheet,X,Y,Label),
	(   quantity_unit_label(Label,_,_)
	-> true
	;   label_quantity_concept(Label,_,0.85,_)
	).


		 /*******************************
		 *         OM MATCHING         *
		 *******************************/

omVoc('file:///home/mvs246/Dropbox/WORK/Analyses/Vocabularies/OM-2.0.rdf').

% A unit symbol may match with either the preferred symbol of an OMUnit,
% the alternative symbol or the description. NB Only the most commonly
% used units are considered
get_unit_symbol(Symbol,OMUnit):-
	\+ atom_number(Symbol,_),
	(  unit_symbol_match(Symbol,OMUnit)
	;  unit_description_match(Symbol,OMUnit)
	).

common_unit(OMUnit):-
	rdf(om:commonApplicationArea,om:usesUnit,OMUnit).

unit_symbol_match(Symbol,OMUnit):-
	omVoc(OM),
	rdf(OMUnit,om:symbol,literal(Symbol),OM),
	rdf(OMUnit,rdf:type,Type,OM),
	rdf_reachable(Type, rdfs:subClassOf, om:'Unit'), !.
unit_symbol_match(Symbol,OMUnit):-
	omVoc(OM),
	rdf(OMUnit,om:alternativeSymbol,literal(Symbol),OM),
	rdf(OMUnit,rdf:type,Type,OM),
	rdf_reachable(Type, rdfs:subClassOf, om:'Unit').
unit_symbol_match(Symbol,OMUnit):-
	omVoc(OM),
	rdf(OMUnit,om:unofficialAbbreviation,literal(Symbol),OM),
	rdf(OMUnit,rdf:type,Type,OM),
	rdf_reachable(Type, rdfs:subClassOf, om:'Unit').


unit_description_match(Symbol,OMUnit):-
	omVoc(OM),
        rdf(OMUnit,rdfs:label,literal(Symbol),OM),
	rdf(OMUnit,rdf:type,Type,OM),
	rdf_reachable(Type, rdfs:subClassOf, om:'Unit').
unit_description_match(Symbol,OMUnit):-
	omVoc(OM),
        rdf(OMUnit,om:unofficialAbbreviation,literal(Symbol),OM),
	rdf(OMUnit,rdf:type,Type,OM),
	rdf_reachable(Type, rdfs:subClassOf, om:'Unit').

% Checking for prefix is not needed, as OM vocabulary contains
% many prefixed units (i.e., symbol with prefix included).
prefix_match(PrefixSymbol,UnitSymbol):-
	rdf(Unit,om:symbol,literal(UnitSymbol)),
	rdf(Unit,rdf:type,Type),
	rdf_reachable(Type, rdfs:subClassOf, om:'Unit'),
	rdf(Unit,om:hasPrefix,Prefix),
	rdf(Prefix,om:symbol,literal(PrefixSymbol)),!.
prefix_match(_,_).

% Find all possible matching Quantity concepts for a Label and
% sort them based on best match (highest isub).
label_quantity_concept(Label, Distance, Threshold, Quantity) :-
	translate_term(Label,Translated),
	findall(C, quantity_candidate(Translated, C), Candidates0),
	sort(Candidates0, Candidates),
	map_list_to_pairs(quantity_isub_distance(Translated), Candidates, Pairs),
	keysort(Pairs, SortedPairs),
	reverse(SortedPairs, BestFirst),
	member(Distance-Quantity, BestFirst),
	Distance > Threshold.

% For a given cell label calculate isubdistance with respect to a
% vocabulary concept, select the best match (max isub).
quantity_isub_distance(Label, Concept,  MaxDistance) :-
	aggregate_all(max(Distance),
		      ( quantity_concept(DomainLabel, Concept),
			isub(Label, DomainLabel, true, Distance)
		      ),
		      MaxDistance).

% Preprocess label (stem+tokenize) and find matching OM quantity
% concepts
quantity_candidate(Label, Quantity) :-
	tokenize_atom(Label, Tokens),
	member(Token, Tokens),
	atom(Token),
	rdf_find_literals(stem(Token), Literals),
	member(Literal, Literals),
	quantity_concept(Literal,Quantity).

% A Quantity concept in OM can be recognized as a subclass
% of om:'Quantity' or as a concept with the property om:commonlyHasUnit.
quantity_concept(Label,Quantity):-
	omVoc(OM),
        rdf(Quantity,rdfs:label,literal(lang(en,Label)),OM),
	rdf_reachable(Quantity, rdfs:subClassOf, om:'Quantity'),!.
quantity_concept(Label,Quantity):-
	omVoc(OM),
        rdf(Quantity,om:unofficialLabel,literal(Label),OM),
	rdf_reachable(Quantity, rdfs:subClassOf, om:'Quantity'),!.
quantity_concept(Label,Quantity):-
	omVoc(OM),
	rdf(Quantity,om:unofficialAbbreviation,literal(Label),OM),
	rdf_reachable(Quantity, rdfs:subClassOf, om:'Quantity'),!.
quantity_concept(Label,Quantity):-
	rdf(Quantity,rdfs:label,literal(lang(en,Label))),
	rdf(Quantity,om:commonlyHasUnit,_).


		 /*******************************
		 *	    UNIT GRAMMAR	*
		 *******************************/

quantity_unit_label(Label,UnitSymbol,Unit):-
	atom_codes(Label,Codes),
	phrase(quantity_unit_label(UnitSymbol,Unit),Codes,[]).

unit_label(Label,UnitSymbol,Unit):-
	atom_codes(Label,Codes),
	length(Codes,Length),
	Length < 11,
	phrase(unit(UnitSymbol,Unit),Codes,[]).


% Combined quantity-unit labels always (assumption) contain a unit
% symbol, i.e., a unit symbol that can be matched with an OM concept,
% between brackets. Before the brackets is at least one term.
quantity_unit_label(UnitSymbol,Unit) -->
	pre_unit,bracketed_unit(UnitSymbol,Unit).

pre_unit --> pre_unit_codes([_|_]).

pre_unit_codes([H|T]) --> [H],pre_unit_codes(T).
pre_unit_codes([]) --> [].

bracket_open(r) --> "(".
bracket_open(s) --> "[".

bracket_close(r) --> ")".
bracket_close(s) --> "]".

% Unit labels always contains a unit with a symbol that can be matched
% with an OM concept, and may also contain brackets, a prefix
% (indicating th size of the unit) and a separator (e.g. a slash or an
% exponent).
unit(UnitSymbol,Unit) -->bracketed_unit(UnitSymbol,Unit).
unit(UnitSymbol,Unit) -->unbracketed_unit(UnitSymbol,Unit).


%term -->term_codes(_Codes).

% term_codes([H|T]) --> [H], { \+ code_type(H, white) }, !,
% term_codes(T). term_codes([]) --> [].


bracketed_unit(UnitSymbol,Unit) -->
	bracket_open(T),!,
	symbol(UnitSymbol,Unit),
	(   bracket_close(T)
	->  []
	;   sep(_),
	    symbol_codes(_),
	    bracket_close(T)
	).

unbracketed_unit(UnitSymbol,Unit) -->
	symbol(UnitSymbol,Unit),
	(   \+ [_]
	->  []
	;   sep(_),
	    symbol_codes(_),
	    sep
	).

%pre_fix(_) -->[].
%pre_fix(C) --> [C], {pre_fix(C)}.
%pre_fix(C) :- code_type(C, alpha).

symbol(UnitSymbol,Unit)-->
	skip_symbols,
	om_symbol(UnitSymbol,Unit),reciprocal(_).

skip_symbols --> [].
skip_symbols --> symbol_codes_non_greedy([_|_]), sep(_), !, skip_symbols.

reciprocal([]) --> [].
reciprocal(C) --> "-", reciprocal_code(C).

reciprocal_code(C) --> [C],{code_type(C, digit)}.

om_symbol(UnitSymbol,Unit)-->
	symbol_codes(Codes),
	{ atom_codes(UnitSymbol,Codes),
	  get_unit_symbol(UnitSymbol,Unit)
	 }, !.

sep --> \+ [_], !.
sep, [C] --> [C], {sep_code(C)}.

sep(C) --> [C], {sep_code(C)}.
sep_code(C) :- \+ code_type(C, alpha), C \== 0'-.

symbol_codes_non_greedy([]) --> [].
symbol_codes_non_greedy([H|T]) -->[H], symbol_codes_non_greedy(T).

symbol_codes([H|T]) --> [H],symbol_codes(T).
symbol_codes([]) --> [].

%post_term -->[].
%post_term --> term.
















