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

remove_unit_slices(Block) :-
	block(Block,context,DS),
	largest_unit_slice(Block, Slice), !,
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
		(   unit_cell_slice(Block,_,_,Slice),
		    ds_cell_count(Slice, Size)
		), Pairs),
	sort(Pairs, Unique),
	keysort(Unique, BySize),
	last(BySize, Size-Slice).

% Find unit slices within a block, starting from unit cell candidates
% A slice is a unit slice when it has more mappings to OM than to the
% domain ontology. A unit slice is either horizontally or vertically
% oriented (assumption: unit slice is single row or single column)
unit_cell_slice(Block,X,Y,ColSlice):-
	unit_cell(Block,X,Y,_,_),
	cell_property(Sheet,X,Y, block(Block)),
	block(Block,_,DS),
	ds_column_slice(DS,_,cell_range(Sheet,X,SY, X,EY)),
	ds_map_check(Block,cell_range(Sheet,X,SY, X,EY)),
	ColSlice = cell_range(Sheet,X,SY, X,EY).

unit_cell_slice(Block,X,Y,RowSlice):-
	unit_cell(Block,X,Y,_,_),
	cell_property(_,X,Y, block(Block)),
	block(Block,_,DS),
	ds_row_slice(DS,_,cell_range(Sheet,SX,Y, EX,Y)),
	ds_map_check(Block,cell_range(Sheet, SX,Y, EX,Y)),
	RowSlice = cell_range(Sheet, SX,Y, EX,Y).

% Check whether suggested slice contains more mappings to OM than
% to the domain ontology.
ds_map_check(Block,cell_range(Sheet, SX,SY, EX,EY)):-
	aggregate_all(count,X-Y,
		      (ds_inside(cell_range(Sheet, SX,SY, EX,EY),X,Y),
		       unit_cell(Block,X,Y,_,_)),
		      UnitMap),
	aggregate_all(count,X-Y,
		      (ds_inside(cell_range(Sheet, SX,SY, EX,EY),X,Y),
		       text_cell(Block,X,Y,_,_)),
		      TextMap),
	UnitMap > TextMap,
	UnitMap > 1.

% If quantity-unit grammar applies, than the term is not a unit,
% but a quantity unit
unit_cell(Block,X,Y,Symbol,OMUnit):-
	block(Block,_,DS),
	ds_inside(DS, X, Y),
	ds_sheet(DS, Sheet),
	cell_value(Sheet,X,Y,Label),
	om_label(Label,Symbol,OMUnit),
	\+ quantity_unit_label(Label,Symbol,OMUnit).

text_cell(Block,X,Y,Label,Concept):-
	block(Block,_,DS),
	ds_inside(DS, X, Y),
	ds_sheet(DS, Sheet),
	cell_value(Sheet,X,Y,Label),
	label_dist_domainconcept(Label,_, Concept).


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
	;   quantity_cell_rate(QuantityDS)
	).

% If a context block is located horizontally or vertially aligned with
% the UnitBlock and the TableBody, than this is a QuantityBlock
% (assumption).
aligned_unit_ds(BodyDS,UnitDS,QuantityDS):-
	block(_,body,BodyDS),
	block(_,unit,UnitDS),
	block(_,context,QuantityDS),
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
quantity_cell(DS,X,Y):-
	ds_inside(DS, X, Y),
	ds_sheet(DS, Sheet),
	cell_value(Sheet,X,Y,Label),
	(   quantity_unit_label(Label,_,_)
	;   label_quantity_concept(Label,_,_)
	).


		 /*******************************
		 *         OM MATCHING         *
		 *******************************/

omVoc('file:///home/mvs246/Dropbox/WORK/Analyses/Vocabularies/OM-2.0.rdf').

% A unit symbol may match with either the preferred symbol of an OMUnit,
% the alternative symbol or the description.
get_unit_symbol(Symbol,OMUnit):-
	\+ atom_number(Symbol,_),
	(  unit_symbol_match(Symbol,OMUnit)
	;  unit_description_match(Symbol,OMUnit)
	).

unit_symbol_match(Symbol,OMUnit):-
	omVoc(OM),
	rdf(OMUnit,om:symbol,literal(Symbol),OM),
	rdf(OMUnit,rdf:type,om:'Unit',OM),!.
unit_symbol_match(Symbol,OMUnit):-
	omVoc(OM),
	rdf(OMUnit,om:alternativeSymbol,literal(Symbol),OM),
	rdf(OMUnit,rdf:type,om:'Unit',OM).

unit_description_match(Symbol,OMUnit):-
	omVoc(OM),
        rdf(OMUnit,rdfs:label,literal(lang(en,Symbol)),OM),
	rdf(_,om:unit_of_measure,OMUnit,OM).


% Find all possible matching Quantity concepts for a Label and
% sort them based on best match (highest isub, NB minimum isub is 0.7).
label_quantity_concept(Label, Distance, Quantity) :-
	findall(C, quantity_candidate(Label, C), Candidates0),
	sort(Candidates0, Candidates),
	map_list_to_pairs(quantity_isub_distance(Label), Candidates, Pairs),
	keysort(Pairs, SortedPairs),
	reverse(SortedPairs, BestFirst),
	member(Distance-Quantity, BestFirst),
	Distance > 0.7.

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

% A Quantity concept in OM can be recognized as a subclass a subclass
% of om:'Quantity' or as a concept with the property om:commonlyHasUnit.
quantity_concept(Label,Quantity):-
	rdf(Quantity,rdfs:subClassOf,om:'Quantity'),
	rdf(Quantity,rdfs:label,literal(lang(en,Label))).
quantity_concept(Label,Quantity):-
	rdf(Quantity,om:commonlyHasUnit,_),
	rdf(Quantity,rdfs:label,literal(lang(en,Label))).


		 /*******************************
		 *	    UNIT GRAMMAR	*
		 *******************************/

quantity_unit_label(Label,Symbol,OMUnit):-
	atom_codes(Label,Codes),
	phrase(quantity_unit_label(Symbol,OMUnit),Codes,[]).

om_label(Label,Symbol,OMUnit):-
	atom_codes(Label,Codes),
	phrase(unit_label(Symbol,OMUnit),Codes,[]).


% Combined quantity-unit labels always (assumption) contain a unit
% symbol, i.e., a unit symbol that can be matched with an OM concept,
% between brackets. Before the brackets is at least one term.
quantity_unit_label(Symbol,OMUnit) -->
	pre_unit,bracket_open(T),unit_label(Symbol,OMUnit),bracket_close(T).

pre_unit --> pre_unit_codes(Codes),
	{atom_codes(Term,Codes),
	 atom(Term),
	 length(Codes,Length),
	 Length > 0}.

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
unit_label(Symbol,OMUnit) -->
	pre_term,unit(Symbol,OMUnit),post_term.


pre_term -->[].
pre_term --> term,white,whites.

term -->term_codes(Codes),
	{atom_codes(Term,Codes),
	 atom(Term)}.

term_codes([H|T]) --> [H], { \+ code_type(H, white) },
term_codes(T).
term_codes([]) --> [].

unit(Symbol,OMUnit) -->
	bracket_open(T),!,
	pre_fix,
	symbol(Symbol,OMUnit),
	bracket_close(T).
unit(Symbol,OMUnit) -->
	pre_fix,symbol(Symbol,OMUnit),sep.

pre_fix -->[].
pre_fix -->[C], { code_type(C, alpha) }.


symbol(Symbol,OMUnit)-->
	om_symbol(Symbol,OMUnit).

om_symbol(Symbol,OMUnit)-->
	symbol_codes(Codes),
	{ atom_codes(Symbol,Codes),
	  get_unit_symbol(Symbol,OMUnit)
	  }.

sep --> \+ [_].
sep, [C] --> [C], {sep_code(C)}.

sep_code(C) :- \+ code_type(C, alpha).


symbol_codes([H|T]) --> [H],symbol_codes(T).
symbol_codes([]) --> [].

post_term -->[].
post_term --> term.
















