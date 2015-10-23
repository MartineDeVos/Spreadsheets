:- use_module(library(dcg/basics)).
:- ensure_loaded(initialBlocks).
:- ensure_loaded(annotateTerms).


:- rdf_register_prefix(om,'http://www.wurvoc.org/vocabularies/om-1.8/').
:- rdf_register_prefix(val,'http://www.foodvoc.org/page/Valerie/').


% Start from initial classification: body and context blocks
% The context blocks are further classified into
% 1) Unit blocks,
% 2) Quantity blocks,
% 3) Phenomenon blocks.



		 /*******************************
		 *  1. ANNOTATE UNIT BLOCKS	*

		 *******************************/
% Recognize unit blocks in context blocks. Context blocks may consist
% entirely of units, or partly. If partly, the unit part is separately
% annotated and substracted from the original context block
remove_unit_slices :-
	forall(block(Block,context,_), remove_unit_slices(Block)).

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
	label_domain_concept(Label,_, Concept).


		 /*******************************
		 *  2. ANNOTATE QUANTITY BLOCKS	*

		 *******************************/
% Start from context blocks
% IF aligned with unit block THEN quantity block (assumption)
% ELIF # quantity cells in block (of q-u of q match) >
% compare with domain match or quant threshold





		 /*******************************
		 *         OM MATCHING         *
		 *******************************/

omVoc('http://www.wurvoc.org/vocabularies/om-1.8/').

% A unit symbol may match with either the preferred symbol of an OMUnit,
% the alternative symbol or the description.
get_om_symbol(Symbol,OMUnit):-
	\+ atom_number(Symbol,_),
	(  unit_symbol_match(Symbol,OMUnit)
	;  unit_description_match(Symbol,OMUnit)
	).

unit_symbol_match(Symbol,OMUnit):-
	omVoc(OM),
	rdf(OMUnit,om:symbol,literal(Symbol),OM),
	rdf(_,om:unit_of_measure,OMUnit,OM),!.
unit_symbol_match(Symbol,OMUnit):-
	omVoc(OM),
	rdf(OMUnit,om:alternative_symbol,literal(Symbol),OM),
	rdf(_,om:unit_of_measure,OMUnit,OM).

unit_description_match(Symbol,OMUnit):-
	omVoc(OM),
        rdf(OMUnit,rdfs:label,literal(lang(en,Symbol)),OM),
	rdf(_,om:unit_of_measure,OMUnit,OM).



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
% with an OM concept. Before the unit only one term is allowed.
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

% A unit always contains a symbol that can be matched with an OM
% concept, and may also contain brackets, a prefix (indicating th size
% of the unit) and a separator (e.g. a slash or an exponent).
unit(Symbol,OMUnit) -->
	bracket_open(T), !,
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
	  get_om_symbol(Symbol,OMUnit)
	  }.

sep --> \+ [_].
sep, [C] --> [C], {sep_code(C)}.

sep_code(C) :- \+ code_type(C, alpha).


symbol_codes([H|T]) --> [H],symbol_codes(T).
symbol_codes([]) --> [].

post_term -->[].
post_term --> term.
















