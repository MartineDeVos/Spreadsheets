:- use_module(library(dcg/basics)).
:- ensure_loaded("../plsheet/test").
:- ensure_loaded(annotateTerms).


:- rdf_register_prefix(om,'http://www.wurvoc.org/vocabularies/om-1.8/').



% Start from initial classification: body and context blocks
% The context blocks are further classifid into 1) unit block,
% 2) a. combined Quantity-Unit block, b. Quantity block,
% 3) Phenomenon block.


		 /*******************************
		 *  1. ANNOTATE UNIT BLOCKS	*

		 *******************************/

% Recognize unit blocks in context blocks.
% Context blocks may consist entirely of units, or partly.
% If partly, the unit part is separately annotated and substracted from
% the original context block
remove_unit_slices :-
	forall(block(Block,_,_), remove_unit_slices(Block)).

remove_unit_slices(Block) :-
	block(Block,Type,DS),
	largest_unit_slice(Block, Slice), !,
	ds_subtract(Slice, DS, Rest),
	retract_block(Block),
	assert_ds(Slice, unit),
	forall(member(_Where-RestDS, Rest),
	       assert_ds(RestDS, Type)).
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

om_label(Label,Symbol,OMUnit):-
	atom_codes(Label,Codes),
	phrase(unit_label(Symbol,OMUnit),Codes,[]).

unit_cell(Block,X,Y,Symbol,OMUnit):-
	block(Block,_,DS),
	ds_inside(DS, X, Y),
	ds_sheet(DS, Sheet),
	cell_value(Sheet,X,Y,Label),
	om_label(Label,Symbol,OMUnit),
	atom_codes(Label,Codes),
	length(Codes,Length),
	Length > 25.

text_cell(Block,X,Y,Label,DomainConcept):-
	block(Block,_,DS),
	ds_inside(DS, X, Y),
	ds_sheet(DS, Sheet),
	cell_value(Sheet,X,Y,Label),
	label_domain_concept(Label,_, DomainConcept).

% Check whether suggested slice contains more mappings to OM than
% domain vocabulary
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

% Find unit slices within a block, starting from unit cell candidates
% A slice is a unit slice when it has  more mappings to OM than AGROVOC
% A unit slice is either horizontally or vertically oriented
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


		 /*******************************
		 *  2. ANNOTATE QUANTITY BLOCKS	*

		 *******************************/



quantity_unit_label(Label,Symbol,OMUnit):-
	atom_codes(Label,Codes),
	phrase(quantity_unit_label(Symbol,OMUnit),Codes,[]).



		 /*******************************
		 *       OM UNIT MATCHING       *
		 *******************************/

omVoc('http://www.wurvoc.org/vocabularies/om-1.8/').

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

quantity_unit_label(Symbol,OMUnit) -->
	pre_unit,bracket_open(T),unit_label(Symbol,OMUnit),bracket_close(T).

pre_unit --> pre_unit_codes(Codes),
	{atom_codes(Term,Codes),
	 atom(Term)}.

pre_unit_codes([H|T]) --> [H],pre_unit_codes(T).
pre_unit_codes([]) --> [].

bracket_open(r) --> "(".
bracket_open(s) --> "[".

bracket_close(r) --> ")".
bracket_close(s) --> "]".


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
	bracket_open(T), !,
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


		 /*******************************
	         *    ASSERT UNIT CONCEPTS      *
		 *******************************/
unit_cell_label(Block,X,Y,Label):-
	unit_cell(Block,X,Y,_,_),
	block(Block,_,DS),
	ds_inside(DS, X, Y),
	ds_sheet(DS, Sheet),
	cell_value(Sheet,X,Y,Label).

unit_label(Label):-
	findall(Label, unit_cell_label(_,_,_,Label), Labels),
	sort(Labels, Unique),
	member(Label, Unique).


count([],_,0).
count([X|T],X,Y):- count(T,X,Z), Y is 1+Z.
count([X1|T],X,Z):- X1\=X,count(T,X,Z).

countall(List,X,C):-
    sort(List,List1),
    member(X,List1),
    count(List,X,C).


unit_label_occurrence(Label,Occurrence):-
	findall(Label, unit_cell_label(_,_,_,Label), Labels),
	countall(Labels,Label,Occurrence).


assert_OM_units:-
	forall(unit_label(Label),
		forall(om_label(Label,_,OMUnit),
		       assert_OM_unit(Label,OMUnit))).
assert_OM_unit(Label,OMUnit):-
	rdf(OMUnit, sheet:omUnitOf, literal(Label),sheet_labels), !.
assert_OM_unit(Label,OMUnit):-
	rdf_assert(OMUnit, sheet:omUnitOf, literal(Label),sheet_labels).


		 /*******************************
	         *  ASSERT QUANTITY CONCEPTS	*
		 *******************************/
% Select quantities from a general application area, i.e. application
% area that does not use other areas. NB: In this case: space_and_time
label_quantity(Label,OMQuantity):-
	omVoc(OM),
	rdf(OMUnit, sheet:omUnitOf, literal(Label),sheet_labels),
	rdf(OMQuantity,om:unit_of_measure,OMUnit,OM).

unique_label_quantity(Label,OMQuantity):-
	findall(Q,label_quantity(Label,Q),QList),
	sort(QList,OMQuantities),
	member(OMQuantity,OMQuantities).

assert_label_quantities:-
	forall(unit_label(Label),
		forall(unique_label_quantity(Label,OMQuantity),
		       assert_label_quantity(Label,OMQuantity))).

assert_label_quantity(Label,OMQuantity):-
	rdf(OMQuantity, sheet:omQuantityOf, literal(Label),sheet_labels), !.
assert_label_quantity(Label,OMQuantity):-
	rdf_assert(OMQuantity, sheet:omQuantityOf, literal(Label),sheet_labels).


assert_units:-
	assert_OM_units,
	assert_label_quantities.


		 /*******************************
		  *	      REST             *
		 *******************************/



unit_block_label(Block,Label):-
	block(Block,unit,DS),
	ds_inside(DS,X,Y),
	ds_sheet(DS, Sheet),
	cell_value(Sheet,X,Y,Label).


all_unit_block_labels(LabelSet):-
	findall(Label,
		unit_block_label(_,Label),
		List),
	sort(List,LabelSet).

false_unit_label(Block,FalseLabel):-
	all_unit_block_labels(LabelSet),
	member(FalseLabel,LabelSet),
	\+unit_label(FalseLabel),
	unit_block_label(Block,FalseLabel).


