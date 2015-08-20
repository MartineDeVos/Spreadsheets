:-  use_module(library(dcg/basics)).
:- ensure_loaded(roadmap).
:- rdf_register_prefix(om,'http://www.wurvoc.org/vocabularies/om-1.8/').



		 /*******************************
		 *       OM UNIT MATCHING       *
		 *******************************/

omVoc('http://www.wurvoc.org/vocabularies/om-1.8/').

get_om_symbol(Symbol,OMUnit):-
	\+ atom_number(Symbol,_),
	(  unit_symbol_match(Symbol,OMUnit)
	;  unit_description_match(Symbol,OMUnit)
	).

%unit_symbol_match(Symbol,OMUnit):-
%	omVoc(OM),
%	(  rdf(OMUnit,om:symbol,literal(Symbol),OM)
%	;  rdf(OMUnit,om:alternative_symbol,literal(Symbol),OM)).


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

unit_label(Symbol,OMUnit) -->
	pre_term,unit(Symbol,OMUnit),post_term.

pre_term -->[].
pre_term --> term,white,whites.

term_codes([H|T]) --> [H], { \+ code_type(H, white) },
term_codes(T).
term_codes([]) --> [].

term -->term_codes(Codes),
	{atom_codes(Term,Codes),
	 atom(Term)}.

%prefix --> [].
%prefix --> "P".
%prefix --> "G".
%prefix --> "M".
%prefix --> "T".

sep --> \+ [_].
sep --> [C], {\+ code_type(C, alpha) }.
sep -->	" ".
sep --> "/".

pre_fix -->[].
pre_fix -->[C], { code_type(C, alpha) }.

unit(Symbol,OMUnit) -->
	 \+ "in",
	 \+ "per",
	 \+ "gas",
	 \+ "en",
	 \+ "op",
	pre_fix,symbol(Symbol,OMUnit),sep.

% symbol_codes([H|T]) --> [H], { \+ code_type(H, white) },
% symbol_codes(T).
symbol_codes([H|T]) --> [H],symbol_codes(T).
symbol_codes([]) --> [].

symbol(Symbol,OMUnit)-->
	om_symbol(Symbol,OMUnit).

om_symbol(Symbol,OMUnit)-->
	symbol_codes(Codes),
	{ atom_codes(Symbol,Codes),
	  get_om_symbol(Symbol,OMUnit)
	  }.


post_term -->[].
post_term --> term.

		 /*******************************
		 *   DISTINGUISH UNIT BLOCKS	*
		 *******************************/

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

is_om_label(Label) :-
	om_label(Label,_,_), !.

unit_cell(Block,X,Y,Symbol,OMUnit):-
	block(Block,_,DS),
	ds_inside(DS, X, Y),
	ds_sheet(DS, Sheet),
	cell_value(Sheet,X,Y,Label),
	om_label(Label,Symbol,OMUnit).

text_cell(Block,X,Y,Label,AgroConcept):-
	block(Block,_,DS),
	ds_inside(DS, X, Y),
	ds_sheet(DS, Sheet),
	cell_value(Sheet,X,Y,Label),
	label_agro_concept(Label,_, AgroConcept).

% Check whether suggested slice contains more mappings to OM than AGROVOC,
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


