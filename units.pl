:-  use_module(library(dcg/basics)).
:- ensure_loaded(roadmap).
:-  rdf_register_prefix(om,'http://www.wurvoc.org/vocabularies/om-1.8/').


		 /*******************************
		 *       OM UNIT MATCHING       *
		 *******************************/

omVoc('file:///home/mvs246/src/ClioPatria/edesign/CPB_Microtax/OMVocabulary.owl').

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
	rdf(OMUnit,om:symbol,literal(Symbol),OM),!.
unit_symbol_match(Symbol,OMUnit):-
	omVoc(OM),
	rdf(OMUnit,om:alternative_symbol,literal(Symbol),OM).


unit_description_match(Symbol,OMUnit):-
	omVoc(OM),
        rdf(OMUnit,rdfs:label,literal(Symbol),OM),
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

prefix -->[].
prefix -->[C], { code_type(C, alpha) }.

unit(Symbol,OMUnit) -->
	 \+ "in",
	 \+ "per",
	 \+ "gas",
	 \+ "en",
	 \+ "op",
	prefix,symbol(Symbol,OMUnit),sep.

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
		(   unit_slice_cell(Block,Slice,_,_),
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


unit_slice_cell(Block,Slice,X,Y):-
	unit_cell(Block,X,Y,_,_),
	cell_property(Sheet,X,Y, block(Block)),
	block(Block,_,DS),
	ds_column_slice(DS,_,cell_range(Sheet,X,SY, X,EY)),
	aggregate_all(count,Y2,
		      (between(SY,EY,Y2), unit_cell(Sheet,X,Y2,_,_)),
		      UnitInCol),
	ds_row_slice(DS,_,cell_range(Sheet,SX,Y, EX,Y)),
	aggregate_all(count,X2,
		      (between(SX,EX,X2), unit_cell(Sheet,X2,Y,_,_)),
		      UnitInRow),
	(   UnitInRow > UnitInCol
	->  Slice = cell_range(Sheet, SX,Y, EX,Y)
	;   Slice = cell_range(Sheet, X,SY, X,EY)
	).



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


		 /*******************************
	         *  ASSERT APPLICATION AREA	*
		 *******************************/
label_quantity_area(Label,OMQuantity,OMArea):-
	omVoc(OM),
	rdf(OMQuantity, sheet:omQuantityOf, literal(Label),sheet_labels),
	rdf(OMArea,om:quantity,OMQuantity,OM).

%unique_label_area(Label,OMArea):-
%	unit_label(Label),
%	findall(A,label_quantity_area(Label,_,A),AList),
%	sort(AList,OMAreas),
%	member(OMArea,OMAreas).

assert_label_areas:-
	forall(unit_label(Label),
		forall(label_quantity_area(Label,_,OMArea),
		       assert_label_area(Label,OMArea))).

assert_label_area(Label,OMArea):-
	rdf(OMArea, sheet:omAreaOf, literal(Label),sheet_labels), !.
assert_label_area(Label,OMArea):-
	rdf_assert(OMArea, sheet:omAreaOf, literal(Label),sheet_labels).

sheet_areas(OMAreas):-
	findall(A,rdf(A, sheet:omAreaOf,_,sheet_labels),AList),
	sort(AList,OMAreas).


area_label_coverage(OMArea,NumLabels):-
	aggregate_all(count,
	      (	  unit_cell_label(_,_,_,Label),
		  rdf(OMArea, sheet:omAreaOf,literal(Label),sheet_labels)),
	      NumLabels).

best_area(BestArea,Pairs2):-
	sheet_areas(OMAreas),
	findall(NumLabels-OMArea,
		(    member(OMArea,OMAreas),
		     area_label_coverage(OMArea,NumLabels)
		),
		Pairs0),
	keysort(Pairs0,Pairs1),
	reverse(Pairs1,Pairs2),
	max_member(_-BestArea,Pairs2).


assert_units:-
	assert_OM_units,
	assert_label_quantities,
	assert_label_areas.

%
%best_area(OMArea):-
%	sheet_areas(OMAreas),
%	aggregate_all(max(NumLabels),
%		      (	  member(OMArea,OMAreas),
%			  area_label_coverage(OMArea,NumLabels)),
%		      MaxLabels),
%	area_label_coverage(OMArea,MaxLabels).


% Find a two cells in a unit block that have different symbols,
% but are associated to the same quantity
% block_shared_quantity(Block,Symbol,Symbol2,OMQuantity):-
%	block(Block,unit,_),
%	unit_cell(Block,X,Y,Symbol,OMUnit),
%	unit_cell(Block,X2,Y2,Symbol2,OMUnit2),
%	\+ (X2 == X, Y2 == Y),
%	\+ Symbol2 == Symbol,
%	rdf(OMQuantity,om:unit_of_measure,OMUnit),
%	rdf(OMQuantity,om:unit_of_measure,OMUnit2).
%
%block_shared_quantity(Block,Symbol,Symbol2,OMQuantity):-
%	block(Block,unit,_),
%	unit_cell(Block,X,Y,Symbol,OMUnit),
%	unit_cell(Block,X2,Y2,Symbol2,OMUnit2),
%	\+ (X2 == X, Y2 == Y),
%	\+ Symbol2 == Symbol,
%	rdf(OMQuantity1,om:unit_of_measure,OMUnit),
%	rdf(OMQuantity2,om:unit_of_measure,OMUnit2).



%quantity_unit(Block,Ancestor,Descendants,NumDesc):-
%	block_ancestors(Block,AncestorSet),
%	member(Ancestor, AncestorSet),
%	findall(Label,
%		block_label_ancestor(Block,Label,Ancestor),
%		DescList),
%	sort(DescList,Descendants),
%	length(Descendants,NumDesc).
%
%block_best_quantity(Block,BestAncestor):-
%	block(Block,unit,_),
%	findall(NumDesc-Ancestor,
%		block_descendants(Block,Ancestor,_,NumDesc)
%	       ,Pairs2),
%	keysort(Pairs2,Pairs1),
%	reverse(Pairs1,Pairs),
%	max_member(_-BestAncestor,Pairs).

%block_slice(Block,Slice):-
%	findall(B-S,
%	       block_slice_cell(B,S,_,_),
%	       BSList),
%	sort(BSList,BSSet),
%	member([Block-Slice],BSSet).




%cell_in_colslice( cell_range(Sheet, CX,SY, CX,EY),X,Y):-
%	cell_property(Sheet,X,Y, block(Block)),
%	block(Block,_,DS),
%	ds_column_slice(DS,_Offset,cell_range(Sheet,CX,SY, CX,EY)),
%	between(SY, EY, Y),
%	CX is X.
%
%cell_in_rowslice( cell_range(Sheet, SX,RY, EX,RY),X,Y):-
%	cell_property(Sheet,X,Y, block(Block)),
%	block(Block,_,DS),
%	ds_row_slice(DS,_Offset,cell_range(Sheet, SX,RY, EX,RY)),
%	between(SX, EX, X),
%	RY is Y.
%
%colslice_sibling(cell_range(Sheet, CX,SY, CX,EY),X,Y,Y2):-
%	cell_in_colslice( cell_range(Sheet, CX,SY, CX,EY),X,Y),
%	between(SY, EY, Y2),
%	\+ Y2 == Y,
%	unit_cell(Sheet,X,Y2,_OMUnit).
%
%rowslice_sibling(cell_range(Sheet, SX,RY, EX,RY),X,X2,Y):-
%	cell_in_rowslice( cell_range(Sheet, SX,RY, EX,RY),X,Y),
%	between(SX, EX, X2),
%	\+ X2 == X,
%	unit_cell(Sheet,X2,Y,_OmConcept).
