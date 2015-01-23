
unit_cell2(Block,X,Y,Symbol,OMUnit):-
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


ds_map_check(Block,cell_range(Sheet, SX,SY, EX,EY),UnitMap,TextMap):-
	aggregate_all(count,X-Y,
		      (ds_inside(cell_range(Sheet, SX,SY, EX,EY),X,Y),
		       unit_cell2(Block,X,Y,_,_)),
		      UnitMap),
	aggregate_all(count,X-Y,
		      (ds_inside(cell_range(Sheet, SX,SY, EX,EY),X,Y),
		       text_cell(Block,X,Y,_,_)),
		      TextMap),
	UnitMap > TextMap.


% Assumption: a unit slice contains more mappings to OM than AGROVOC,
% and this can only be true for either horizontal or vertical slice.
unit_slice(Block,ColSlice):-
	unit_cell2(Block,X,Y,_,_),
	cell_property(_,X,Y, block(Block)),
	block(Block,_,DS),
	ds_column_slice(DS,_,cell_range(Sheet,X,SY, X,EY)),
	ds_map_check(Block,cell_range(Sheet,X,SY, X,EY)),
	ColSlice = cell_range(Sheet,X,SY, X,EY),!.
unit_slice(Block,RowSlice):-
	unit_cell2(Block,X,Y,_,_),
	cell_property(_,X,Y, block(Block)),
	block(Block,_,DS),
	ds_row_slice(DS,_,cell_range(Sheet,SX,Y, EX,Y)),
	ds_map_check(Block,cell_range(Sheet, SX,Y, EX,Y)),
	RowSlice = cell_range(Sheet, SX,Y, EX,Y).


