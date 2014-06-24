

row_slice_inside( cell_range(_Sheet, SX,RY, EX,RY),X,Y):-
	between(SX, EX, X),
	Y is RY.


column_slice_inside( cell_range(_Sheet, CX,SY, CX,EY),X,Y):-
	between(SY, EY, Y),
	X is CX.


label_inside_row_slice(Label,Label2,cell_range(Sheet, SX,Y, EX,Y)):-
	cell_value(Sheet,X,Y,Label),
	cell_property(Sheet,X,Y, block(Block)),
	block(Block, string, cell_range(Sheet, SX,_SY, EX,_EY)),
	row_slice_inside(cell_range(Sheet, SX,Y, EX,Y),X2,Y),
	\+ X2 == X,
	cell_value(Sheet, X2, Y, Label2).

row_unit_siblings(Block,Siblings,RowSlice):-

	findall(RowLabel,
		(   label_inside_row_slice(Label,RowLabel,RowSlice),
		    unit_label(RowLabel,_)
		),
		Siblings).


column_unit_siblings(Block,Siblings,ColSlice):-
	findall(ColLabel,
		(   label_inside_column_slice(Label,ColLabel,ColSlice),
		    unit_label(ColLabel,_)
		),
		Siblings).


label_inside_column_slice(Label,Label2,cell_range(Sheet, X,SY, X,EY)):-
	cell_value(Sheet,X,Y,Label),
	cell_property(Sheet,X,Y, block(Block)),
	block(Block, string, cell_range(Sheet, _SX,SY, _EX,EY)),
	column_slice_inside(cell_range(Sheet, X,SY, X,EY),X,Y2),
	\+ Y2 == Y,
	cell_value(Sheet, X, Y2, Label2).

unit_slice(Block,Slice):-
	row_unit_siblings(Label,RowSiblings,RowSlice),
	column_unit_siblings(Label,ColSiblings,ColSlice),
	length(ColSiblings,ColLength),
	length(RowSiblings,RowLength),
	(   RowLength > ColLength
	->  Slice = RowSlice
	;   Slice = ColSlice
	).


unit_slice_block(Slice,Block):-
	% Block contains unit label
	label_block_unit(Label,Block,_),
	% Unit is present in slice
	unit_inside_slice(Label,Slice).





%label_column_slice(Label,Offset,cell_range(Sheet, X,SY, X,EY)):-
%	cell_value(Sheet,X,Y,Label),
%	cell_property(Sheet,X,Y, block(Block)),
%	block(Block, string, cell_range(Sheet, SX,SY, EX,EY)),
%	ds_column_slice(cell_range(Sheet, SX,SY, EX,EY)
%		       , Offset,
%			cell_range(Sheet, X,SY, X,EY)).
%
%label_row_slice(Label,Offset,cell_range(Sheet, SX,Y, EX,Y)):-
%	cell_value(Sheet,X,Y,Label),
%	cell_property(Sheet,X,Y, block(Block)),
%	block(Block, string, cell_range(Sheet, SX,SY, EX,EY)),
%	ds_row_slice(cell_range(Sheet, SX,SY, EX,EY)
%		       , Offset,
%			cell_range(Sheet, SX,Y, EX,Y)).
