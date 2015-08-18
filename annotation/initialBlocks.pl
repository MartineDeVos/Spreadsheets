:- ensure_loaded("../plsheet/test").


% Run once per ods file to derive initial separation of float and string blocks
init(File):-
	ods_unload,
	load(File),
	segment.


		 /*******************************
		 *	     TABLE BODY		*
		 *******************************/

% max_float_block(+Sheet,-DS) is det.
% finds float block with largest cellcount
max_float_block(Sheet,DS):-
	sheet(Sheet,_),
	findall(Count-DS,
		(   sheet_object(Sheet,block,block(_,float,DS)),
		    ds_cell_count(DS,Count)),
	        Pairs),
	sort(Pairs,Unique),
	keysort(Unique,BySize),
	last(BySize,Count-DS).


% max_adjacent_block(+Sheet,+DS,-Adjacent)is det.
% finds adjacent (to initial block) float block with largest cellcount
max_adjacent_block(Sheet,DS,Adjacent):-
	sheet(Sheet,_),
	findall(Count-Adjacent,
		(   block(_,float,Adjacent),
		    ds_adjacent(DS,_,Adjacent),
		    ds_cell_count(Adjacent,Count)),
	        Pairs),
	sort(Pairs,Unique),
	keysort(Unique,BySize),
	last(BySize,Count-Adjacent).

% label_inside_block(+Sheet,+DS) is det.
% true when there is a cell with a string
label_inside_block(Sheet,DS):-
	cell_value(Sheet,X,Y,Value),
	atom(Value),
	ds_inside(DS, X, Y).

% merge_float_blocks(+Sheet,+DS,-Union) is det.
% unify initial (largest) float block with largest adjacent float block
% NB union may also contain cells that are not part of the float blocks;
% empty cells are allowed, string cells not
merge_float_blocks(Sheet,DS,Union):-
	max_adjacent_block(Sheet,DS,Adjacent),
	ds_union(DS,Adjacent,Union),
	\+label_inside_block(Sheet,Union).


% table_body(+Sheet,+DS,-TableBody) is det.
% from an initial (largest) float block, build table body by
% recursively merging with largests adjacent float blocks
table_body(Sheet,DS,TableBody):-
	merge_float_blocks(Sheet,DS,Union), !,
	table_body(Sheet,Union,TableBody).
table_body(_,DS,DS).


