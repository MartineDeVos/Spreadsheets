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
	findall(Count-DS,
		(   ds_sheet(DS,Sheet),
		    block(_,float,DS),
		    ds_cell_count(DS,Count)),
	        Pairs),
	sort(Pairs,Unique),
	keysort(Unique,BySize),
	last(BySize,Count-DS).

close_block(Sheet,DS,Close):-
	ds_sheet(Close,Sheet),
	block(_,float,Close),
	(	ds_adjacent(DS,_,Close);
		ds_intersection(DS,Close,_)
	 ),
	\+DS==Close.


% max_adjacent_block(+Sheet,+DS,-Adjacent)is det.
% finds adjacent (to initial block) float block with largest cellcount
max_close_block(Sheet,DS,Close):-
	sheet(Sheet,_),
	findall(Count-Close,
		(  close_block(Sheet,DS,Close),
		   ds_cell_count(Close,Count)),
	        Pairs),
	sort(Pairs,Unique),
	keysort(Unique,BySize),
	last(BySize,Count-Close).

block_intersect(Sheet,DS,Intersect):-
	ds_sheet(IntersectDS,Sheet),
        block(Intersect,float,IntersectDS),
	ds_intersection(DS,IntersectDS,_).

% label_inside_block(+Sheet,+DS) is det.
% true when there is a cell with a string
label_inside_ds(Sheet,DS):-
	ds_inside(DS, X, Y),
	cell_value(Sheet,X,Y,Value),
	atom(Value).


% merge_float_blocks(+Sheet,+DS,-Union) is det.
% unify initial (largest) float block with largest adjacent float block
% NB union may also contain cells that are not part of the float blocks;
% empty cells are allowed, string cells not
merge_float_blocks(Sheet,DS,Union):-
	max_close_block(Sheet,DS,Adjacent),
	ds_union(DS,Adjacent,Union),
	\+label_inside_ds(Sheet,Union).


% table_body(+Sheet,+DS,-TableBody) is det.
% from an initial (largest) float block, build table body by
% recursively merging with largests adjacent float blocks
table_body(Sheet,DS,TableBody):-
	merge_float_blocks(Sheet,DS,Union), !,
	table_body(Sheet,Union,TableBody).
table_body(_,DS,DS).


assert_table_body(Sheet,DS,Body):-
	table_body(Sheet,DS,Body),
	forall(block_intersect(Sheet,Body,Intersect),
			      retract_block(Intersect)
	      ),
	ds_id(Body,Id,block),
	assert_block(block(Id,body,Body)).

assert_body(Sheet):-
	max_float_block(Sheet,DS),!,
	assert_table_body(Sheet,DS,_),
	assert_body(Sheet).
assert_body(_):-
	true.
