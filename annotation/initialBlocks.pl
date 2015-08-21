:- ensure_loaded("../plsheet/test").


% Run once per ods file to derive initial separation of float and string blocks
init(File):-
	ods_unload,
	load(File),
	segment.


		 /*******************************
		 *	     TABLE BODY		*
		 *******************************/

% From an initial (largest) float block, build and assert table
% body,i.e., the rectangle with quantitative observations, for every
% table in a sheet
% NB:all float blocks that are inside or intersecting with the table
% bodies are removed
assert_body(Sheet):-
	max_float_block(Sheet,DS),!,
	assert_table_body(Sheet,DS,_),
	assert_body(Sheet).
assert_body(_):-
	true.

assert_table_body(Sheet,DS,Body):-
	table_body(Sheet,DS,Body),
	forall(block_intersect(Sheet,Body,Intersect),
			      retract_block(Intersect)
	      ),
	ds_id(Body,Id,block),
	assert_block(block(Id,body,Body)).

% table_body(+Sheet,+DS,-TableBody)
% from an initial float block, build table body by
% recursively merging with largest close float blocks
table_body(Sheet,DS,TableBody):-
	merge_float_blocks(Sheet,DS,Union), !,
	table_body(Sheet,Union,TableBody).
table_body(_,DS,DS).



% max_float_block(+Sheet,-DS)
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

% block_intersect(+Sheet,+DS1, +Block2)
% true when DS1 and DS2 intersect.
% NB:returns the blockID of DS2 instead of the cell_range
block_intersect(Sheet,DS,Intersect):-
	ds_sheet(IntersectDS,Sheet),
        block(Intersect,float,IntersectDS),
	ds_intersection(DS,IntersectDS,_).

% label_inside_block(+Sheet,+DS)
% true when there is a cell with a string
label_inside_ds(Sheet,DS):-
	ds_inside(DS, X, Y),
	cell_value(Sheet,X,Y,Value),
	atom(Value).


% merge_float_blocks(+Sheet,+DS,-Union)
% unify initial (largest) float block with largest adjacent float block
% NB union may also contain cells that are not part of the float blocks;
% empty cells are allowed, string cells not
merge_float_blocks(Sheet,DS,Union):-
	max_close_block(Sheet,DS,Adjacent),
	ds_union(DS,Adjacent,Union),
	\+label_inside_ds(Sheet,Union).


% close_block(+Sheet,+DS,-Close)
% finds float block adjacent or intersecting with DS
close_block(Sheet,DS,Close):-
	ds_sheet(Close,Sheet),
	block(_,float,Close),
	(	ds_adjacent(DS,_,Close);
		ds_intersection(DS,Close,_)
	 ),
	\+ds_within(DS,Close),
	\+DS==Close.

% max_close_block(+Sheet,+DS,-Close)
% finds close (to DS) float block with largest cellcount
max_close_block(Sheet,DS,Close):-
	sheet(Sheet,_),
	findall(Count-Close,
		(  close_block(Sheet,DS,Close),
		   ds_cell_count(Close,Count)),
	        Pairs),
	sort(Pairs,Unique),
	keysort(Unique,BySize),
	last(BySize,Count-Close).

% ds_within(+DS1, +DS2)
% true when the DS2 is within DS1.
ds_within(cell_range(Sheet, SX1,SY1, EX1,EY1),
	  cell_range(Sheet, SX2,SY2, EX2,EY2)):-
	SX1=<SX2,
	EX2=<EX1,
	SY1=<SY2,
	EY2=<EY1.







