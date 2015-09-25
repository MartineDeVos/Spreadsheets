:- ensure_loaded("../plsheet/test").


% Run once per ods file to derive initial separation of float and string blocks
init(File):-
	unload_file(File),
	retractall(block(_,_,_)),
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
	max_block(Sheet,float,DS),!,
	assert_table_body(Sheet,DS,_),
	assert_body(Sheet).
assert_body(_):-
	true.

assert_table_body(Sheet,DS,Body):-
	table_body(Sheet,DS,Body),
	forall(block_intersect(Sheet,float,Body,Intersect),
			      retract_block(Intersect)
	      ),
	ds_id(Body,Id,block),
	assert_block(block(Id,body,Body)).

% table_body(+Sheet,+DS,-TableBody)
% from an initial block of a spec. type, build table body by
% recursively merging with largest close blocks of that same type
table_body(Sheet,DS,TableBody):-
	merge_float_blocks(Sheet,DS,Union), !,
	table_body(Sheet,Union,TableBody).
table_body(_,DS,DS).


% max_type_block(+Sheet,-DS)
% finds a specific type of block with largest cellcount
max_block(Sheet,Type,DS):-
	blocks_by_size(Sheet,Type,BySize),
	last(BySize,_-DS).

blocks_by_size(Sheet,Type,BySize):-
	findall(Count-DS,
		(   ds_sheet(DS,Sheet),
		    block(_,Type,DS),
		    ds_cell_count(DS,Count)),
	        Pairs),
	sort(Pairs,Unique),
	keysort(Unique,BySize).



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
	max_close_block(Sheet,float,DS,Adjacent),
	ds_union(DS,Adjacent,Union),
	\+label_inside_ds(Sheet,Union).


% close_block(+Sheet,+Type,+DS,-Close)
% finds block of a spec. type adjacent or intersecting with DS
close_block(Sheet,Type,DS,Close):-
	ds_sheet(Close,Sheet),
	block(_,Type,Close),
	(   ds_close_vertical(DS,Close);
	    ds_close_horizontal(DS,Close);
	    ds_intersection(DS,Close,_)
	 ),
	\+ds_within(DS,Close),
	\+DS==Close.

% max_close_block(+Sheet,+DS,-Close)
% finds close (to DS) float block with largest cellcount
max_close_block(Sheet,Type,DS,Close):-
	ds_sheet(DS,Sheet),
	findall(Count-Close,
		(  close_block(Sheet,Type,DS,Close),
		   ds_cell_count(Close,Count)),
	        Pairs),
	sort(Pairs,Unique),
	keysort(Unique,BySize),
	last(BySize,Count-Close).



		 /*******************************
		 *	  CONTEXT BLOCKS	*
		 *******************************/
% From an initial (largest) string block, build and assert context
% blocks,i.e., string blocks surrounding the table body
% NB:all string blocks that are inside or intersecting with the
% context blocks are removed
assert_context(Sheet):-
	blocks_by_size(Sheet,string,BySize),
	reverse(BySize,MaxFirst),
	forall(member(_-DS,MaxFirst),
	       assert_context_block(Sheet,DS)
	      ).

assert_context_block(Sheet,DS):-
	context_candidate(Sheet,DS,Candidate),
	tailor_context(Sheet,Candidate,Tailored),
	assert_context_block2(Sheet,Tailored).
assert_context_block(_,_).

assert_context_block2(Sheet,Candidate):-
	forall(block_intersect(Sheet,string,Candidate,Intersect),
			      retract_block(Intersect)
	      ),
	ds_id(Candidate,Id,block),
	assert_block(block(Id,context,Candidate)).

% tailor_context(+Sheet,+Context,-Tailored)
% Align context block with table body, and force width (top-aligned)
% or height (left/right aligned) of contxt to be the same as the body
tailor_context(Sheet,Context,Tailored):-
	block(_,body,Body),
	Body    = cell_range(Sheet, SX1, SY1, EX1, EY1),
	Context = cell_range(Sheet, SX2, SY2, EX2, EY2),
	(   ds_top_aligned(Sheet,Body,Context)
	->  Tailored = cell_range(Sheet, SX1, SY2, EX1, EY2)
	;   ds_left_aligned(Sheet,Body,Context)
	->     Tailored = cell_range(Sheet, SX2, SY1, EX2, EY1)
	;   ds_right_aligned(Sheet,Body,Context)
	->     Tailored = cell_range(Sheet, SX2, SY1, EX2, EY1)
	).

context_candidate(Sheet,DS,Candidate):-
	merge_string_blocks(Sheet,DS,Union), !,
	context_candidate(Sheet,Union,Candidate).
context_candidate(_,DS,DS).


merge_string_blocks(Sheet,DS,Union):-
	max_close_block(Sheet,string,DS,Adjacent),
	ds_union(DS,Adjacent,Union),
	\+float_inside_ds(Sheet,Union).

float_inside_ds(Sheet,DS):-
	ds_inside(DS, X, Y),
	cell_value(Sheet,X,Y,Value),
	float(Value).



		 /*******************************
		 * DATASOURCE SPATIAL RELATIONS *
		 *******************************/

% block_intersect(+Sheet,+DS1, +Block2)
% true when DS1 and DS2 intersect.
% NB:returns the blockID of DS2 instead of the cell_range
block_intersect(Sheet,Type,DS,Intersect):-
	ds_sheet(IntersectDS,Sheet),
        block(Intersect,Type,IntersectDS),
	ds_intersection(DS,IntersectDS,_).


% ds_close_vertical(+DS, +Close)
% true when close block is max 1 row apart from DS
% and on the same horizontal level
ds_close_vertical(cell_range(Sheet, SX1,SY1, EX1,EY1),
	  cell_range(Sheet, SX2,SY2,_,EY2)):-
	between(SX1,EX1,SX2),
	(   SY2 >= EY1,
	    SY2 - EY1 =< 2
	;
	    SY1 >= EY2,
	    SY1 - EY2 =<2).

% ds_close_horizontal(+DS, +Close)
% true when close block is max 1 column apart from DS
% and on the same vertical level
ds_close_horizontal(cell_range(Sheet, SX1,SY1, EX1,EY1),
	  cell_range(Sheet, SX2,SY2,EX2,_)):-
	between(SY1,EY1,SY2),
	(   SX2 >= EX1,
	    SX2 - EX1 =< 2
	;
	    SX1 >= EX2,
	    SX1 - EX2 =<2).

% ds_within(+DS1, +DS2)
% true when the DS2 is within DS1.
ds_within(cell_range(Sheet, SX1,SY1, EX1,EY1),
	  cell_range(Sheet, SX2,SY2, EX2,EY2)):-
	SX1 =< SX2,
	EX2 =< EX1,
	SY1 =< SY2,
	EY2 =< EY1.


% ds_top_aligned(+Sheet,+Body,+Context)
% true if Body and Context have the same width and
% if there are not more than 1 row separating the two
ds_top_aligned(Sheet,Body,Context):-
	Body    = cell_range(Sheet, SX1, SY1, EX1,_),
	Context = cell_range(Sheet, SX2, _, EX2, EY2),
	SX1=:=SX2,
	EX1=:=EX2,
	SY1 > EY2,
	SY1 - EY2 =< 2.

% ds_left_aligned(+Sheet,+Body,+Context)
% true if Body and Context have the same height and
% if there are not more than 1 column separating the two
ds_left_aligned(Sheet,Body,Context):-
	Body    = cell_range(Sheet, SX1, SY1, _,EY1),
	Context = cell_range(Sheet, _, SY2, EX2, EY2),
	SY1=:= SY2,
	EY1 =:= EY2,
	SX1 > EX2,
	SX1 - EX2 =< 2.


% ds_right_aligned(+Sheet,+Body,+Context)
% true if Body and Context have the same height and
% if there are not more than 1 colum separating the two
ds_right_aligned(Sheet,Body,Context):-
	Body    = cell_range(Sheet, _, SY1, EX1,EY1),
	Context = cell_range(Sheet, SX2, SY2, _, EY2),
	SY1=:= SY2,
	EY1 =:= EY2,
	SX2 > EX1,
	SX2 - EX1 =< 2.






