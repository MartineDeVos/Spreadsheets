% Assumption1 : When there is a Data block, there is an accompanying
% Unit block
% Assumption2: The Unit block is always located right_of or above
% datablock


% Localize datablock that accompanies a unitblock; float blocks
% below/right_of unitblock might be datablocks, but do not accompany
% that unitblock
unitblock_adj_datablock(DSUnit,Rel,DSData):-
	block(_UnitBlock,unit, DSUnit),
	block(_DataBlock,float, DSData),
	ds_adjacent(DSUnit,Rel,DSData),
	\+ Rel == below,
	\+ Rel == right_of.

% Assumption3: When there is a Unit block, there is an accompanying Quantity block
% Assumption4: The Quantity Block is always located adjacent (above/left_of) to the Unit block
quantityblock_adj_unitblock(DSUnit,RelU,DSData,RelQ,DSQuantity):-
	unitblock_adj_datablock(DSUnit,RelU,DSData),
	block(_UnitBlock, unit, DSUnit),
	block(_QuantityBlock, string, DSQuantity),
	ds_adjacent(DSQuantity,RelQ,DSUnit),
	RelQ = RelU.

% Assumption5: When there is a Quantity block, there is an accompanying
% Phenomena block
% Assumption6: The Phenomena block is always located adjacent(above or
% left) to the Data block AND across from the Quantity Block

phenomenablock_adj_datablock(DSUnit,RelU,DSData,RelP,DSPhenomena):-
	unitblock_adj_datablock(DSUnit,RelU,DSData),
	block(_PhenomenaBlock, string, DSPhenomena),
	block(_DataBlock, float, DSData),
	ds_adjacent(DSPhenomena,RelP,DSData),
	(   RelU == left_of
	->  RelP = above
	;   RelU == above
	->  RelP = left_of).


		 /*******************************
		 * CHARACTERIZING MANUAL BLOCKS	*

		 *******************************/
man_unitblock_adj_datablock(DSUnit,Rel,DSData):-
	manual_block(DSUnit,Sheet,unit,_),
	manual_block(DSData,Sheet,float,_),
	ds_adjacent(DSUnit,Rel,DSData),
	\+ Rel == below,
	\+ Rel == right_of.

man_quantityblock_adj_unitblock(DSUnit,RelU,DSData,RelQ,DSQuantity):-
	man_unitblock_adj_datablock(DSUnit,RelU,DSData),
	manual_block(DSUnit,Sheet,unit,_),
	manual_block(DSQuantity,Sheet,string,_),
	ds_adjacent(DSQuantity,RelQ,DSUnit),
	RelQ = RelU.

man_phenomenablock_adj_datablock(DSUnit,RelU,DSData,RelP,DSPhenomena):-
	man_unitblock_adj_datablock(DSUnit,RelU,DSData),
	manual_block(DSPhenomena,Sheet,string,_),
	manual_block(DSData,Sheet,float,_),
	ds_adjacent(DSPhenomena,RelP,DSData),
	(   RelU == left_of
	->  RelP = above
	;   RelU == above
	->  RelP = left_of).
