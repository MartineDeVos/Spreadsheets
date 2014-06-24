% Assumption1 : When there is a Data block, there is an accompanying
% Unit block
% Assumption2: The Unit block is always located right_of or above
% datablock


% Localize datablock that accompanies a unitblock; float blocks below/right_of unitblock
% might be datablocks, but do not accompany that unitblock
unitblock_adj_datablock(UnitBlock,Rel,DataBlock):-
	% Replace ntwo lines with new concept unitblock
	unit_cell(Sheet,X,Y,_OMConcept),
	cell_property(Sheet,X,Y, block(UnitBlock)),
	block(UnitBlock, string, DSUnit),
	block(DataBlock, float, DSData),
	ds_adjacent(DSUnit,Rel,DSData),
	\+ Rel == below,
	\+ Rel == right_of.

% Assumption3: When there is a Unit block, there is an accompanying
% Quantity block
% Assumption4: The Quantity Block is always located adjacent (above/left_of) to the Unit block
quantityblock_adj_unitblock(QuantityBlock,Rel,UnitBlock):-
	unitblock_adj_datablock(UnitBlock,Rel,_DataBlock),
	block(UnitBlock, string, DSUnit),
	block(QuantityBlock, string, DSQuantity),
	% NB If UnitBlock is left_of DataBlock, then QuantityBlock is left_of UnitBlock
	ds_adjacent(DSQuantity,Rel,DSUnit).

% Assumption5: When there is a Quantity block, there is an accompanying
% Phenomena block
% Assumption6: The Phenomena block is always located adjacent(above or
% left) to the Data block AND across from the Quantity Block

phenomenablock_adj_datablock(PhenomenaBlock,Rel2,UnitBlock):-
	unitblock_adj_datablock(UnitBlock,Rel1,DataBlock),
	block(PhenomenaBlock, string, DSPhenomena),
	block(DataBlock, float, DSData),
	(   Rel1 == left_of
	->  Rel2 = above
	;   Rel2 = left_of),
	ds_adjacent(DSPhenomena,Rel2,DSData).


% Script is working, but many blocks are missed. Empty cells result in
% non-continuous blocks, and as a consequence 'adjacent' is false.



unitblock_concept(UnitBlock,OMConcept):-
	block(UnitBlock,_,DS),
	ds_inside(DS,X,Y),
	unit_cell(_, X,Y,OMConcept).
