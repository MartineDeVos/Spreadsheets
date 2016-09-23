:- rdf_meta (my_manual_blockterm(r,r,o,r)).
:- ensure_loaded(annotateBlocks).
:- ensure_loaded(annotateTables).
:- ensure_loaded(manualAnnotatedBlocks).



		 /***********************************
		       *  EVALUATE BLOCKTERMS *
		  **********************************/
block_term_agree(AutoBlock,ManualBlockRange,Concept):-
	my_manual_blockterm(Concept,sheet:blockTermOf,ManualBlockId,ground_truth),
	block_id_cell_range(ManualBlockId,ManualBlockRange),
	block_overlap(AutoBlock,ManualBlockRange,phenomenon),
        sheet_block_label(_, AutoBlock, phenomenon, Label),
	rdf(Concept, sheet:blockTermOf, literal(Label), sheet_labels).

evaluate_block_terms(AutoBlock,ManualBlockRange,AutoName,ManualName):-
	my_manual_blockterm(ManualConcept,sheet:blockTermOf,ManualBlockId,ground_truth),
	block_id_cell_range(ManualBlockId,ManualBlockRange),
	block_overlap(AutoBlock,ManualBlockRange,phenomenon),
        sheet_block_label(_, AutoBlock, phenomenon, Label),
	rdf(AutoConcept, sheet:blockTermOf, literal(Label), sheet_labels),
	domain_pref_label(AutoConcept,AutoName),
	domain_pref_label(ManualConcept,ManualName).


autoblock_blockterm(Block,BlockTerm):-
	rdf(Concept, sheet:blockTermOf, literal(Label), sheet_labels),
	sheet_block_label(_,Block, phenomenon, Label),
	domain_pref_label(Concept,BlockTerm).


		 /***********************************
	              *  EVALUATE BLOCKS *
		  **********************************/
block_overlap(AutoBlock,ManualBlock,Type):-
	sheet_object(Sheet, block, block(AutoBlock,Type,_)),
	manual_block(ManualBlock,Sheet,Type),
	(   union_overlap(AutoBlock,ManualBlock,Type)
	->   true
	;   side_overlap(AutoBlock,ManualBlock,Type)
	).

union_overlap(AutoBlock,ManualBlock,Type):-
	sheet_object(Sheet, block, block(AutoBlock,Type,AutoDS)),
	manual_block(ManualBlock,Sheet,Type),
	ds_union(ManualBlock,AutoDS,ManualBlock).


% This function calculates the overlap between the sides of manual and
% automatically generated blocks. If 3 or more sides overlap, it returns
% true. NB: overlap of a side is defined as having the same border cell,
% it does not necessarily mean the sides completely coincide

side_overlap(AutoBlock,ManualBlock,Type):-
	sheet_object(Sheet, block, block(AutoBlock,Type,AutoDS)),
	manual_block(ManualBlock,Sheet,Type),
	aggregate_all(count,
		      ( ds_side(Side,AutoDS,Val),
			ds_side(Side,ManualBlock,Val)
		      ), Count),
	Count >= 3.

manual_block(cell_range(Sheet,SX,SY,EX,EY),Sheet,Type):-
	my_manual_block(Id,Sheet,Type),
	block_id_cell_range(Id, cell_range(Sheet,SX,SY,EX,EY)).

block_id_cell_range(Id, cell_range(Sheet,SX,SY,EX,EY)) :-
	sub_atom(Id, 1, _, 1, NoBrace),
	split_string(NoBrace, ".:", "", [Sheet0, From, To]),
	clean_sheet_name(Sheet0, Sheet),
	col_row(From, SX, SY),
	col_row(To, EX, EY).

clean_sheet_name(Sheet0, Sheet) :-
	sub_string(Sheet0, 0, 1, _, "'"), !,
	sub_atom(Sheet0, 1, _, 1, Sheet).
clean_sheet_name(Sheet0, Sheet):-
	string_to_atom(Sheet0,Sheet).

col_row(Name, Col, Row) :-
	sub_atom(Name, B, 1, _, Char),
	char_type(Char, digit), !,
	sub_atom(Name, 0, B, _, ColName),
	sub_atom(Name, B, _, 0, RowName),
	atom_number(RowName, Row),
	column_name(Col, ColName).


%% This function uses a manual mark up of spreadsheet tables by
%% retrieving the background color of cells. The following colors are
%% used:
%% cell_color #00dcff = blue, string or text cells
%% cell_color #ffff00 = yellow, unit cells
%% cell_color #ff6633 = orange, float or measure cells

%manual_block(ManualBlock,Sheet,Type):-
%	(   Type == 'string'
%	->  Color = '#00dcff'
%	;   Type == 'unit'
%	->  Color = '#ffff00'
%	;   Color = '#ff6633'
%	    ),
%	ds_sheet(ManualBlock, Sheet),
%	anchor(ManualBlock, style(cell_color(Color))),
%	once(block(ManualBlock, style(cell_color(Color)))).


