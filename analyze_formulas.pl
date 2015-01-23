:- ensure_loaded(roadmap).


total_formula_group(Groups):-
	Groups = 0,
	forall(Sheet,
	       (      sheet_ds_formulas(Sheet, Formulas),
		      length(Formulas,Length),
		      Groups = Groups + Length
	       )).





		 /*******************************
		 *     INTERMEDIATE RESULTS     *
		 *******************************/

% Single (NB so not ranges) cells that are input to a formula in an
% other cell
input_cell(Input):-
	cell_formula(Sheet, X, Y,_),
	ods_formula:cell_dependency(user:Sheet,cell(user:Sheet,X,Y), Inputs),
	member(Input,Inputs),
	\+ds_cell_count(Input,_).

unique_input_cell(Unique):-
	findall(Input,input_cell(Input),InputList),
	sort(InputList,InputSet),
	member(Unique,InputSet).

assert_inputs:-
	forall(unique_input_cell(Unique),
	assert(input(Unique))).

% A intermediate result cell is 1) a float cell, 2) a sink cell ( is not
% input itself) and 3) input cells of sink cell should also be input to
% at least one other cell. An intermediate result cell is a dead end,
% but the calculation continues through its inputs

sink_cell(cell(user:Sheet,X,Y)):-
	cell_type(Sheet,X,Y,float),
	cell_formula(Sheet, X, Y,_),
	\+input(cell(user:Sheet,X,Y)).
%	\+input_cell(cell(user:Sheet,X,Y)).

unique_sink_cell(Unique):-
	findall(Sink,sink_cell(Sink),SinkList),
	sort(SinkList,SinkSet),
	member(Unique,SinkSet).

assert_sinks:-
	forall(unique_sink_cell(Unique),
	       assert(sink(Unique))).

intermediate_result(cell(user:Sheet,X,Y)):-
	sink(cell(user:Sheet,X,Y)),
	ods_formula:cell_dependency(user:Sheet,cell(user:Sheet,X,Y),Inputs),
	member(Input,Inputs),
	ods_formula:cell_dependency(user:Sheet1,cell(user:Sheet1,X1,Y1),Inputs1),
	member(Input,Inputs1),
	\+X==X1,
	\+Y==Y1.
