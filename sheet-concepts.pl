#!/usr/bin/env swipl

:- set_prolog_flag(verbose, silent).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/turtle)).
:- use_module(library(thread)).
:- use_module(library(apply)).
:- use_module(argv_dict).
:- use_module(lib/rdf_load_any).
:- ensure_loaded(roadmap).
:- initialization main.

main :-
	argv_dict(Options, Sheets),
	start_tmon(Options),
	sheet_concepts(Sheets, Options).

sheet_concepts(_Sheets, Options) :-
	_{help:true} :< Options, !,
	usage,
	halt.
sheet_concepts(_Sheets, Options) :-
	_{pl:true} :< Options, !.
sheet_concepts(Sheets, Options) :-
	(   true == Options.get(trace)
	->  gtrace
	;   true
	),
	concurrent(2,
	    [ load_ontologies(Options),
	      load_sheets(Sheets)
	    ], []),
	extract_concepts(Options),
	report(Options),
	halt.

start_tmon(Options) :-
	_{tmon:true} :< Options, !,
	prolog_ide(thread_monitor).
start_tmon(_).

usage :-
	current_prolog_flag(os_argv, [_SwiPL,Prog|_]),
	format(user_error, 'Usage: ~w [option ...] sheet~n', [Prog]),
	format(user_error, 'Options:~n', []),
	format(user_error, '  --in-scheme=URI     Only consider concepts in SKOS scheme~n', []),
	format(user_error, '  --ontology=src      Load ontologies from source~n', []),
	format(user_error, '  --tmon		  Show actvity window~n', []),
	format(user_error, '  --pl		  Start toplevel~n', []),
	format(user_error, '  --trace             Run debugger~n', []).

load_ontologies(Options) :-
	_{ontology:Ontology} :< Options, !,
	rdf_load_any(Ontology).
load_ontologies(_).

load_sheets([Sheet]) :- !,
	ods_unload,
	ods_load(Sheet).
load_sheets(_) :-
	usage, halt(1).

extract_concepts(_Options) :-
	segment,
	assert_agro_concepts,
	assert_agro_ancestors,
	assert_block_ancestors,
	assert_select_concepts.

report(Options) :-
	report_block_ancestors(Options),
	report_concepts(Options).

report_block_ancestors(_Options) :-
	block_ancestors(BlockAncestors),
	list_concepts(BlockAncestors).

report_concepts(_Options) :-
	selected_concepts(Concepts),
	list_concepts(Concepts).

list_concepts(Concepts) :-
	rdf_save_turtle(stream(current_output),
			[ expand(label_triple(Concepts)),
			  group(false),
			  subject_white_lines(0)
			]).

label_triple(Concepts, S,P,O,_G) :-
	rdf_equal(skos:prefLabel, P),
	member(S, Concepts),
	rdf(S, P, O).


selected_concepts(Concepts) :-
	findall(Concept,
		rdf(Concept, sheet:selectAgroConceptOf, literal(_Label), sheet_labels),
		Concepts0),
	sort(Concepts0, Concepts).

block_ancestors(Concepts) :-
	findall(Concept,
		rdf(Concept, sheet:blockAncestorOf, literal(_Label), sheet_labels),
		Concepts0),
	sort(Concepts0, Concepts).

segment :-
	clean_data,
	assert_labels(_Sheet),
	assert_blocks(_Sheet1,_Type),
	color_sheets(_Sheet2, block).

