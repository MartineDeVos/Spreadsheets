/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2014, VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(argv_dict,
	  [ argv_dict/2,		% -Dict, -Rest
	    argv_dict/3			% +Argv, -Dict, -Rest
	  ]).

argv_dict(Dict, Rest) :-
	current_prolog_flag(argv, Argv),
	argv_dict(Argv, Dict, Rest).

argv_dict(Argv, Dict, Rest) :-
	argv_pairs(Argv, Pairs, Rest),
	group_pairs_by_key(Pairs, ByKey),
	maplist(list_or_single, ByKey, Grouped),
	dict_pairs(Dict, argv, Grouped).

list_or_single(Key-[Value], Key-Value) :- !.
list_or_single(Key-List, Key-List).

argv_pairs([], [], []) :- !.
argv_pairs([H|T0], Opts, Rest) :-
	sub_atom(H, 0, _, _, '--'), !,
	(   sub_atom(H, B, _, A, =)
	->  NLen is B-2,
	    sub_atom(H, 2, NLen, _, Name),
	    sub_atom(H, _, A, 0, Atom),
	    (   atom_number(Atom, Value)
	    ->  true
	    ;   Value = Atom
	    )
	;   sub_atom(H, 2, _, _, 'no-')
	->  sub_atom(H, 5, _, 0, Name),
	    Value = false
	;   sub_atom(H, 2, _, 0, Name),
	    Value = true
	),
	Opts = [Name-Value|OptT],
	argv_pairs(T0, OptT, Rest).
argv_pairs(Rest, [], Rest).



