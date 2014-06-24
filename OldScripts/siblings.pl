resource('A').
resource('B').
resource('C').
resource('D').
resource('E').
resource('F').
resource('G').
resource('H').
resource('I').
resource('J').
resource('K').
resource('L').
resource('M').
resource('hydrogen').
resource('electricity').
resource('uranium').
resource('coal').
resource('biomass').
resource('oil').
resource('heat').

parent('hydrogen','A').
parent('A','B').
parent('B','C').
parent('electricity','D').
parent('D','B').
parent('D','E').
parent('E','C').
parent('E','F').
parent('F','G').
parent('uranium','E').
parent('coal','E').
parent('biomass','H').
parent('H','I').
parent('I','G').
parent('J','I').
parent('oil','J').
parent('oil','K').
parent('K','L').
parent('L','M').
parent('heat','K').


ancestor(X,Y)  :-parent(X,Y).
ancestor(X,Y)  :-parent(X,Z),
		 ancestor(Z,Y).

ancestor(X,Y,parent(X)) :- parent(X,Y).
ancestor(X,Y,parent(Z2)) :- parent(Z,Y), ancestor(X,Z,Z2).


end_of_chain(Resource):-
	resource(Resource),
	not(ancestor(_,Resource)).


end_ancestor(Descendant,Ancestor):-
	ancestor(Descendant,Ancestor),
	end_of_chain(Descendant).

num_end_descendants(Ancestor,DescList,NumDesc):-
	setof(Descendant,end_ancestor(Descendant,Ancestor),DescList),
	length(DescList,NumDesc).



%descend(X,Y) :- parent(Y,X).
%descend(X,Y) :- parent(Z,X),
%	        descend(Z,Y).



%grandparent(X,Y) :- parent(P,Y), parent(X,P).
%greatgrandparent(X,Y) :- parent(P,Y), grandparent(X,P).



%common_ancestor(X,Y,A):-
%	ancestor(A,Y),ancestor(A,X), X\=Y.

%nr_descendants(Number,Resource):-
%	resource(Resource),
%	findall(Descendant,descend(Descendant,Resource),DList),
%	length(DList,Number).



