:- module(chr_sudoku, [solve/1, solve_all/0]).
:- use_module(library(chr)).
:- chr_option(debug, off). % on - off

:- consult('puzzles.pl').

% Simple Shikaku Solution where each rect is repesented as X,Y coordinates, and Width, Height variables.

% Define types
:- chr_type list(T) ---> [] ; [T | list(T)].
:- chr_type cor ---> (natural, natural).    % x and y coordinates
:- chr_type size ---> (natural, natural).   % height x width
%:- chr_type rect ---> (natural, cor, size)
:- chr_type val ---> [] ; [natural | list(natural)]. % Value or list of possible values

% Define constraints
:- chr_constraint board(+natural, +natural, ?list(val)).
:- chr_constraint rect(?val, +cor, +size).
:- chr_constraint search(+natural).
:- chr_constraint propagate, cleanup.

% Shikaku Solution where each rect is repesented as Top, Left, Bottom and Right coordinate variables.
% Works much better.

solve_all :-
	solve(_),
	fail.
solve_all.

write_solution(Solution) :-
	write("Solution = ["), nl,
	( foreach(Rect, Solution)
	do
		write("\t"), write(Rect), write(",") , nl
	),
	write("]"), nl, nl.


% Constraints
no_overlap @ rect(_,c(X1,Y1),s(W1,H1)), rect(_,c(X2,Y2),s(W2,H2)) # passive
	<=> X1 + W1 =< X2 | false.
no_overlap @ rect(_,c(X1,Y1),s(W1,H1)), rect(_,c(X2,Y2),s(W2,H2)) # passive
	<=> X2 + W2 =< X1 | false.
no_overlap @ rect(_,c(X1,Y1),s(W1,H1)), rect(_,c(X2,Y2),s(W2,H2)) # passive
	<=> Y1 + H1 =< Y2 | false.
no_overlap @ rect(_,c(X1,Y1),s(W1,H1)), rect(_,c(X2,Y2),s(W2,H2)) # passive
	<=> Y2 + H2 =< Y1 | false.

contains_point @ c(X,Y), s(W,H), PX, PY) :-
		PX #>= X,
		PY #>= Y,
		PX #< X + W,
		PY #< Y + H.


alldifferent_in_row @ cell((Row, ColA), [Value]), cell((Row,ColB), [Value]) # passive
    <=> ColA \= ColB | false.
alldifferent_in_column @ cell((RowA, Col), [Value]), cell((RowB,Col), [Value]) # passive
    <=> RowA \= RowB | false.
alldifferent_in_box @ cell((Row,Col), [Value]), cell((ORow,OCol), [Value]) # passive
    <=> (Row \= ORow ; Col \= OCol), box(Row-Col, ORow-OCol) | false.

eliminate_in_row @ propagate, cell((Row,_), [Value])
    \ cell((Row,Col), [V1, V2 | Vs])
    <=> select(Value, [V1, V2 | Vs], NVs)
        | cell((Row, Col), NVs).
eliminate_in_column @ propagate, cell((_,Col), [Value])
    \ cell((Row,Col), [V1, V2 | Vs])
    <=> select(Value, [V1, V2 | Vs], NVs)
        | cell((Row,Col), NVs).
eliminate_in_box @ propagate, cell((Row,Col), [Value])
    \ cell((ORow,OCol), [V1, V2 | Vs])
    <=> (Row \= ORow ; Col \= OCol), box(Row-Col, ORow-OCol),
        select(Value, [V1, V2 | Vs], NVs)
        | cell((ORow,OCol), NVs).

propagate <=> search(2).

first_fail @ search(N), cell((Row,Col), Vs) # passive
<=> length(Vs, Len), Len =:= N | member(V, Vs), cell((Row,Col), [V]), propagate.

search(9) <=> true.
search(N) <=> NN is N + 1, search(NN).

cleanup \ cell(_, _) <=> true.
cleanup <=> true.
