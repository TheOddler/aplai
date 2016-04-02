% The classical view of sudoku is that:
% - numbers in a row must be different
% - numbers in a column must be different
% - numbers in a block must be different
% So basically we are mapping numbers to places
%
% Alternate view:
% for the alternate view, we are mapping places - coordinates - to numbers
% instead of rows, we have a list of coordinates for each number
% - no coordinates can be double
% - each list should contain N coordinates (with N^2 the width of the sudoku)
% - within one list, every x and y coordinate must occur once

% I ended up with exactly the same so obviously I went wrong
% I guess the decision "lets not make a list of coords because that will make it
% harder to check the blocks" may not have been the best
% Damn you different view - also Toons' thingy didnt help much D:
% Sleepies now x_x

% Import libraries & compile sudokus
:- lib(ic).
:- lib(util).
:- lib(ic_global).

:- compile("sudex_toledo").

% Solve all different puzzles
solve_all :-
	solve(_),
	fail.
solve_all.

solve(Name) :-
	puzzles(Board,Name),       % get the puzzle

	write("Solving: "), write(Name),

    length(Board,N),
    dim(AltBoard, [N, N]),      % Create alternate board with N rows x N positions
    AltBoard[1..N, 1..N] :: 1..N,

    % Go over each position in the boardlist
    % Select element of row I col J
    % represent on axes
    ( multifor([I,J],1,N), param(Board, AltBoard) do
        nth1(I, Board, RowI),   % takes the nth element, element/3 doesnt work
        nth1(J, RowI, ElemIJ),  % because it is a multilist
        %write(I),write(","),write(J),write(" - "),write(ElemIJ), nl,
        subscript(AltBoard, [I,J], ElemIJ)
    ),

    % Selection methods:
    % input_order, anti_first_fail, first_fail, smallest, occurrence, largest, most_constrained, max_regret
	% Choice methods:
	% indomain/1, indomain_max, indomain_min, indomain_reverse_min Like, indomain_reverse_max, indomain_middle, indomain_median, indomain_split, indomain_reverse_split, indomain_random, indomain_interval
	time(solve(AltBoard, most_constrained,indomain,complete,[backtrack(B)])),

	write("Required "), write(B), write(" backtracks"), nl,

	% write final result
	( foreach(Row, Board)
	do
	    write(Row), nl
	), nl.

solve(AltBoard, Select, Choice, Method, Option) :-
	% get dim, also makes sure it's square
	dim(AltBoard, [N,N]),

	% all cells have range 1 to Dim
	AltBoard :: 1..N,

	% set constraints
	occurs_only_once_constraint(AltBoard),
    square_constraint(AltBoard),            % Needed to ensure 9 in blocks

	% do the search
	search(AltBoard, 0, Select, Choice, Method, Option).

% Check if each row in the grid contains each number from 1 to N only once
occurs_only_once_constraint(AltBoard) :-
    dim(AltBoard, [N,N]),
    ( multifor([I,J],1,N), param(AltBoard,N) do
        Row is AltBoard[I,1..N],
        Col is AltBoard[1..N,I],
        occurrences(J,Row,1),
        occurrences(J,Col,1)
    ).

% check that each block contains all the numbers
square_constraint(AltBoard) :-
	dim(AltBoard, [D,D]),
	DD is integer(sqrt(D)),
	( multifor([I,J],[1,1],[DD,DD]), param(AltBoard,DD)
	do
		Right is (I-1) * DD + 1,
		Left is Right + DD - 1,
		Top is (J-1) * DD + 1,
		Bottom is Top + DD - 1,

		Block is AltBoard[Right..Left, Top..Bottom],
		flatten(Block,FlatBlock),
		alldifferent(FlatBlock)
	).
