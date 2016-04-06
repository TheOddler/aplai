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

% We represent the coordinates as an array where the column represent the X coordinate
% and the row the possible Y coordinate
% So we have a 2D array, each the row index is the number (1 to N)

% Import libraries & compile sudokus
:- lib(ic).
:- lib(util).

:- compile("sudex_toledo").

% Alternate sudoku solution where we have lists of coordinates, rather than rows of numbers

% Solve all different puzzles
solve_all :-
	solve(_),
	fail.
solve_all.

solve(Name) :-
	puzzles(Board,Name),       % get the puzzle

	%Board is a list, but array is more useful
	( foreach(Row,Board), foreach(RowArray,Out)
	do
		array_list(RowArray,Row)
	),
	array_list(BoardArray,Out),

	write("Solving: "), write(Name),

    % Selection methods:
    % input_order, anti_first_fail, first_fail, smallest, occurrence, largest, most_constrained, max_regret
	% Choice methods:
	% indomain/1, indomain_max, indomain_min, indomain_reverse_min Like, indomain_reverse_max, indomain_middle, indomain_median, indomain_split, indomain_reverse_split, indomain_random, indomain_interval
	time(solve(BoardArray, OutCoordinates, most_constrained,indomain,complete,[backtrack(B)])),

	write("Required "), write(B), write(" backtracks"), nl,

	% write final result
	write("Alternate Solution: "), nl,
	dim(OutCoordinates, [N,N]),
	( for(Number, 1, N), param(N, OutCoordinates)
	do
		write("Number "), write(Number), write(": "),
		( for(Col, 1, N), param(Number, OutCoordinates)
		do
			Row is OutCoordinates[Number, Col],
			write("("), write(Col), write(","), write(Row), write(")"), write("  ")
		),
		nl
	), nl,

	write("Or properly readable: "), nl,
	( foreach(Row, Board)
	do
	    write(Row), nl
	), nl.

solve(BoardArray, Coordinates, Select, Choice, Method, Option) :-
	% Find dimention of board
	dim(BoardArray, [N,N]),

	% An array for the coordinates
	% Rows are the numbers
	% Columns represent the colums the number is on, each number has a spot on each Columns
	% The fields represent the possible rows the number is one
	dim(Coordinates, [N,N]),
	Coordinates :: 1..N,

	% ROW/COL Constrain the coordinates
	( for(I, 1, N), param(Coordinates, N)
	do
		% Each number (so each row in Coordinates) must have different rows
		alldifferent(Coordinates[I,1..N]),
		% Make sure no two numbers are on the same row and column
		alldifferent(Coordinates[1..N,I])
	),

    % Link the numbers from the board with coordinates
	( multifor([I,R,C], [1,1,1], [N,N,N]),
		param(BoardArray, Coordinates)
	do
		% the number in the board on R,C is equal to I
		% if the number in coordinates on I,C is equal to R
		#=(BoardArray[R,C], I, B),
		#=(Coordinates[I,C], R, B)
		% The numbers in BoardArray represent the actual number, so I
		% While the numbers in Coordinates represent the possible rows, so R
		% Columns are both just the colums, so C
	),

    NN is integer(sqrt(N)),
    ( multifor([Number, X], [1,1], [N, NN]), param(Coordinates, NN)
    do
        % Because of earlier constraints, we already know that there are no
        % numbers on the same row or in the same column
        % Every NN coordinates in a column represent a row of NN blocks
        % In the example of 3x3 sudokus, this means that in the first column
        % (the column for number 1), the first 3 Y-coordinates must lie between
        % 1..3 - 4..6 or 7..9

        % Define the blocks' dimension
        Left is (X-1) * NN + 1,
        Right is Left + NN - 1,
        % dus maar 1 Y waarde tussen 1 - 3, 4 - 6 en 7 - 9

        % Get an array with all the Y values
        BlockRow is Coordinates[Number, Left..Right],
        flatten(BlockRow, UnorderedY)

        % In the array, there must be one Y coordinate for each block
        ( for(BlockNo, 1, NN), param(NN, UnorderedY)
        do
            member(B, UnorderedY),
            (BlockNo * NN - B) #> -1,
            (BlockNo * NN - B) #< NN
        )
    ),

	% do the search
	search(Coordinates, 0, Select, Choice, Method, Option).
