:- module(chr_sudoku_alt, [solve/1, solve_all/0]).
:- use_module(library(chr)).
:- chr_option(debug, off). % on - off

:- consult('sudex_toledo.pl').


% Solve all puzzles
solve_all :-
	solve(_), nl,
	fail.
solve_all.

% Solve puzzle with specific name
solve(Name) :-
	puzzles(Board,Name),                    % get the puzzle
    write('Solving: '), write(Name), nl,    % Feedback puzzle name

    board(Width, BlockWidth, Board),
    b_setval(width, Width),
    b_setval(blockwidth, BlockWidth),

    once(time(solve(Board, Width))),                    % Solve puzzle + feedback stats
    %format('Runtime: ~`.t ~2f~34|  Backtracks: ~`.t ~D~72|~n', [RunT, BackT]),
    print_board(Width),
    cleanup.
