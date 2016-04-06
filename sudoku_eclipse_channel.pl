:- lib(ic).
:- lib(util).

:- compile("sudex_toledo").
:- compile("sudoku_eclipse").
:- compile("sudoku_eclipse_alt").

solve_all :-
	solve(_),
	fail.
solve_all.

solve(Name) :-
	puzzles(Board,Name),       % get the puzzle

	write("Solving with channeling: "), write(Name),

    % Selection methods:
    % input_order, anti_first_fail, first_fail, smallest, occurrence, largest, most_constrained, max_regret
	% Choice methods:
	% indomain/1, indomain_max, indomain_min, indomain_reverse_min Like, indomain_reverse_max, indomain_middle, indomain_median, indomain_split, indomain_reverse_split, indomain_random, indomain_interval
	time(solve(Board, most_constrained,indomain,complete,[backtrack(B)])),

	write("Required "), write(B), write(" backtracks"), nl,

	% write final result
	write("Or properly readable: "), nl,
	write_board(Board).

solve(Board, Select, Choice, Method, Option) :-
	% Model both
	model(Board, BoardArray),
	alt_model(Board, Coordinates),
	% They are both linked to the corresponding Board variables, so channeling done

	array_concat(BoardArray, Coordinates, Combined),

	% do the search on combined variables
	search(Combined, 0, Select, Choice, Method, Option).
