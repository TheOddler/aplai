% Channeling
% Usage:
% solve(Name) like always
% you can also use solve(Name, ModelUsage) as helper function, ModelUsage can be 'both', 'simple' or 'alt'. Using 'both' will give you channeling, otherwise it's just the simple or alt model.

:- lib(ic).
:- lib(util).

:- compile("sudex_toledo").
:- compile("sudoku_eclipse").
:- compile("sudoku_eclipse_alt").

solve_all :-
	solve(_),
	fail.
solve_all.

solve(Name) :- solve(Name, both).

solve(Name, ModelUsage) :-
	puzzles(Board,Name),       % get the puzzle

	write("Solving with "),write(ModelUsage),write(": "), write(Name),

    % Selection methods:
    % input_order, anti_first_fail, first_fail, smallest, occurrence, largest, most_constrained, max_regret
	% Choice methods:
	% indomain/1, indomain_max, indomain_min, indomain_reverse_min Like, indomain_reverse_max, indomain_middle, indomain_median, indomain_split, indomain_reverse_split, indomain_random, indomain_interval
	time(solve(Board, ModelUsage, most_constrained,indomain,complete,[backtrack(B)])),

	write("Required "), write(B), write(" backtracks"), nl,

	% write final result
	write("Or properly readable: "), nl,
	write_board(Board).

solve(Board, ModelUsage, Select, Choice, Method, Option) :-
	% Model
	( ModelUsage == both ->
		% They are both linked to the corresponding Board variables, so channeling done
		% combine variables
		model(Board, BoardArray),
		alt_model(Board, Coordinates),
		array_concat(BoardArray, Coordinates, Variables)
	; ModelUsage == simple ->
		model(Board, Variables)
	; ModelUsage == alt ->
		alt_model(Board, Variables)
	; nl, nl, write("Please use 'both', 'simple' or 'alt' as model usage."), nl, nl
	),

	% do the search
	search(Variables, 0, Select, Choice, Method, Option).
