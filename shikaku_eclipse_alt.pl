:- lib(ic).
%:- lib(gfd).
:- lib(util).
:- lib(lists).

:- compile("puzzles").
:- compile("print_shikaku").

% Shikaku Solution where each rect is repesented as Top, Left, Bottom and Right coordinate variables.
% Works much better.

solve_all :-
	solve(_),
	fail.
solve_all.

solve(Name) :-
	problem(Name, GridW, GridH, Hints), % get the puzzle

	write("Solving: "), write(Name),

	% Selection methods: smallest works best apparently
	% input_order, anti_first_fail, first_fail, smallest, occurrence, largest, most_constrained, max_regret
	% Choice methods:
	% indomain/1, indomain_max, indomain_min, indomain_reverse_min Like, indomain_reverse_max, indomain_middle, indomain_median, indomain_split, indomain_reverse_split, indomain_random, indomain_interval
	time(solve(GridW, GridH, Hints, Solution, smallest, indomain, complete, [backtrack(B)])),

	write("Required "), write(B), write(" backtracks"), nl,

	% write final result
	write_solution(GridW, GridH, Hints, Solution).

write_solution(GridW, GridH, Hints, Solution) :-
	% Transform view to uniform with the school shikaku print
	( foreach(rect(Id, Top, Left, Bottom, Right), Solution),
	  foreach(Rect, SolutionAlt) %will be filled
	do
		C = c(Left, Top),
		W is Right - Left + 1,
		H is Bottom - Top + 1,
		S = s(W,H),
		Rect = rect(Id,C,S)
	),
	show(GridW, GridH, Hints, SolutionAlt, ascii).

/*
write_solution(Solution) :-
	write("Solution = ["), nl,
	( foreach(rect(Id, Top, Left, Bottom, Right), Solution)
	do
		C = c(Left, Top),
		W is Right - Left + 1,
		H is Bottom - Top + 1,
		S = s(W,H),
		write("\t"), write(rect(Id,C,S)), write(",") , nl
	),
	write("]"), nl, nl.
*/

solve(GridW, GridH, Hints, Solution, Select, Choice, Method, Option) :-
	% Create constraints
	( foreach((X,Y,Area), Hints),
	  foreach(Rect, Solution), %will be filled
	  param(GridW), param(GridH), param(Hints)
	do
		Rect = rect(c(X,Y), Top, Left, Bottom, Right),
		Top #=< Bottom,
		Left #=< Right,

		inside_grid(Top, Left, Bottom, Right, GridW, GridH),
		has_area(Top, Left, Bottom, Right, Area),
		contains_point(Top, Left, Bottom, Right, X, Y),

		% extra redundant constraint
		subtract(Hints, [(X,Y,Area)], OtherHints),
		doesnt_contain_hints(Top, Left, Bottom, Right, OtherHints)
	),
	no_overlap(Solution),
	term_variables(Solution, Vars),
	search(Vars, 0, Select, Choice, Method, Option).

% Rects are inside the grid
inside_grid(Top, Left, Bottom, Right, GridW, GridH) :-
	Top :: 1..GridH,
	Left :: 1..GridW,
	Bottom :: 1..GridH,
	Right :: 1..GridW.

% Rects need to be a certain area
has_area(Top, Left, Bottom, Right, Area) :-
	[W,H] #:: 1..Area,
	W*H #= Area,
	Right - Left #= W - 1,
	Bottom - Top #= H - 1.

% Contains point
contains_point(Top, Left, Bottom, Right, X, Y) :-
	X #>= Left,
	X #=< Right,
	Y #>= Top,
	Y #=< Bottom.

% Additional redundant constraint:
% Does not containt points
doesnt_contain_hints(Top, Left, Bottom, Right, Hints) :-
	( foreach((X,Y,_), Hints),
	  param(Top), param(Left), param(Bottom), param(Right)
	do
		X #< Left or
		X #> Right or
		Y #< Top or
		Y #> Bottom
	).

% No-overlap
no_overlap(Rects) :-
	( fromto(Rects, [Rect1 | Rest1], Rest1, [])
	do
		( fromto(Rest1, [Rect2 | Rest2], Rest2, []), param(Rect1)
		do
			no_overlap(Rect1, Rect2)
		)
	).
no_overlap(rect(_, T1, L1, B1, R1), rect(_, T2, L2, B2, R2)) :-
    R1#<L2 or
	R2#<L1 or
	B1#<T2 or
	B2#<T1.
