:- lib(ic).
%:- lib(gfd).
:- lib(util).

:- compile("puzzles").

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
	write_solution(Solution).

write_solution(Solution) :-
	write("Solution = ["), nl,
	( foreach(Rect, Solution)
	do
		write("\t"), write(Rect), write(",") , nl
	),
	write("]"), nl, nl.

solve(GridW, GridH, Hints, Solution, Select, Choice, Method, Option) :-
	% Create constraints
	( foreach((X,Y,Area), Hints),
	  foreach(Rect, Solution), %will be filled
	  param(GridW), param(GridH)
	do
		Pos = c(_,_),
		Size = s(_,_),
		Rect = rect(c(X,Y), Pos, Size),

		has_area(Size, Area),
		contains_point(Pos, Size, X, Y),
		inside_grid(Pos, Size, GridW, GridH)
	),
	no_overlap(Solution),
	term_variables(Solution, Vars),
	search(Vars, 0, Select, Choice, Method, Option).

% Rects are inside the grid
inside_grid(c(X,Y), s(W,H), GridW, GridH) :-
	X :: 1..GridW,
	Y :: 1..GridH,
	W :: 1..GridW,
	H :: 1..GridH,
	X + W #=< GridW + 1,
	Y + H #=< GridH + 1.

% Rects need to be a certain area
has_area(s(W,H), A) :- W * H #= A.

% Contains point
contains_point(c(X,Y), s(W,H), PX, PY) :-
	PX #>= X,
	PY #>= Y,
	PX #< X + W,
	PY #< Y + H.

% No-overlap
no_overlap(Rects) :-
	( fromto(Rects, [Rect1 | Rest1], Rest1, [])
	do
		( fromto(Rest1, [Rect2 | Rest2], Rest2, []), param(Rect1)
		do
			no_overlap(Rect1, Rect2)
		)
	).
no_overlap(rect(_,c(X1,Y1),s(W1,H1)), rect(_,c(X2,Y2),s(W2,H2))) :-
    X1 + W1 #=< X2 or
	X2 + W2 #=< X1 or
	Y1 + H1 #=< Y2 or
	Y2 + H2 #=< Y1.
