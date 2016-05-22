:- module(chr_shikaku, [solve/1, solve_all/0]).
:- use_module(library(chr)).
:- chr_option(debug, off). % on - off

:- consult('puzzles.pl').
:- consult('print_shikaku.pl').

% Simple Shikaku Solution where each rect is repesented as
% X,Y coordinates, and Width, Height variables.

% Define types
:- chr_type list(T) ---> [] ; [T | list(T)].
:- chr_type c ---> c(natural, natural).    % x and y coordinates
:- chr_type s ---> s(natural, natural).   % height x width
%:- chr_type rect ---> (natural, c, s)
%:- chr_type val ---> [] ; [natural | list(natural)]. % Value or list of possible values

% Define constraints
%:- chr_constraint c(+natural, +natural).
%:- chr_constraint s(+natural, +natural).
:- chr_constraint rect(+c, +c, +s).
%:- chr_constraint contains_point(+c, +s, +natural, +natural).
%:- chr_constraint has_area(+s, +natural).
%:- chr_constraint inside_grid(+c, +s, +natural, +natural).
%:- chr_constraint search(+natural).
:- chr_constraint cleanup.

/*
contains_point @ rect(c(Px,Py), c(X,Y), s(W,H))
	<=> Px >= X, Py >= Y, Px < X + W, Py < Y + H | true.
*/
% Solve all puzzles
solve_all :-
	solve(_),
	fail.
solve_all.

% Solve puzzle with specific name
solve(Name) :-
	problem(Name, GridW, GridH, Hints),  % get the puzzle
    write('Solving: '), write(Name), nl, % Feedback puzzle name

	% Solve puzzle + feedback stats
    once(time(solve(GridW, GridH, Hints))),

    %format('Runtime: ~`.t ~2f~34|  Backtracks: ~`.t ~D~72|~n', [RunT, BackT]),
	show(GridW, GridH, Hints, chr, ascii),
    %write_solution(GridW, GridH, Hints),
    cleanup.

solve(_, _, []) :- !.
solve(GridW, GridH, [(X,Y,Area) | OtherHints]) :-
	findall((Pos, Size),
			(
			/*range(GWrange, 1, GridW),
			range(GHrange, 1, GridH),
			member(XX, GWrange),
			member(YY, GHrange),
			member(W, GWrange),
			member(H, GHrange),*/
			inside_grid(Pos, Size, GridW, GridH),
			has_area(Size, Area),
			contains_point(c(X,Y), Pos, Size)
			), Possibble_rectangles),
	write(Possibble_rectangles),nl,
	member((Pos, Size), Possibble_rectangles),
	rect(c(X,Y), Pos, Size),
	solve(GridW, GridH, OtherHints).
/*
create_rectangles(_, []) :- !.
create_rectangles(Point, [(c(X,Y), s(W,H))|Other]) :-
	rect(Point, c(X,Y), s(W,H)),
	create_rectangles(Point, Other).
*/
/*
 * Utils
 */

contains_point(c(Px,Py), c(X,Y), s(W,H)) :-
	Px >= X,
	Py >= Y,
	Px < X + W,
	Py < Y + H.


inside_grid(c(X,Y), s(W,H), GridW, GridH):-
	range(GWrange, 1, GridW),
	range(GHrange, 1, GridH),
	member(X, GWrange),
	member(Y, GHrange),
	member(W, GWrange),
	member(H, GHrange),
	X + W =< GridW + 1,
	Y + H =< GridH + 1.

has_area(s(W,H), Area) :-
	Area is W * H.

% get array with range Low to High
range([X], X, X).
range(Out, Low, High) :- findall(X, between(Low, High, X), Out).

%write_solution(_) :-
%	show(BoardW, BoardH, Hints, chr, unicode).
%write("Solved = ["), nl.
% Get calculate the area for a certain width and height
%area(s(W,H), Area) :- Area is W * H.
/*
contains_point(c(X,Y), s(W,H), PX, PY)
	<=> PX >= X,
		PY >= Y,
		PX < X + W,
		PY < Y + H | true.
*/
% Rects are inside the grid
/*
inside_grid(c(X,Y), s(W,H), GridW, GridH)
	<=> X + W =< GridW + 1, Y + H =< GridH + 1 | true.

has_area(s(W,H), Area)
	<=> Area is W * H |true.
*/



% Constraints
/*
no_overlap @ rect(_,c(X1,_),s(W1,_)), rect(_,c(X2,_),_) # passive
	<=> X1 + W1 =< X2 | false.
no_overlap @ rect(_,c(X1,Y1),s(W1,H1)), rect(_,c(X2,Y2),s(W2,H2)) # passive
	<=> X2 + W2 =< X1 | false.
no_overlap @ rect(_,c(X1,Y1),s(W1,H1)), rect(_,c(X2,Y2),s(W2,H2)) # passive
	<=> Y1 + H1 =< Y2 | false.
no_overlap @ rect(_,c(X1,Y1),s(W1,H1)), rect(_,c(X2,Y2),s(W2,H2)) # passive
	<=> Y2 + H2 =< Y1 | false.
*/

no_overlap @ rect(_,c(X1,_), s(W1,_)), rect(_,c(X2,_), _) # passive
	<=> X1 + W1 =< X2 | false.
no_overlap @ rect(_,c(X1,_), _), rect(_,c(X2,_), s(W2,_)) # passive
	<=> X2 + W2 =< X1 | false.
no_overlap @ rect(_,c(_,Y1), s(_,H1)), rect(_,c(_,Y2), _) # passive
	<=> Y1 + H1 =< Y2 | false.
no_overlap @ rect(_,c(_,Y1), _), rect(_,c(_,Y2), s(_,H2)) # passive
	<=> Y2 + H2 =< Y1 | false.

cleanup \ rect(_, _, _) <=> true.
cleanup <=> true.
