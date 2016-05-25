:- module(chr_shikaku, [solve/1, solve_all/0]).
:- use_module(library(chr)).
:- chr_option(debug, off). % on - off

:- consult('puzzles.pl').
:- consult('print_shikaku.pl').

% Define types
:- chr_type list(T) ---> [] ; [T | list(T)].
:- chr_type c ---> c(natural, natural).   % x and y coordinates
:- chr_type s ---> s(natural, natural).   % height x width
%:- chr_type val ---> (natural, natural, natural, natural).

%:- chr_constraint rect(+c, ?list(val)).
%:- chr_constraint rect(+c, +natural, +natural, +natural, +natural).
%:- chr_constraint rect(+c, +c, +s).
:- chr_constraint rect/2, rect/3, rect/5.
:- chr_constraint propagate, cleanup.

% Shikaku Solution where each rect is repesented as Top, Left, Bottom and Right coordinate variables.

% Solve all puzzles
solve_all :-
	solve(_),
	fail.
solve_all.

% Solve puzzle with specific name
solve(Name) :-
	problem(Name, GridW, GridH, Hints),  % get the puzzle
    write('Solving: '), write(Name), nl, % Feedback puzzle name

    length(Hints, Nbr_of_hints),
	b_setval(to_find_nbr, Nbr_of_hints),

	% Solve puzzle + feedback stats
    once(time(solve(GridW, GridH, Hints))),

	% Change format for prints
	findall(rect(A,B,C),
		(
		find_chr_constraint(rect(A,Top,Left,Bottom,Right)),
		B = c(Left, Top),
		W is Right - Left + 1,
		H is Bottom - Top + 1,
		C = s(W,H)
		), Rectangles),

    %format('Runtime: ~`.t ~2f~34|  Backtracks: ~`.t ~D~72|~n', [RunT, BackT]),
	show(GridW, GridH, Hints, Rectangles, ascii),
    cleanup.

solve(GridW, GridH, Hints) :-
    create_rects(GridW, GridH, Hints, Hints),
    propagate.

create_rects(_, _, [], _) :- !.
create_rects(GridW, GridH, [(X,Y,Area) | OtherHints], Hints) :-
	findall((Top, Left, Bottom, Right),
			(
			inside_grid(Top, Left, Bottom, Right, GridW, GridH),
			Top =< Bottom,
			Left =< Right,
			has_area(Top, Left, Bottom, Right, Area),
			contains_point(Top, Left, Bottom, Right, X, Y),
			select((X,Y,Area), Hints, HintsWithoutCurrent),
			doesnt_contain_hints(Top, Left, Bottom, Right, HintsWithoutCurrent)
			), Possibble_rectangles),
	rect(c(X,Y), Possibble_rectangles),
	create_rects(GridW, GridH, OtherHints, Hints).

/*
 * Utils
 */
doesnt_contain_hints(_, _, _, _, []).
doesnt_contain_hints(Top, Left, Bottom, Right, [(X,Y,_)|OtherHints]) :-
	contains_point(Top, Left, Bottom, Right, X, Y) ->
		false ;
		doesnt_contain_hints(Top, Left, Bottom, Right, OtherHints).

 % Contains point
contains_point(Top, Left, Bottom, Right, X, Y) :-
	X >= Left,
	X =< Right,
	Y >= Top,
	Y =< Bottom.

% Rects are inside the grid
inside_grid(Top, Left, Bottom, Right, GridW, GridH):-
	range(GWrange, 1, GridW),
	range(GHrange, 1, GridH),
	member(Top, GHrange),
	member(Left, GWrange),
	member(Bottom, GHrange),
	member(Right, GWrange).

% Rects need to be a certain area
has_area(Top, Left, Bottom, Right, Area) :-
	Height is Bottom - Top + 1,
	Width is Right - Left + 1,
	Area is Height * Width.

% get array with range Low to High
range([X], X, X).
range([Low|Out],Low,High) :- NewLow is Low+1, NewLow =< High, range(Out, NewLow, High).

/*
 * Constraints
 */

% If the list is empty, Fail
rect(_,[]) ==> fail.

% The last one of a list gets automatically selected
last_of_list @ rect(Point,[(T, L, B, R)])
	<=> rect(Point, T, L, B, R).

% If overlap between rectangles, false
no_overlap_rule @ rect(_, T1, L1, B1, R1), rect(_, T2, L2, B2, R2) #passive
	<=> \+no_overlap(T1, L1, B1, R1, T2, L2, B2, R2) | false.

no_overlap(T1, L1, B1, R1, T2, L2, B2, R2) :-
	(R1 < L2 ; R2 < L1) ;
	(B1 < T2 ; B2 < T1).

% Drastically speeds up some, strongly slows down others
% Removes overlapping in suggestions list
remove_overlapping @ rect(_, T, L, B, R) \ rect(P2,Pos)
    <=>
        overlap(T, L, B, R, Pos, Overlap),
		select(Overlap, Pos, NewPos) | rect(P2,NewPos).

overlap(_,_,_,_,[],_) :- false.
overlap(T1, L1, B1, R1, [(T2, L2, B2, R2) | Rest], Return) :-
	% If there is no overlap
    no_overlap(T1, L1, B1, R1, T2, L2, B2, R2) ->
    overlap(T1, L1, B1, R1, Rest, Return)
        ;
    % if there is an overlap, return the whole list without the overlap
    Return = (T2, L2, B2, R2).

% Guess a possible combination
propagate, rect(Point, Possible) #passive
    <=>
    member((T, L, B, R),Possible), rect(Point,T, L, B, R), propagate.

cleanup \ rect(_, _) <=> true.
cleanup \ rect(_, _, _) <=> true.
cleanup \ rect(_, _, _, _, _) <=> true.
cleanup <=> true.
