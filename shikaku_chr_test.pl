:- module(chr_shikaku, [solve/1, solve_all/0]).
:- use_module(library(chr)).
:- chr_option(debug, off). % on - off

:- consult('puzzles.pl').
:- consult('print_shikaku.pl').

% Define types
:- chr_type list(T) ---> [] ; [T | list(T)].
:- chr_type c ---> c(natural, natural).   % x and y coordinates
:- chr_type s ---> s(natural, natural).   % height x width
:- chr_type val ---> (c,s).

:- chr_constraint rect(+c, ?list(val)).
:- chr_constraint rect(+c, +c, +s).
%:- chr_constraint search(+natural).
:- chr_constraint propagate, cleanup.

% Simple Shikaku Solution where each rect is repesented as
% X,Y coordinates, and Width, Height variables.

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

    %format('Runtime: ~`.t ~2f~34|  Backtracks: ~`.t ~D~72|~n', [RunT, BackT]),
	show(GridW, GridH, Hints, chr, ascii),
    cleanup.

solve(GridW, GridH, Hints) :-
    create_rects(GridW, GridH, Hints),
    propagate.

create_rects(_, _, []) :- !.
create_rects(GridW, GridH, [(X,Y,Area) | OtherHints]) :-
	findall((Pos, Size),
			(
			inside_grid(Pos, Size, GridW, GridH),
			has_area(Size, Area),
			contains_point(c(X,Y), Pos, Size)
			), Possibble_rectangles),
	rect(c(X,Y), Possibble_rectangles),
	create_rects(GridW, GridH, OtherHints).

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
range([Low|Out],Low,High) :- NewLow is Low+1, NewLow =< High, range(Out, NewLow, High).

/*
 * Constraints
 */

% If the list is empty, remove from store
rect(_,[]) ==> fail.
/*max_dim_one_per_point @ propagate, rect(Point,Cor,Size) \ rect(Point,Pos)
    <=> select((Cor,Size),Pos,NewPos) | rect(Point,NewPos).
*/
/*no_overlap_rule @ rect(Point,_,_)\ rect(Point,_) # passive
	<=> rect(Point,[]) | false.*/
/*no_overlap_rule @ rect(P1,_,_), rect(P1,_,_) # passive
	<=> false.*/
no_overlap_rule @ rect(P1,c(X1,Y1),s(W1,H1)), rect(P2,c(X2,Y2),s(W2,H2)) # passive
	<=> P1\=P2, \+no_overlap(c(X1,Y1), s(W1,H1), c(X2,Y2), s(W2,H2)) | false.

no_overlap(c(X1,Y1), s(W1,H1), c(X2,Y2), s(W2,H2)) :-
	(X1 + W1 =< X2 ; X2 + W2 =< X1) ;
	(Y1 + H1 =< Y2 ; Y2 + H2 =< Y1).
/*
no_overlaps @ propagate, rect(P1, Cor, Size) \ rect(P2,Pos)
    <=> P1 \= P2,
        select_no_overlap(Cor, Size, Pos, NewPos) | rect(P2,NewPos).

select_no_overlap(Pos, Size, List, NewList) :-
    select_no_overlap(Pos, Size, List, [], NewList), !,
    NewList \= [].
select_no_overlap(_,_,[],_,[]).
select_no_overlap(c(X1,Y1), s(W1,H1), [(c(X2,Y2), s(W2,H2)) | Rest], Checked, Return) :-
    % If there is no overlap
    no_overlap(c(X1,Y1), s(W1,H1), c(X2,Y2), s(W2,H2)),
    select_no_overlap(c(X1,Y1), s(W1,H1), Rest, [(c(X2,Y2), s(W2,H2))| Checked], Return)
        ;
    % if there is an overlap, return the whole list without the overlap
    append(Rest, Checked, Return).
*/
%propagate <=> search.

% Guess a possible combination
propagate, rect(Point, Possible)
    <=>
    member((Cor, Size),Possible), rect(Point,Cor,Size), propagate.
/*
search(N) <=> nb_getval(width, Nbr_of_hints), N == Width | true.
search(N) <=> NN is N + 1, search(NN).
*/
cleanup \ rect(_, _) <=> true.
cleanup \ rect(_, _, _) <=> true.
cleanup <=> true.
