:- module(chr_sudoku, [solve/1, solve_all/0]).
:- use_module(library(chr)).
:- chr_option(debug, off). % on - off

:- consult('sudex_toledo.pl').

% Define types
:- chr_type list(T) ---> [] ; [T | list(T)].
:- chr_type pos ---> (natural, natural).      % Row x column
:- chr_type val ---> [] ; [natural | list(natural)]. % Value or list of possible values

% Define constraints
:- chr_constraint board(+natural, +natural, ?list(val)).
:- chr_constraint cell(+pos, ?val).
:- chr_constraint search(+natural).
:- chr_constraint propagate, cleanup.

% Check if square and get width + length of a block
board(_, _, []) ==> fail.
board(Width, BlockWidth, Board)
    <=> dim(Board, [Width,Width]), BlockWidth is round(sqrt(Width))| true.

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

solve(Board, Width) :-
    create_grid(Board, 1, Width),
    propagate.


% Print the board
print_board(X) :- Y is X*X, print_board(0, X, Y).
print_board(X, _, X).
print_board(P, X, Y):-
    b_getval(blockwidth, BlockWidth),
	Row is P//X + 1,
	Col is P mod X + 1,
	find_chr_constraint(cell((Row,Col),[N])),
	write(N),write(' '),
    (Col mod BlockWidth =:= 0 -> write('  ');true),
	(Col == X -> nl;true),
	NewP is P+1,
    (NewP mod (BlockWidth*X) =:= 0 -> nl;true),
	print_board(NewP, X, Y).


% Create the cells from the board...
create_grid([], _, _):- !.
create_grid([Row |RestRows], RowNum, Max) :-
    create_rows(Row, RowNum, 1, Max),
    RowNext is RowNum + 1,
    create_grid(RestRows, RowNext, Max).

create_rows([], _, _, _) :- !.
create_rows([Num | ColRest], RowNum, ColNum, Max) :-
    (nonvar(Num) ->                          % If value is Number, not _
        cell((RowNum, ColNum), [Num])  ;
        (range(Possible_positions, 1, Max),
         cell((RowNum, ColNum), Possible_positions))
    ),
    ColNext is ColNum + 1,
    create_rows(ColRest, RowNum, ColNext, Max).

/*
 * Utils
 */

% Get the dimension
dim([X | XRest], [D | DD]) :- length([X | XRest], D), dim(X, DD), !.
dim(X, [D]) :- is_list(X), length(X, D).
dim(_, []).

% get array with range Low to High
range([X], X, X).
range(Out, Low, High) :- findall(X, between(Low, High, X), Out).

box(Row-Col, ORow-OCol) :-
    nb_getval(blockwidth, BlockWidth),
    (Row - 1) // BlockWidth =:= (ORow - 1) // BlockWidth,
    (Col - 1) // BlockWidth =:= (OCol - 1) // BlockWidth.


% Constraints

alldifferent_in_row @ cell((Row, ColA), [Value]), cell((Row,ColB), [Value]) # passive
    <=> ColA \= ColB | false.
alldifferent_in_column @ cell((RowA, Col), [Value]), cell((RowB,Col), [Value]) # passive
    <=> RowA \= RowB | false.
alldifferent_in_box @ cell((Row,Col), [Value]), cell((ORow,OCol), [Value]) # passive
    <=> (Row \= ORow ; Col \= OCol), box(Row-Col, ORow-OCol) | false.

eliminate_in_row @ propagate, cell((Row,_), [Value])
    \ cell((Row,Col), [V1, V2 | Vs])
    <=> select(Value, [V1, V2 | Vs], NVs)
        | cell((Row, Col), NVs).
eliminate_in_column @ propagate, cell((_,Col), [Value])
    \ cell((Row,Col), [V1, V2 | Vs])
    <=> select(Value, [V1, V2 | Vs], NVs)
        | cell((Row,Col), NVs).
eliminate_in_box @ propagate, cell((Row,Col), [Value])
    \ cell((ORow,OCol), [V1, V2 | Vs])
    <=> (Row \= ORow ; Col \= OCol), box(Row-Col, ORow-OCol),
        select(Value, [V1, V2 | Vs], NVs)
        | cell((ORow,OCol), NVs).

propagate <=> search(2).

first_fail @ search(N), cell((Row,Col), Vs) # passive
<=> length(Vs, Len), Len =:= N | member(V, Vs), cell((Row,Col), [V]), propagate.

search(9) <=> true.
search(N) <=> NN is N + 1, search(NN).

cleanup \ cell(_, _) <=> true.
cleanup <=> true.
