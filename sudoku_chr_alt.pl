:- module(chr_sudoku_alt, [solve/1, solve_all/0]).
:- use_module(library(chr)).
:- chr_option(debug, off). % on - off

:- consult('sudex_toledo.pl').

% Define types
:- chr_type list(T) ---> [] ; [T | list(T)].
:- chr_type val ---> [] ; [natural | list(natural)]. % Value or list of possible values
:- chr_type rv ---> (natural,natural).

:- chr_constraint board(+natural, +natural, ?list(val)).
:- chr_constraint rvc(+rv, +list(natural)).
:- chr_constraint search(+natural).
:- chr_constraint propagate, cleanup, combine, stop_combine.

% Check if square and get width + length of a block
board(_, _, []) ==> fail.
board(Width, BlockWidth, Board)
    <=> dim(Board, [Width,Width]), BlockWidth is round(sqrt(Width)) | true.

% Solve all puzzles
solve_all :-
    cleanup,
	solve(_), nl,
	fail.
solve_all.

% Solve puzzle with specific name
solve(Name) :-
	puzzles(Board,Name),                 % get the puzzle
    write('Solving: '), write(Name), nl, % Feedback puzzle name

    board(Width, BlockWidth, Board),
    b_setval(width, Width),
    b_setval(blockwidth, BlockWidth),

    once(time(solve(Board, Width))),     % Solve puzzle + feedback stats
    %format('Runtime: ~`.t ~2f~34|  Backtracks: ~`.t ~D~72|~n', [RunT, BackT]),
    %chr_show_store(chr_sudoku_alt), nl, %for debugging
    print_board(Width),
    cleanup.

% Print the board
print_board(X) :- Y is X*X, print_board(0, X, Y).
print_board(X, _, X).
print_board(P, X, Y):-
    b_getval(blockwidth, BlockWidth),
	Row is P//X + 1,
	Col is P mod X + 1,
	% find_chr_constraint(cell((Row,Col),[N])),
	find_chr_constraint(rvc((Row,N),[Col])),
	write(N),write(' '),
    (Col mod BlockWidth =:= 0 -> write('  ');true),
	(Col == X -> nl;true),
	NewP is P+1,
    (NewP mod (BlockWidth*X) =:= 0 -> nl;true),
	print_board(NewP, X, Y).

solve(Board, Width) :-
    combine,
    create_grid_unknown(Board, 1, Width),
    stop_combine,
    create_grid_known(Board, 1, Width),
    propagate.

create_grid_unknown([], _, _):- true.
create_grid_unknown([Row |RestRows], RowNum, Max) :-
    exclude(var,Row,RowKnown),
    create_rows_unknown(Row, RowKnown, RowNum, 1, Max),
    RowNext is RowNum + 1,
    create_grid_unknown(RestRows, RowNext, Max).

create_rows_unknown([], _, _, _, _) :- !.
create_rows_unknown([Num | ColRest], RowKnown, RowNum, ColNum, Max) :-
        %write('***1'), nl,
    (var(Num) -> % If value is Number, not _
        range(Nums, 1, Max),
        subtract(Nums,RowKnown,PossibleNums),
		create_unknown_rvc(RowNum,ColNum,PossibleNums)
    ; true
    ),
        %write('***2'), nl,
    ColNext is ColNum + 1,
    create_rows_unknown(ColRest, RowKnown, RowNum, ColNext, Max).

create_unknown_rvc(_,_,[]) :- true.
create_unknown_rvc(Row, Col, [Num|Rest]) :-
        %write('+++1'), nl,
	rvc((Row,Num),[Col]),
        %write('+++2'), nl,
	create_unknown_rvc(Row,Col,Rest).


create_grid_known([], _, _):- true.
create_grid_known([Row |RestRows], RowNum, Max) :-
    create_rows_known(Row, RowNum, 1, Max),
    RowNext is RowNum + 1,
    create_grid_known(RestRows, RowNext, Max).

create_rows_known([], _, _, _) :- true.
create_rows_known([Num | ColRest], RowNum, ColNum, Max) :-
    (nonvar(Num) -> % If value is Number, not _
		rvc((RowNum,Num),[ColNum])
	; true
    ),
    ColNext is ColNum + 1,
    create_rows_known(ColRest, RowNum, ColNext, Max).

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

same_box(RowA, ColA, RowB, ColB) :-
	nb_getval(blockwidth, BlockWidth),
	(RowA - 1) // BlockWidth =:= (RowB - 1) // BlockWidth,
	(ColA - 1) // BlockWidth =:= (ColB - 1) // BlockWidth.

same_box_cols(_,_,_,[], AggrCols,AggrCols) :- !.
same_box_cols(RowA,ColA,RowB,[ColB|RestColB], AggrCols,Out) :-
	( same_box(RowA,ColA,RowB,ColB) ->
		NewAggrCols = [ColB|AggrCols]
	;
		NewAggrCols = AggrCols
	),
	same_box_cols(RowA,ColA,RowB,RestColB,NewAggrCols,Out).
same_box_cols(RowA,ColA,RowB,ColsB,SameBoxCols) :-
	same_box_cols(RowA,ColA,RowB,ColsB,[],SameBoxCols).

% combine the rvcs with for the same row and num
combine_rvcs @ combine \ rvc((Row,Val), ColsA), rvc((Row,Val), ColsB) # passive
    <=> append(ColsA, ColsB, ColsTotal), rvc((Row,Val), ColsTotal).
stop_combine @ stop_combine, combine <=> true.

% Constraints
no_double_booking @ propagate,
    rvc((Row,ValA),[Col]), rvc((Row,ValB),[Col])
    <=> ValA \= ValB | false.
alldifferent_in_row @ propagate,
	rvc((Row,Value),[ColA]), rvc((Row,Value),[ColB]) # passive
	<=> ColA \= ColB | false.
alldifferent_in_col @ propagate,
	rvc((RowA,Value),[Col]), rvc((RowB,Value),[Col]) # passive
	<=> RowA \= RowB | false.
alldifferent_in_box @ propagate,
	rvc((RowA,Value),[ColA]), rvc((RowB,Value),[ColB]) # passive
	<=> (RowA \= RowB ; ColA \= ColB), same_box(RowA,ColA,RowB,ColB)
	| false.

% eliminate
eliminate_in_row @ propagate,
	%same row, different values, one has exact col, eliminate that col from the other
	rvc((Row,ValA),[ColA]) \ rvc((Row,ValB),[C1,C2|Cs])
	<=> ValA \= ValB, select(ColA, [C1,C2|Cs], NewCs) | ( NewCs = [] -> false; rvc((Row,ValB),NewCs) ).
eliminate_in_col @ propagate,
	%different row, same value, one has exact col, eliminate that col from the other
	rvc((RowA,Val),[ColA]) \ rvc((RowB,Val),[C1,C2|Cs])
	<=> RowA \= RowB, select(ColA, [C1,C2|Cs], NewCs) | ( NewCs = [] -> false; rvc((RowB,Val),NewCs) ).
eliminate_in_box @ propagate,
	% two same values, one has exact col and row, the other only row
	% check for the other all the cols that would put it in the same box
	% remove these collumns from it's possible cols
	rvc((RowA,Val),[ColA]) \ rvc((RowB,Val),[C1,C2|Cs])
	<=> same_box_cols(RowA,ColA,RowB,[C1,C2|Cs],SameBoxCols),
		SameBoxCols = [_|_], %at least one same col
		subtract([C1,C2|Cs], SameBoxCols, NewCs)
		| ( NewCs = [] -> false; rvc((RowB,Val),NewCs) ).

propagate <=> search(2).

first_fail @ search(N), rvc((Row,Val), Cs) # passive
	<=> length(Cs, N)
    | %write('Search: '), write(N), write(','), write(((Row,Val), Cs-C)), nl, %chr_show_store(chr_sudoku_alt), nl,
    member(C, Cs), rvc((Row,Val), [C]), propagate.

search(N) <=> nb_getval(width, Width), N == Width | true.
search(N) <=> NN is N + 1, search(NN).

cleanup \ rvc(_, _) <=> true.
cleanup <=> true.
