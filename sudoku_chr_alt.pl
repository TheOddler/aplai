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
:- chr_constraint propagate, cleanup.

% Check if square and get width + length of a block
board(_, _, []) ==> fail.
board(Width, BlockWidth, Board)
    <=> dim(Board, [Width,Width]), BlockWidth is round(sqrt(Width)) | true.

% Solve all puzzles
solve_all :-
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
	% find_chr_constraint(cell((Row,Col),[N])),
	find_chr_constraint(rvc((Row,N),[Col])),
	write(N),write(' '),
    (Col mod BlockWidth =:= 0 -> write('  ');true),
	(Col == X -> nl;true),
	NewP is P+1,
    (NewP mod (BlockWidth*X) =:= 0 -> nl;true),
	print_board(NewP, X, Y).

create_grid([], _, _):- !.
create_grid([Row |RestRows], RowNum, Max) :-
    create_rows(Row, RowNum, 1, Max),
    RowNext is RowNum + 1,
    create_grid(RestRows, RowNext, Max).

create_rows([], _, _, _) :- !.
create_rows([Num | ColRest], RowNum, ColNum, Max) :-
    (nonvar(Num) -> % If value is Number, not _
		%cell((RowNum, ColNum), [Num]);
		rvc((RowNum,Num),[ColNum])
	; % else (so now value is _)
        range(Nums, 1, Max),
		create_unknown_rvc(RowNum,ColNum,Nums)
    ),
    ColNext is ColNum + 1,
    create_rows(ColRest, RowNum, ColNext, Max).

create_unknown_rvc(_,_,[]) :- !.
create_unknown_rvc(Row, Col, [Num|Rest]) :-
	rvc((Row,Num),[Col]),
	create_unknown_rvc(Row,Col,Rest).

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

test_same_box_cols(Output) :-
	b_setval(blockwidth, 3),
	RowA = 5,
	ColA = 5,
	RowB = 1,
	[C1,C2|Cs] = [1,2,3,4,5,6,7,8,9],

	same_box_cols(RowA,ColA,RowB,[C1,C2|Cs],SameBoxCols),
	length(SameBoxCols,Count), Count > 0, %at least one same col
	subtract([C1,C2|Cs], SameBoxCols, NewCs),

	Output = NewCs.

% combine the rvcs with for the same row and num
combine_rvcs @ rvc((Row,Val), [Col]), rvc((Row,Val), Cols) # passive
    <=> rvc((Row,Val), [Col | Cols]).

% Constraints
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
	<=> ValA \= ValB, select(ColA, [C1,C2|Cs], NewCs) | rvc((Row,ValB),NewCs).
eliminate_in_col @ propagate,
	%different row, same value, one has exact col, eliminate that col from the other
	rvc((RowA,Val),[ColA]) \ rvc((RowB,Val),[C1,C2|Cs])
	<=> RowA \= RowB, select(ColA, [C1,C2|Cs], NewCs) | rvc((RowB,Val),NewCs).
%eliminate_in_box @ propagate,
	% two same values, one has exact col and row, the other only row
	% check for the other all the cols that would put it in the same box
	% remove these collumns from it's possible cols
%	rvc((RowA,Val),[ColA]) \ rvc((RowB,Val),[C1,C2|Cs])
%	<=> same_box_cols(RowA,ColA,RowB,[C1,C2|Cs],SameBoxCols),
%		length(SameBoxCols,Count), Count > 0, %at least one same col
%		subtract([C1,C2|Cs], SameBoxCols, NewCs)
%		| rvc((RowB,Val),NewCs).

propagate <=> search(2).

first_fail @ search(N), rvc((Row,Val), Cs) # passive
	<=> length(Cs, N) | member(C, Cs), rvc((Row,Val), [C]), propagate.

search(N) <=> nb_getval(width, Width), N == Width | true.
search(N) <=> NN is N + 1, search(NN).

cleanup \ rvc(_, _) <=> true.
cleanup <=> true.
