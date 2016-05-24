:- module(chr_sudoku_alt, [solve/1, solve_all/0]).
:- use_module(library(chr)).
:- chr_option(debug, off). % on - off

:- consult('sudex_toledo.pl').

% Define types
:- chr_type list(T) ---> [] ; [T | list(T)].
:- chr_type val ---> [] ; [natural | list(natural)]. % Value or list of possible values
:- chr_type rv ---> (natural,natural).
:- chr_type pos ---> (natural, natural).      % Row x column

:- chr_constraint board(+natural, +natural, ?list(val)).
:- chr_constraint rvc(+rv, +list(natural)).
:- chr_constraint cell(+pos, ?val).
:- chr_constraint search(+natural).
:- chr_constraint propagate, cleanup, combine, stop_combine.
:- chr_constraint remove_rcv(+natural, +natural, +natural), fix_rcv(+natural, +natural, +natural).

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
	find_chr_constraint(cell((Row,Col),[N])),
	write(N),write(' '),
    (Col mod BlockWidth =:= 0 -> write('  ');true),
	(Col == X -> nl;true),
	NewP is P+1,
    (NewP mod (BlockWidth*X) =:= 0 -> nl;true),
	print_board(NewP, X, Y).

solve(Board, Width) :-
    % Basic view
    create_grid(Board, 1, Width),
    % Alt view
    combine,
    create_grid_unknown(Board, 1, Width),
    stop_combine,
    create_grid_known(Board, 1, Width),
    %chr_show_store(chr_sudoku_alt), nl, %for debugging
    % Start search
    propagate.

% Create the cells from the board...
create_grid([], _, _):- !.
create_grid([Row |RestRows], RowNum, Max) :-
    create_rows(Row, RowNum, 1, Max),
    RowNext is RowNum + 1,
    create_grid(RestRows, RowNext, Max).

create_rows([], _, _, _) :- !.
create_rows([Num | ColRest], RowNum, ColNum, Max) :-
    (nonvar(Num) ->                          % If value is Number, not _
        cell((RowNum, ColNum), [Num])
    ;
        range(Possible_positions, 1, Max),
        cell((RowNum, ColNum), Possible_positions)
    ),
    ColNext is ColNum + 1,
    create_rows(ColRest, RowNum, ColNext, Max).

% Alt view
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

% Channeling stuff
fix_values @ %propagate,
    fix_rcv(Row,Col,Val),
    cell((Row,Col), _), rvc((Row,Val),_)
    <=> cell((Row,Col), [Val]), rvc((Row,Val),[Col]).
fix_rcv(Row,Col,Val)
    <=> write(('No fix', Row, Col, Val)), nl,
    chr_show_store(chr_sudoku_alt), nl,
    break,
    true.

remove_when_two @ propagate,
    remove_rcv(Row,Col,Val) \ cell((Row,Col),[V1,V2]) # passive
    <=> select(Val, [V1,V2], [NewVal])
    | cell((Row,Col),[NewVal]), fix_rcv(Row,Col,NewVal).
remove_when_two @ propagate,
    remove_rcv(Row,Col,Val) \ rvc((Row,Val),[C1,C2]) # passive
    <=> select(Col, [C1,C2], [NewCol])
    | rvc((Row,Val),[NewCol]), fix_rcv(Row,NewCol,Val).

remove @ propagate,
    remove_rcv(Row,Col,Val) \ cell((Row,Col),Values) # passive
    <=> select(Val,Values,NewValues)
    | cell((Row,Col),NewValues).
remove @ propagate,
    remove_rcv(Row,Col,Val) \ rvc((Row,Val),Cols) # passive
    <=> select(Col,Cols,NewCols)
    | rvc((Row,Val),NewCols).
remove_rcv(_,_,_) <=> true.

% Check empty
check_empty @ propagate, rvc((_,_),[]) <=> false.
check_empty @ propagate, cell((_,_),[]) <=> false.

% Simple Constraints
alldifferent_in_row @ propagate,
    cell((Row, ColA), [Value]), cell((Row,ColB), [Value]) # passive
    <=> ColA \= ColB
    | false.
alldifferent_in_column @ propagate,
    cell((RowA, Col), [Value]), cell((RowB,Col), [Value]) # passive
    <=> RowA \= RowB
    | false.
alldifferent_in_box @ propagate,
    cell((Row,Col), [Value]), cell((ORow,OCol), [Value]) # passive
    <=> (Row \= ORow ; Col \= OCol), same_box(Row, Col, ORow, OCol)
    | false.

% Alternative Constraints
no_double_booking @ propagate,
    rvc((Row,ValA),[Col]), rvc((Row,ValB),[Col]) # passive
    <=> ValA \= ValB
    | false.
alldifferent_in_row @ propagate,
	rvc((Row,Value),[ColA]), rvc((Row,Value),[ColB]) # passive
	<=> ColA \= ColB
    | false.
alldifferent_in_col @ propagate,
	rvc((RowA,Value),[Col]), rvc((RowB,Value),[Col]) # passive
	<=> RowA \= RowB
    | false.
alldifferent_in_box @ propagate,
	rvc((RowA,Value),[ColA]), rvc((RowB,Value),[ColB]) # passive
	<=> (RowA \= RowB ; ColA \= ColB), same_box(RowA,ColA,RowB,ColB)
	| false.

% Simple Eliminate
eliminate_in_row @ propagate,
    cell((Row,_), [Value]), cell((Row,Col), [_,_|_])
    ==> remove_rcv(Row, Col, Value).
eliminate_in_column @ propagate,
    cell((_,Col), [Value]), cell((Row,Col), [_,_|_])
    ==> remove_rcv(Row,Col, Value).
eliminate_in_box @ propagate,
    cell((RowA,ColA), [Value]), cell((RowB,ColB), [_, _ | _])
    ==> (RowA \= RowB ; ColA \= ColB), same_box(RowA, ColA, RowB, ColB)
    | remove_rcv(RowB, ColB, Value).

% Alternative Eliminate
eliminate_in_row @ propagate,
	%same row, different values, one has exact col, eliminate that col from the other
	rvc((Row,ValA),[ColA]), rvc((Row,ValB),[_,_|_])
	==> ValA \= ValB
    | remove_rcv(Row,ColA,ValB).
eliminate_in_col @ propagate,
	%different row, same value, one has exact col, eliminate that col from the other
	rvc((RowA,Val),[ColA]), rvc((RowB,Val),[_,_|_])
	==> RowA \= RowB
    | remove_rcv(RowB,ColA,Val).
%eliminate_in_box @ propagate,
	% two same values, one has exact col and row, the other only row
	% check for the other all the cols that would put it in the same box
	% remove these collumns from it's possible cols
%	rvc((RowA,Val),[ColA]), rvc((RowB,Val),[C1,C2|Cs])
%	==> same_box_cols(RowA,ColA,RowB,[C1,C2|Cs],SameBoxCols),
%		SameBoxCols = [_|_] %at least one same col
%   | remove_rcv_all(RowB, SameBoxCols, Val).
remove_rcv_all([]).
remove_rcv_all(Row, [Col|Rest], Value) :-
    remove_rcv(Row,Col,Value),
    remove_rcv_all(Row,Rest,Value).

propagate <=> search(2).

first_fail @ cell((Row,Col), Vs) # passive \ search(N)
    <=> length(Vs, N)
    | member(V, Vs), fix_rcv(Row,Col,V), propagate.

first_fail @ rvc((Row,Val), Cs) # passive \ search(N)
	<=> length(Cs, N)
    | member(C, Cs), fix_rcv(Row,C,Val), propagate.

search(N) <=> nb_getval(width, Width), N == Width | true.
search(N) <=> NN is N + 1, search(NN).

cleanup \ cell(_, _) <=> true.
cleanup \ rvc(_, _) <=> true.
cleanup <=> true.
