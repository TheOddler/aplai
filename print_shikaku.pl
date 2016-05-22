% Symbols: every corner has 4 exits  N E S W corresponding to bits 1248
%

init_format(unicode).
init_format(ascii).
init_format(latex) :-
    write('% \\usepackage{pmboxdraw}'), nl.

corner_symbol(unicode, 0, ' ').
corner_symbol(unicode, 1, '?').
corner_symbol(unicode, 2, '?').
corner_symbol(unicode, 3, '└').  % NE
corner_symbol(unicode, 4, '?').
corner_symbol(unicode, 5, '│'). % NS
corner_symbol(unicode, 6, '┌'). % ES
corner_symbol(unicode, 7, '├'). % NES
corner_symbol(unicode, 8, '?').
corner_symbol(unicode, 9, '┘'). % WN
corner_symbol(unicode, 10, '─'). % EW
corner_symbol(unicode, 11, '┴'). % NEW
corner_symbol(unicode, 12, '┐'). % SW
corner_symbol(unicode, 13, '┤'). % NSW
corner_symbol(unicode, 14, '┬'). % ESW
corner_symbol(unicode, 15, '┼'). % NESW

corner_symbol(ascii, 0, ' ').
corner_symbol(ascii, 1, '?').
corner_symbol(ascii, 2, '?').
corner_symbol(ascii, 3, '+').  % NE
corner_symbol(ascii, 4, '?').
corner_symbol(ascii, 5, '|'). % NS
corner_symbol(ascii, 6, '+'). % ES
corner_symbol(ascii, 7, '+'). % NES
corner_symbol(ascii, 8, '?').
corner_symbol(ascii, 9, '+'). % WN
corner_symbol(ascii, 10, '-'). % EW
corner_symbol(ascii, 11, '+'). % NEW
corner_symbol(ascii, 12, '+'). % SW
corner_symbol(ascii, 13, '+'). % NSW
corner_symbol(ascii, 14, '+'). % ESW
corner_symbol(ascii, 15, '+'). % NESW

corner_symbol(latex, 0, ' ').
corner_symbol(latex, 1, '?').
corner_symbol(latex, 2, '?').
corner_symbol(latex, 3, '\\textSFii').  % NE
corner_symbol(latex, 4, '?').
corner_symbol(latex, 5, '\\textSFxi'). % NS
corner_symbol(latex, 6, '\\textSFi'). % ES
corner_symbol(latex, 7, '\\textSFviii'). % NES
corner_symbol(latex, 8, '?').
corner_symbol(latex, 9, '\\textSFiv'). % WN
corner_symbol(latex, 10, '\\textSFx'). % EW
corner_symbol(latex, 11, '\\textSFvii'). % NEW
corner_symbol(latex, 12, '\\textSFiii'). % SW
corner_symbol(latex, 13, '\\textSFix'). % NSW
corner_symbol(latex, 14, '\\textSFvi'). % ESW
corner_symbol(latex, 15, '\\textSFv'). % NESW

corner_symbol(N, S) :- corner_symbol(u, N, S).

select_from_list(X, [X|R], R).
select_from_list(X, [H|R], [H|S]) :-
    select_from_list(X, R, S).


update_bit_list(Key, Value, ListIn, [(Key, NewValue)|Rest]) :-
    select_from_list((Key, CurrentValue), ListIn, Rest),
    !,
    NewValue is CurrentValue \/ Value.
update_bit_list(Key, Value, ListIn, [(Key, Value)|ListIn]).

% show :- show(unicode).
%
% show(Format) :-
%     findall(Rect, (find_chr_constraint(Rect), Rect = rect(_,_,_)), Rectangles),
%     findall((X,Y,N), (find_chr_constraint(Hint), (Hint = hint(c(X,Y),N); Hint = h(c(X,Y),N))), Hints),
%     find_chr_constraint(board(s(BoardW, BoardH))),
%     show(BoardW, BoardH, Hints, Rectangles, Format).

show(BoardW, BoardH, Hints, Rectangles) :-
    show(BoardW, BoardH, Hints, Rectangles, unicode).

show(BoardW, BoardH, Hints, chr, Format) :-
    !,
    findall(rect(A,B,C), find_chr_constraint(rect(A,B,C)), Rectangles),
    show(BoardW, BoardH, Hints, Rectangles, Format).

show(BoardW, BoardH, Hints, Rectangles, Format) :-
    init_format(Format),
    collect_lines([rect(_, c(1,1), s(BoardW, BoardH))|Rectangles], [], Corners, [], Edges),
    \+((xbetween(1, BoardH, Y), \+((
        draw_line(1, Y, BoardW, Corners, Edges, Format),
        draw_row(1, Y, BoardW, Hints, Edges, Format)
    )))),
    Y1 is BoardH + 1,
    draw_line(1, Y1, BoardW, Corners, Edges, Format).

draw_line(X, Y, W, Corners, _, Format) :-
    X =:= W + 1,
    !,
    ( member(((X, Y), V), Corners) ->
        corner_symbol(Format, V, Symbol),
        write(Symbol), nl
    ;
        write('   '), nl
    ).

draw_line(X, Y, W, Corners, Edges, Format) :-
    ( member(((X, Y), V), Corners) ->
        corner_symbol(Format, V, Symbol),
        write(Symbol)
    ;
        write(' ')
    ),
    ( member(((h, X, Y), VE), Edges) ->
        corner_symbol(Format, VE, SymbolE),
        write(SymbolE), write(SymbolE), write(SymbolE)
    ;
        write('   ')
    ),
    X1 is X + 1,
    draw_line(X1, Y, W, Corners, Edges, Format).

draw_row(X, Y, W, _, Edges, Format) :-
    X =:= W + 1,
    !,
    ( member(((v, X, Y), V), Edges) ->
        corner_symbol(Format, V, Symbol),
        write(Symbol), nl
    ;
        write(' '), nl
    ).

draw_row(X, Y, W, Hints, Edges, Format) :-
    ( member(((v, X, Y), VE), Edges) ->
        corner_symbol(Format, VE, SymbolE),
        write(SymbolE)
    ;
        write(' ')
    ),
    ( member((X, Y, V), Hints) ->
        write_hint(V)
    ;
        write(' . ')
    ),
    X1 is X + 1,
    draw_row(X1, Y, W, Hints, Edges, Format).

collect_lines([], Corners, Corners, Edges, Edges).
collect_lines([rect(_, c(X,Y), s(W,H))|Rest], CornersIn, CornersOut, EdgesIn, EdgesOut) :-
    collect_lines_hline(X, Y, W, CornersIn, Corners1, EdgesIn, Edges1),
    YH is Y+H,
    collect_lines_hline(X, YH, W, Corners1, Corners2, Edges1, Edges2),
    collect_lines_vline(X, Y, H, Corners2, Corners3, Edges2, Edges3),
    XW is X+W,
    collect_lines_vline(XW, Y, H, Corners3, Corners4, Edges3, Edges4),
    collect_lines(Rest, Corners4, CornersOut, Edges4, EdgesOut).

collect_lines_hline(X, Y, Len, CornersIn, CornersOut, EdgesIn, EdgesOut) :-
    update_bit_list((X,Y), 2, CornersIn, Corners1),   % start point: E = 2
    update_bit_list((h, X,Y), 10, EdgesIn, Edges1),   % start point: E = 2
    Len1 is Len - 1,
    X1 is X + 1,
    collect_lines_hline1(X1, Y, Len1, Corners1, CornersOut, Edges1, EdgesOut).

collect_lines_hline1(X, Y, 0, CornersIn, CornersOut, EdgesIn, EdgesIn) :-
    !,
    update_bit_list((X,Y), 8, CornersIn, CornersOut).   % end point: W = 8

collect_lines_hline1(X, Y, Len, CornersIn, CornersOut, EdgesIn, EdgesOut) :-
    update_bit_list((X,Y), 10, CornersIn, Corners1),   % horizontal: WE = 10
    update_bit_list((h, X,Y), 10, EdgesIn, Edges1),   % start point: E = 2
    Len1 is Len - 1,
    X1 is X + 1,
    collect_lines_hline1(X1, Y, Len1, Corners1, CornersOut, Edges1, EdgesOut).

collect_lines_vline(X, Y, Len, CornersIn, CornersOut, EdgesIn, EdgesOut) :-
    update_bit_list((X,Y), 4, CornersIn, Corners1),   % start point: S = 4
    update_bit_list((v, X,Y), 5, EdgesIn, Edges1),   % start point: E = 2
    Len1 is Len - 1,
    Y1 is Y + 1,
    collect_lines_vline1(X, Y1, Len1, Corners1, CornersOut, Edges1, EdgesOut).

collect_lines_vline1(X, Y, 0, CornersIn, CornersOut, EdgesIn, EdgesIn) :-
    !,
    update_bit_list((X,Y), 1, CornersIn, CornersOut).   % end point: N = 1

collect_lines_vline1(X, Y, Len, CornersIn, CornersOut, EdgesIn, EdgesOut) :-
    update_bit_list((X,Y), 5, CornersIn, Corners1),   % horizontal: NS = 5
    update_bit_list((v, X,Y), 5, EdgesIn, Edges1),   % start point: E = 2
    Len1 is Len - 1,
    Y1 is Y + 1,
    collect_lines_vline1(X, Y1, Len1, Corners1, CornersOut, Edges1, EdgesOut).

write_hint(Hint) :-
    Hint < 10, !,
    write(' '), write(Hint), write(' ').

write_hint(Hint) :-
    Hint < 100, !,
    write(' '), write(Hint).

write_hint(Hint) :-
    write(Hint).

xbetween(A, B, A) :- A =< B.
xbetween(A, B, X) :-
    A < B,
    A1 is A + 1,
    xbetween(A1, B, X).
