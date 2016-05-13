write_board(Board) :-
	( foreach(Row, Board)
	do
		write(Row), nl
	), nl.
