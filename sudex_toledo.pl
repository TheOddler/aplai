puzzles(P,verydifficult) :- verydifficult(P).
puzzles(P,expert) :- expert(P).
puzzles(P,lambda) :- lambda(P).
puzzles(P,hard17) :- hard17(P).
puzzles(P,symme) :- symme(P).
puzzles(P,eastermonster) :- eastermonster(P).
puzzles(P,tarek_052) :- tarek_052(P).
puzzles(P,goldennugget) :- goldennugget(P).
puzzles(P,coloin) :- coloin(P).
puzzles(P,extra1) :- extra1(P).
puzzles(P,extra2) :- extra2(P).
puzzles(P,extra3) :- extra3(P).
puzzles(P,extra4) :- extra4(P).
%puzzles(P) :- fin(P).   % is the same as inkara2012
puzzles(P,inkara2012) :- inkara2012(P).
puzzles(P,clue18) :- clue18(P).
puzzles(P,clue17) :- clue17(P).
puzzles(P,sudowiki_nb28):- sudowiki_nb28(P).
puzzles(P,sudowiki_nb49):- sudowiki_nb49(P).
puzzles(P,sudoku_16x16):- sudoku_16x16(P).
puzzles(P,sudoku_25x25):- sudoku_25x25(P).
%De standaard online
% http://www.standaard.be/artikel/detail.aspx?artikelid=DMF20120629_074
% Fin bedenkt de moeilijkste sudoku ter wereld
/*fin(P) :- P =    see inkara2012
	[[8,_,_,_,_,_,_,_,_],
	[_,_,3,6,_,_,_,_,_],
	[_,7,_,_,9,_,2,_,_],

	[_,5,_,_,_,7,_,_,_],
        [_,_,_,_,4,5,7,_,_],
	[_,_,_,1,_,_,_,3,_],

	[_,_,1,_,_,_,_,6,8],
	[_,_,8,5,_,_,_,1,_],
	[_,9,_,_,_,_,4,_,_]].


*/
verydifficult(P) :- P =
	[[_,_,8,7,_,_,_,_,6],
	[4,_,_,_,_,9,_,_,_],
	[_,_,_,5,4,6,9,_,_],

	[_,_,_,_,_,3,_,5,_],
	[_,_,3,_,_,7,6,_,_],
	[_,_,_,_,_,_,_,8,9],

	[_,7,_,4,_,2,_,_,5],
	[8,_,_,9,_,5,_,2,3],
	[2,_,9,3,_,8,7,6,_]].

	/*	Solution of very diff:
	5	9	8	7	3	1	2	4	6
	4	3	6	2	8	9	5	7	1
	1	2	7	5	4	6	9	3	8
	6	8	2	1	9	3	4	5	7
	9	4	3	8	5	7	6	1	2
	7	1	5	6	2	4	3	8	9
	3	7	1	4	6	2	8	9	5
	8	6	4	9	7	5	1	2	3
	2	5	9	3	1	8	7	6	4 */


expert(P) :- P =
	[[_,_,_,_,_,_,8,_,5],
	[3,_,_,_,2,6,_,_,7],
	[4,_,_,_,_,3,_,_,_],

	[_,_,_,_,_,8,_,_,_],
	[7,_,_,_,9,_,_,_,8],
	[9,_,8,3,_,4,_,6,_],

	[2,3,_,9,_,_,_,_,_],
	[_,_,_,_,_,_,_,9,6],
	[_,6,_,2,_,_,_,5,_]].

	/* Sotuion of expert:
		6	7	2	1	4	9	8	3	5
		3	9	5	8	2	6	1	4	7
		4	8	1	7	5	3	6	2	9
		5	2	3	6	1	8	9	7	4
		7	4	6	5	9	2	3	1	8
		9	1	8	3	7	4	5	6	2
		2	3	4	9	6	5	7	8	1
		8	5	7	4	3	1	2	9	6
		1	6	9	2	8	7	4	5	3
	*/

lambda(P) :- P =
        [[1,_,_, _,_,_, _,_,_],
         [_,_,2, 7,4,_, _,_,_],
         [_,_,_, 5,_,_, _,_,4],

         [_,3,_, _,_,_, _,_,_],
         [7,5,_, _,_,_, _,_,_],
         [_,_,_, _,_,9, 6,_,_],

         [_,4,_, _,_,6, _,_,_],
         [_,_,_, _,_,_, _,7,1],
         [_,_,_, _,_,1, _,3,_]].


hard17(P) :- P =
	[[_,_,2,_,9,_,3,_,_],
	[8,_,5,_,_,_,_,_,_],
	[1,_,_,_,_,_,_,_,_],

	[_,9,_,_,6,_,_,4,_],
	[_,_,_,_,_,_,_,5,8],
	[_,_,_,_,_,_,_,_,1],

	[_,7,_,_,_,_,2,_,_],
	[3,_,_,5,_,_,_,_,_],
	[_,_,_,1,_,_,_,_,_]].

symme(P) :- P =
	[[3,_,_,_,_,_,_,_,4],
	[_,8,_,2,_,_,_,7,_],
	[_,_,6,_,_,_,5,_,_],

	[_,1,_,9,_,8,_,_,_],
	[_,_,_,_,6,_,_,_,_],
	[_,_,_,_,_,7,_,2,_],

	[_,_,5,_,_,_,6,_,_],
	[_,9,_,_,_,1,_,8,_],
	[4,_,_,_,_,_,_,_,3]].

eastermonster(P) :- P =
	[[1,_,_,_,_,_,_,_,2],
	[_,9,_,4,_,_,_,5,_],
	[_,_,6,_,_,_,7,_,_],

	[_,5,_,9,_,3,_,_,_],
	[_,_,_,_,7,_,_,_,_],
	[_,_,_,8,5,_,_,4,_],

	[7,_,_,_,_,_,6,_,_],
	[_,3,_,_,_,9,_,8,_],
	[_,_,2,_,_,_,_,_,1]].


tarek_052(P) :- P =
	[[_,_,1,_,_,4,_,_,_],
	[_,_,_,_,6,_,3,_,5],
	[_,_,_,9,_,_,_,_,_],

	[8,_,_,_,_,_,7,_,3],
	[_,_,_,_,_,_,_,2,8],
	[5,_,_,_,7,_,6,_,_],

	[3,_,_,_,8,_,_,_,6],
	[_,_,9,2,_,_,_,_,_],
	[_,4,_,_,_,1,_,_,_]].

goldennugget(P) :- P =
	[[_,_,_,_,_,_,_,3,9],
	[_,_,_,_,_,1,_,_,5],
	[_,_,3,_,5,_,8,_,_],

	[_,_,8,_,9,_,_,_,6],
	[_,7,_,_,_,2,_,_,_],
	[1,_,_,4,_,_,_,_,_],

	[_,_,9,_,8,_,_,5,_],
	[_,2,_,_,_,_,6,_,_],
	[4,_,_,7,_,_,_,_,_]].

coloin(P) :- P =
	[[_,2,_,4,_,3,7,_,_],
	[_,_,_,_,_,_,_,3,2],
	[_,_,_,_,_,_,_,_,4],

	[_,4,_,2,_,_,_,7,_],
	[8,_,_,_,5,_,_,_,_],
	[_,_,_,_,_,1,_,_,_],

	[5,_,_,_,_,_,9,_,_],
	[_,3,_,9,_,_,_,_,7],
	[_,_,1,_,_,8,6,_,_]].


hardest(P) :- P =
       [[1,_,_,_,_,_,_,_,2],
        [_,9,_,4,_,_,_,5,_],
        [_,_,6,_,_,_,7,_,_],

        [_,5,_,9,_,3,_,_,_],
        [_,_,_,_,7,_,_,_,_],
        [_,_,_,8,5,_,_,4,_],

        [7,_,_,_,_,_,6,_,_],
        [_,3,_,_,_,9,_,8,_],
        [_,_,2,_,_,_,_,_,1]].

extra1(P) :- P = [
    [_, 9, 8, _, _, _, _, _, _],
    [_, _, _, _, 7, _, _, _, _],
    [_, _, _, _, 1, 5, _, _, _],
    [1, _, _, _, _, _, _, _, _],
    [_, _, _, 2, _, _, _, _, 9],
    [_, _, _, 9, _, 6, _, 8, 2],
    [_, _, _, _, _, _, _, 3, _],
    [5, _, 1, _, _, _, _, _, _],
    [_, _, _, 4, _, _, _, 2, _]].

extra2(P) :- P = [
    [_, _, 1, _, 2, _, 7, _, _],
    [_, 5, _, _, _, _, _, 9, _],
    [_, _, _, 4, _, _, _, _, _],
    [_, 8, _, _, _, 5, _, _, _],
    [_, 9, _, _, _, _, _, _, _],
    [_, _, _, _, 6, _, _, _, 2],
    [_, _, 2, _, _, _, _, _, _],
    [_, _, 6, _, _, _, _, _, 5],
    [_, _, _, _, _, 9, _, 8, 3]].


extra3(P) :-  P = [
    [1, _, _, _, _, _, _, _, _],
    [_, _, 2, 7, 4, _, _, _, _],
    [_, _, _, 5, _, _, _, _, 4],
    [_, 3, _, _, _, _, _, _, _],
    [7, 5, _, _, _, _, _, _, _],
    [_, _, _, _, _, 9, 6, _, _],
    [_, 4, _, _, _, 6, _, _, _],
    [_, _, _, _, _, _, _, 7, 1],
    [_, _, _, _, _, 1, _, 3, _]].

extra4(P) :- P = [
    [1, _, 4, _, _, _, _, _, _],
    [_, _, 2, 7, 4, _, _, _, _],
    [_, _, _, 5, _, _, _, _, _],
    [_, 3, _, _, _, _, _, _, _],
    [7, 5, _, _, _, _, _, _, _],
    [_, _, _, _, _, 9, 6, _, _],
    [_, 4, _, _, _, 6, _, _, _],
    [_, _, _, _, _, _, _, 7, 1],
    [_, _, _, _, _, 1, _, 3, _]].


% inkara2012
% from: http://www.telegraph.co.uk/science/science-news/9359579/Worlds-hardest-sudoku-can-you-crack-it.html
inkara2012(P) :- P = [
	[8,_,_,   _,_,_,   _,_,_],
	[_,_,3,   6,_,_,   _,_,_],
	[_,7,_,   _,9,_,   2,_,_],

	[_,5,_,   _,_,7,   _,_,_],
	[_,_,_,   _,4,5,   7,_,_],
	[_,_,_,   1,_,_,   _,3,_],

	[_,_,1,   _,_,_,   _,6,8],
	[_,_,8,   5,_,_,   _,1,_],
	[_,9,_,   _,_,_,   4,_,_]].


clue18(P) :- P = [
	[7,_,8,   _,_,_,   3,_,_],
	[_,_,_,   2,_,1,   _,_,_],
	[5,_,_,   _,_,_,   _,_,_],

	[_,4,_,   _,_,_,   _,2,6],
	[3,_,_,   _,8,_,   _,_,_],
	[_,_,_,   1,_,_,   _,9,_],

	[_,9,_,   6,_,_,   _,_,4],
	[_,_,_,   _,7,_,   5,_,_],
	[_,_,_,   _,_,_,   _,_,_]].

% from:
% http://school.maths.uwa.edu.au/~gordon/sudokumin.php

clue17(P) :- P = [
	[_,_,_,   _,_,_,   _,1,_],
	[4,_,_,   _,_,_,   _,_,_],
	[_,2,_,   _,_,_,   _,_,_],

	[_,_,_,   _,5,_,   4,_,7],
	[_,_,8,   _,_,_,   3,_,_],
	[_,_,1,   _,9,_,   _,_,_],

	[3,_,_,   4,_,_,   2,_,_],
	[_,5,_,   1,_,_,   _,_,_],
	[_,_,_,   8,_,6,   _,_,_]].

%http://www.sudokuwiki.org/Weekly_Sudoku.asp?puz=28
sudowiki_nb28(P) :- P = [
	[6,_,_,_,_,8,9,4,_],
	[9,_,_,_,_,6,1,_,_],
	[_,7,_,_,4,_,_,_,_],

	[2,_,_,6,1,_,_,_,_],
	[_,_,_,_,_,_,2,_,_],
	[_,8,9,_,_,2,_,_,_],

	[_,_,_,_,6,_,_,_,5],
	[_,_,_,_,_,_,_,3,_],
	[8,_,_,_,_,1,6,_,_]].

sudowiki_nb49(P) :- P = [
	[_,_,2,8,_,_,_,_,_],
	[_,3,_,_,6,_,_,_,7],
	[1,_,_,_,_,_,_,4,_],

	[6,_,_,_,9,_,_,_,_],
	[_,5,_,6,_,_,_,_,9],
	[_,_,_,_,5,7,_,6,_],

	[_,_,_,3,_,_,1,_,_],
	[_,7,_,_,_,6,_,_,8],
	[4,_,_,_,_,_,_,2,_]].

/*
veryeasy(P) :- P = [
	[_,_,_,_,_,_,_,_,_],
	[_,_,_,_,_,_,_,_,_],
	[_,_,_,_,_,_,_,_,_],

	[_,_,_,_,_,_,_,_,_],
	[_,_,_,_,_,_,_,_,_],
	[_,_,_,_,_,_,_,_,_],

	[_,_,_,_,_,_,_,_,_],
	[_,_,_,_,_,_,_,_,_],
	[_,_,_,_,_,_,_,_,_]].
*/

/* Source: http://planetsudoku.com/sudoku/25x25/standard/very-easy/ */
sudoku_16x16(P) :- P = [
    [12, _, _, _,  10, _, 4, _,   _,16, _, 5,   _, _, _,11],
    [ _, _, 3, _,   _, _,13, _,   _,12, _, _,   _, 9, _, _],
    [ 9, _, _, 7,   _, 1, _,12,   4, _, 3, _,   5, _, _, 8],
    [16, _,10, _,   8, _, _, _,   _, _, _, 7,   _,12, _,14],

    [15, _, 8, _,   _,14, 3, _,   _, 6, 7, _,   _,13, _,10],
    [ _, 7, 6, _,   _,13,12, 8,  11, 1,10, _,   _, 2, 4, _],
    [ _, _,13,10,   _, 9, 6, _,   _,15, 4, _,  16,11, _, _],
    [ _, 5, _, _,   2, _, _, 1,  14, _, _, 3,   _, _, 7, _],

    [ _, 2, _, _,  13, _, _, 6,   7, _, _,16,   _, _,12, _],
    [ _, _,14,16,   _, 2, 9, _,   _, 4, 1, _,  11, 7, _, _],
    [ _, 9, 4, _,   _,16, 7,15,   3, 8, 6, _,   _, 5, 1, _],
    [ 8, _, 7, _,   _, 4, 5, _,   _, 2,11, _,   _,14, _,13],

    [ 2, _,11, _,   7, _, _, _,   _, _, _,15,   _,16, _, 1],
    [ 6, _, _,15,   _,11, _, 4,  13, _,12, _,   8, _, _, 7],
    [ _, _, 5, _,   _, _, 8, _,   _,11, _, _,   _, 4, _, _],
    [ 3, _, _, _,  14, _, 2, _,   _, 7, _, 4,   _, _, _,15]].

/* Sudoku Extreme - 25x25
   Source: http://planetsudoku.com/sudoku/25x25/standard/very-easy/
   */
sudoku_25x25(P) :- P = [
    [ _,20, _, _,17,   _, _, _, 2, _,   _,14,11, 9, 1,   _, _,18, _, 5,   _,22, 8, _, _],
    [ _,13, _, _,18,   6, 7,19,10, _,  16,25, _, _, _,   _, _,11,23, 8,   _, _,21, _, _],
    [ 4, 2, _,16, 8,  21, 9,13,25,24,  12, _, _, _, 7,   _, _, _, _,15,  10, _, _,19,23],
    [ _, _,19,11, 6,   _, _, 1, _,16,   _, _,10, 8, _,   _, _, _, _, _,   _, _,13, _, _],
    [10, _, _,12, _,  14, _, _, _, _,   _, _, _,24, _,  20, 4, 9, _,25,   7, _, 1,17,18],

    [ 5,14, _, _, _,  19,16,20, _, _,   _, _, _, _, _,  18,21, _, 9, _,   _,12,11, _, 1],
    [ 9, _, _, _,10,   8,23,17, 5, 3,   4, _, _,12,16,   _, _, _, _, _,   _,24, 2, _, _],
    [ _, _,22, 7, _,   9, _, _, _, 1,  14, _, 3, _, _,   5, _, _,11,10,  21, _, _, _, _],
    [ _,12, _, _, _,   _,24, _,11, _,   _, _,15,10, _,   _,17, _, _, 7,  22,25, 5, 6, _],
    [15, _, _, _,11,   _, _,22, 7, _,   _,13,25,23,17,   _, _, 8, 6, _,   3, _, _, 9, _],

    [ _,16, _, 9, _,  15, 2, _, _,19,  25,10, _, 4, _,  24, _, _, _, _,   6,20, _, 7, 8],
    [ _,18, _, _,25,   _,12, _, 8, 7,   9, 5, _, _, _,  16, 3, 1, _,20,   _,21,22, _, _],
    [13, _, _, 1, _,   _, _,23, _, 4,   _, _, _, _, _,   _, _,10, _,22,  15, _, _, _, 3],
    [ _,21,17, 3, _,   _, _,16,24, 6,  13, _,12, _,20,   _,25, _, 7, _,   _,14, _, 5, _],
    [20,19, _, _, _,   _,22,10, _, _,   3,16, _, _, _,   4, _, _, _,14,  25, _,12, _,17],

    [ _, 7, _,21, _,   _,20, _, 3, _,  23, _, _,14,11,   _, _, _, 4, _,  18, _, _,15, _],
    [18, _, _,14,23,   2, _, _, _, _,   _, _, _, _, _,   _, _, 5, 8, _,   9, 3,19,22, _],
    [25, 8, _,24,22,   _,10, _, _, _,   _, _, _, _, _,   _, _,20, _, _,  23, _, _, _, 4],
    [17,10, _, _, _,  23, _, _,22, _,   _, _, 9,18,12,   _,16,13, _,11,  20, _, _, _, _],
    [ _, 3, _, _, _,  17,15,21, _, _,   _, _, _,22, _,   _,23, _, _, 2,  13,16,14,12,24],

    [ _, 1, _,17, _,  24, _, 9, _, _,   _, _, 6, _, _,   _, _, 2,10, _,   _, _,15,20, _],
    [21, _, _, _, _,   _, 4,14, _,22,   _, _, _, _, _,   _, _,12, _, _,   _, _,24, _,16],
    [ _, 9, _,20,12,   _, _, _, _, _,   _, _, _,13, _,   7, _,17,22, _,   8, 5, _, _,14],
    [ _,15,14, _, _,   _, 6, _,16, _,  11,20, _, _, _,   _,19, _,24, _,   _,23, 4, 3,25],
    [19, _, _, 2, 3,  12, _,15,23, _,   7,17, _, _, 5,   _, _, _,14, _,   _, _, _,11,10]].

/*
 * JavaScript code to easily get the sudoku in the right format
 * It can be used by pasting in the console window (F12 in Chrome) at the desired puzzle
 *
 * Note: With small modifications, it can also be used to get the full solution from the site
 */

/*
    var board = [],
        temp = [],
        block = Math.sqrt(gameBoard._boardMaskKey.length),
        base = Math.sqrt(block),
        prologBoard = gameBoard._gameCodeName;
    $.each(gameBoard._boardMaskKey, function(i,n) {
      if(isNaN(n)) n = (n.charCodeAt(0) - 55)+"";
      else n = " "+n;

      if((i+1)%base == 0 && (i+1)%block != 0) n = n+",   ";

      temp.push(n);
      if((i+1)%block == 0) {
        board.push(temp);
        temp = [];
      }

      if((i+1)%(block*base) ==0) board.push([]);
    });
    prologBoard += "(P) :- P = "
    prologBoard += JSON.stringify(board).replace("[[", "[\n\t[").replace("],[]]", "]].").replace(/(\],\[)/g, "],\n\t[").replace(/(\s0)/g, " _").replace(/([\"])|(\[\],)|(\s\",\")/g, '');
    console.log(prologBoard);
*/

/* Solution for 25x25
[
    [ 7,20,24,15,17,   4, 3,12, 2,23,  21,14,11, 9, 1,  10,13,18,19, 5,  16,22, 8,25, 6],
    [ 1,13, 9,22,18,   6, 7,19,10,20,  16,25, 4, 5, 3,  17, 2,11,23, 8,  24,15,21,14,12],
    [ 4, 2, 5,16, 8,  21, 9,13,25,24,  12,18,17,20, 7,  14, 6,22, 1,15,  10,11, 3,19,23],
    [14,25,19,11, 6,  22,17, 1,18,16,  15, 2,10, 8,23,  12, 7, 3,21,24,   5, 9,13, 4,20],
    [10,23, 3,12,21,  14,11, 5,15, 8,  19,22,13,24, 6,  20, 4, 9,16,25,   7, 2, 1,17,18],

    [ 5,14,23, 4,13,  19,16,20, 6,15,  22,24, 8, 7, 2,  18,21,25, 9, 3,  17,12,11,10, 1],
    [ 9, 6,21,25,10,   8,23,17, 5, 3,   4,11,20,12,16,  22, 1,19,15,13,  14,24, 2,18, 7],
    [ 2,17,22, 7,20,   9,13,25,12, 1,  14,19, 3, 6,18,   5,24, 4,11,10,  21, 8,23,16,15],
    [ 3,12,16, 8,19,  18,24, 4,11, 2,   1,21,15,10, 9,  23,17,14,20, 7,  22,25, 5, 6,13],
    [15,24, 1,18,11,  10,21,22, 7,14,   5,13,25,23,17,   2,12, 8, 6,16,   3, 4,20, 9,19],

    [12,16,11, 9,14,  15, 2, 3,17,19,  25,10, 1, 4,22,  24, 5,23,13,21,   6,20,18, 7, 8],
    [ 6,18,15,10,25,  13,12,11, 8, 7,   9, 5,14,19,24,  16, 3, 1,17,20,   4,21,22,23, 2],
    [13, 5, 7, 1, 2,  20,25,23,14, 4,   6, 8,18,17,21,  11, 9,10,12,22,  15,19,16,24, 3],
    [22,21,17, 3, 4,   1,18,16,24, 6,  13,23,12, 2,20,   8,25,15, 7,19,  11,14,10, 5, 9],
    [20,19, 8,23,24,   5,22,10, 9,21,   3,16, 7,11,15,   4,18, 6, 2,14,  25,13,12, 1,17],

    [16, 7,13,21, 1,  25,20, 8, 3, 9,  23, 6, 2,14,11,  19,22,24, 4,12,  18,10,17,15, 5],
    [18, 4, 6,14,23,   2, 1,24,13,12,  20, 7,21,16,25,  15,10, 5, 8,17,   9, 3,19,22,11],
    [25, 8,12,24,22,  16,10, 6,19,11,  17,15, 5, 3,13,  21,14,20,18, 9,  23, 1, 7, 2, 4],
    [17,10, 2,19,15,  23,14, 7,22, 5,  24, 4, 9,18,12,   1,16,13, 3,11,  20, 6,25, 8,21],
    [11, 3,20, 5, 9,  17,15,21, 4,18,  10, 1,19,22, 8,   6,23, 7,25, 2,  13,16,14,12,24],

    [23, 1,18,17,16,  24, 5, 9,21,13,   8,12, 6,25,14,   3,11, 2,10, 4,  19, 7,15,20,22],
    [21,11,10, 6, 7,   3, 4,14,20,22,  18, 9,23,15,19,  25, 8,12, 5, 1,   2,17,24,13,16],
    [24, 9,25,20,12,  11,19,18, 1,10,   2, 3,16,13, 4,   7,15,17,22,23,   8, 5, 6,21,14],
    [ 8,15,14,13, 5,   7, 6, 2,16,17,  11,20,22, 1,10,   9,19,21,24,18,  12,23, 4, 3,25],
    [19,22, 4, 2, 3,  12, 8,15,23,25,   7,17,24,21, 5,  13,20,16,14, 6,   1,18, 9,11,10]].
*/
