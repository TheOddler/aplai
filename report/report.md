% Report: Advanced Programming Languages in Artificial Intelligence
% Pablo Bollansée [r0465328], Vincent Tanghe [r0294173]
% \today

# INTRODUCTION

Constraint Logic Programming (CLP) is an augmentation of the logic programming paradigm where relations between variables are specified with constraints.
It can be seen as an extension that brings relational arithmetic to Prolog.

For the course Advanced Programming Languages in Artificial Intelligence, we are given the opportunity to get real experience with some CLP languages.
The goal of this assignment is to get practical experience with different CLP languages and to get a deeper understanding in how they work.
This will be done by solving two different puzzle games with the languages two of three thought languages: _ECLiPSe_, _Constraint Handling Rules_ (_CHR_) and _Jess_.

First we will do a short discussion of the CLP languages, explaining which we chose and why.
We then discuss of our solver for the popular puzzle game Sudoku in both chosen languages, followed by a discussion our approach for solving Shikaku.
Finally we will end with a conclusion regarding the assignment as a whole.

# Constraint Programming languages

## ECLiPSE

_ECLiPSe_ is an Open-Source, Prolog-based system for the development and deployment of Constraint Programming applications.
It is largely backwards compatible with Prolog, which means that a large set of the Prolog libraries are still be available.
It also includes its own development environment _TkEclipse_, which includes some tools for debugging.
It also has a fairly active community, which means that questions asked on popular forums (e.g. StackOverflow) will get answered within a reasonable amount of time.

_ECLiPSe_ extends Prolog by providing a core language for specifying constraints and relations between variables.
It also introducing new data types and structures such as arrays and for-loops, which make the programming easier even though for-loops in a declarative language might seem weird.
It is declarative, easy to understand and it has a built-in search mechanism that supports many different parameters.
The running time, however, is highly dependent on the selected parameters (which define the heuristics used during the search).
Because of its slight differences with Prolog and the way it handles lists, it can be confusing for programmers that are new to the language.
Debugging can be a troublesome, as with any declarative language as there isn't always a clear path trough the code, however the _TkEclipse_ debugging tools relieve this problem a bit.

_ECLiPSe_ can be run using a more traditional command-line interface or a more interactive Graphical User Interface (GUI) tkeclipse.
But don't let the looks deceive you, the GUI works fairly well.

## CHR

Constraint Handling Rules (CHR) allows for multiple rules to be defined in a certain order to simplify and propagate of multi-relation sets.
It also allows you to define redundant rules that may further simplify the possibility space.
CHR consists of multiple head atoms, followed by guarded rules.

CHR is available in different languages, such as Java, Haskell and C.
However, we will use the recommended Prolog implementation (SWI-prolog), which also means the SWI-Prolog editor and several prolog libraries become available.
CHR is free to download and has some online tutorials.
Unfortunately, most of them refer to exactly the same examples.

## Jess

Jess is a rule engine for the Java platform. It has a GUI based on the open-source Eclipse IDE, which -in combination with friendly user-friendly error messages, should make the development easier. It is free for academic use, but a license is needed for commercial use. And it uses its own declarative XML language (JessML) which offers a lot of the Java perks, such as regular expressions and Java object manipulation.

However, the minor amount of available documentation, the trouble to get a working installation, the fact that it had not yet been introduced to us at the time of coding and the fact that it uses yet another language (JessML) instead of the already known Prolog syntax made Jess a less practical choice.
Besides these practicalities, we found that both Jess and CHR are good for expressing rules, but have a lesser support for search compared to _ECLiPSe_. We found that this is even worse for Jess, where delegating to host language (Java) is a common implementation.

## Which did we choose?

For this assignment we chose **ECLiPSE** and **CHR** to make our solvers in.
The main reasons for this are that ECLiPSE and CHR seem to have a superior amount of documentation and a more active community.

The course was mainly focused on **ECLiPSE** so this was an easy choice, as this allowed us to start working on the assignment much sooner.
Both **ECLiPSE** and **CHR** are also implemented on Prolog, which also makes working in both languages simultaneously much more enjoyable.

When looking at the puzzles we had to solve for this assignment, Sudoku and Shikaku, **ECLiPSE** has some very useful features:

* Both puzzles require some search, as there are no known set of constraints that can solve any puzzle, and ECLiPSE has very nice support for this with very powerful propagation support.
* The build-in array syntax gave a very natural way of reasoning about the constraints and specifying them.
* The for-loop support gave a very nice way to specify constraints on the cells
* The `ic` libary provided some very nice constraints (e.i. alldifferent)

There are however also some drawbacks:

* Some functions require grounded variables, and it is not always immediately clear which these are. ECLiPSe allows variables to have a domian, but Prolog doesn't always agree with this, which can be confusing at times.
* The required running time of a program can vary greatly on only slight changes in the code. As there is no way, to our knowledge, to see the propagation that is happening, nor are there any profiling tools, it is very hard to optimize the constraints.

**CHR** is much more basic than ECLiPSE.
It requires much more to be hand-crafted.
However since it is implemented on Prolog you do get some very nice features.
Mainly backtracking is very useful during search, as this also backtracks the changes that happened in the **CHR** constraint-store, allowing for a relatively easy way to implement a simple search strategy.
So any rules of a puzzle are easily implemented, however once search is required CHR is harder to use.

We didn't use **Jess** as this has the same problems as CHR, but doesn't have the nice features of Prolog and was only touched on very briefly at the end of the course.

# SUDOKU

## TASK-DESCRIPTION

Sudoku is a logic puzzle game.
It consists of a a 9 by 9 board, divided in 3 by 3 blocks.
Each cell must contain exactly one number between 1 and 9, and at the start some of these are filled in.
Each column, row and block must contain all different numbers, so it's up to the player to find a configuration that works within these constraints (without of course changing the given numbers).

An alternative version of Sudoku works on any N by N grid.
We implemented a version that supports any grid of `N = X^2`, where X is a whole number.
So for instance, we support grids of 16 by 16 and 25 by 25.
In the classic Sudoku there are 9 3 by 3 block, but in our case there are N X by X blocks.
We chose to do this alternative version as it also includes the _normal_ Sudoku, and gave our solution a nice extra dimension (pun intended).

In this part we will discuss the implementation of our solver for Sudoku in _ECLiPSe_ and CHR.

## VIEWPOINTS AND PROGRAMS

The __classical viewpoint__ for Sudoku is a simple 2D grid of numbers.
The rules of Sudoku then state that all numbers in a row must be different, that all numbers in a column must be different and that all numbers in a block must be different.
These are quite easy to define in _ECLiPSe_ (see later).
In this viewpoint, it comes down to mapping numbers to places.

For the __alternate viewpoint__ we propose a viewpoint where we map places, or coordinates, to the numbers.
For the classic Sudoku this means that there are 9 list of 9 coordinates, as there are 9 1's that need a place in the grid, 9 2's, ....
The rules of Sudoku are of course unchanged, however the constraints are specified slightly different for this viewpoints:
Each list must contain exactly one coordinate in each row, column, and block.
Also between lists no coordinate may exist twice.

To combine both viewpoints, we used __channeling__ by linking the board variables to both implementations.

## Eclipse implementations

### Normal view

Eclipse is very well suited for these kinds of problems.
The implementation reflects this, and is quite simple.
First we give each cell an initial domain, 1 to 9 (or 1 to N in our case).
Then, since the puzzles are already given as a grid of numbers, all we had to do was iterate over each row, column and block and add the `alldifferent` constraint from the `ic` library.
Eclipse has a very nice array syntax, so this was quite easy:

	( for(I,1,D), param(BoardArray,D) % D is the dimention of the board
	do
		Row is BoardArray[I,1..D],
		Col is BoardArray[1..D,I],
		alldifferent(Row),
		alldifferent(Col)
	).

Similarly we extract each block and add the `alldifferent` constaint.
All that's left then is starting the search.

### Alternative view

The implementation of the alternative view is very similar.
Here we use again a N by N grid, each row represents a number (which was N coordinated).
Each column represents the X coordinate, whereas the number in the array represents the Y coordinate.
Then again we use multiple `alldifferent` constraints to ensure that no numbers share spaces.

One more complex part was extracting the numbers for this view from the puzzle, and linking it to the variables.
Here we used the excellent `#=/3`.
This is a build-in that allows you to add an equality constraint based on a third boolean variable.
This works as followed:

    ( multifor([I,R,C], [1,1,1], [N,N,N]),
        param(BoardArray, Coordinates)
    do
        #=(BoardArray[R,C], I, B),
        #=(Coordinates[I,C], R, B)
    ).

The `BoardArray` is the puzzle (simple 2D array with numbers), where the `Coordinates` is our alternative view (2D array with the numbers representing Y coordinates).
This code basically makes sure than when a number on position `Row,Col` is equal to `I` in the puzzle, that the number on position `I,Col` is equal to `Row`.

### Channeling

To do channeling the constraints from both viewpoints need to be 'linked' to each other, so that if one variable's domain changes this can be propagated to the variables of the other viewpoint.
For our _ECLiPSe_ implementation this was very easy.
Both viewpoints' variables are already linked with the variables of the given puzzle, and constraints added on them.
The original view unifies their variables with those in the puzzle, where our alternative links them as described above.
This made it so that both viewpoints actually already are channeled to the puzzle.
To channel both viewpoints all we had to do was let them both add their constraints to the same puzzle and then let _ECLiPSe_ search on the combined variables.

## CHR implementation

...

// TODO

### Channeling

// TODO
Hier was het moeilijker, hier is wel echt apparte file met dubbele code erin hiervoor.

## EXPERIMENTS SET-UP

To make the testing easier, we created a file "sudoku_eclipse_channeling" where the classical viewpoint, the alternate viewpoint and a combination using channeling can be called.
The file defines a method solve_all/0 which will loop over all the different Sudokus defined in the sudex_toledo, using default channeling method.
You can also solve a specific puzzle by using solve/2 using the puzzle name and model ('simple', 'alt' or 'both') as arguments.
Every time a solve method is called, the name of the method, name of the puzzle, running time, backtracks and solution is outputted.

*__Note:__ Since we implemented this to work for any NxN Sudokus we added two bigger Sudokus: a 16 by 16 and a 25 by 25 puzzle to the sudex_toledo file.*

## RESULTS

### ECLiPSe

![Results of Sudoku ECLiPSE alternate implementation\label{sudoku_clp_chart2}](images/sudoku_clp_chart2.png "Results of Sudoku ECLiPSE alternate implementation")

Voorbeeld reference: \ref{voorbeeldLabel}. Je maakt dan een label aan zoals hier boven (in md)!

![Comparison between ECLiPSE and Channeling\label{sudoku_clp_chart3}](images/sudoku_clp_chart3.png "Comparison between ECLiPSE and Channeling")

**CHR**

![Results of Sudoku CHR implementation. Note II: we cut off the Channeling at 200 sec in soduwiki_nb28 (with a value of 938,76 sec) deformed the chart. Note II: the alternative version wasn't able to solve the 25x25 within a reasonable time\label{sudoku_chr_all}](images/sudoku_chr_all.png "Results of Sudoku CHR implementation")

## DISCUSSION
**The _ECLiPSe_ implementation:**

We can see that the classical view is faster than the other methods. However, there is one puzzle (Extra 2) where the power of channeling really does speed up the process.
We can also noticed that only the classical viewpoint uses backtracks.
It could also mean that the constraints are stronger in the alternate and with channeling, meaning that only the correct numbers are found for each cell. But, because of the long running times, we think that there could be ghost backtracks which are not captured by the program.

**The *CHR* implementation:**

We can immediately see that the results of CHR are a lot slower than those from _ECLiPSe_. But, there are some exceptions where the alternative in CHR performs better (e.g. Clue 17). We can see that the most difficult problems remain the same for both CHR and _ECLiPSe_ (e.g. sudowiki_nb28) and that in both cases the alternative outperforms the simple at extra2. Because of the high amount of backtracks in _ECLiPSe_ and inferences in CHR, we assume this has to do with the order of the propagation being less favorable.

**Conclusion**

The first thing we notice, solely based on our experiments, is that the implementation in _ECLiPSe_ is way faster than the CHR implementation. The combination with channeling is slower than CHR but only in one case faster than the _ECLiPSe_ implementation.

Personally, we felt it was easier to implement in _ECLiPSe_ rather than CHR.

// TODO: Ik zou precies die bespreking en de afbeeldingen samen zetten. En mag meer besproken worden denkik.

# SHIKAKU

## TASK-DESCRIPTION

Shikaku is a puzzle game where the board consists of a grid with in some cells a number.
To solve the puzzle, you need to transform each number into a rectangle with a surface that equals the number, so that none of the rectangles overlap and the whole board is filled.

The assignment stated that we had to create a single viewpoint.
However during the implementation we tried a second slightly different one as well, as we were not satisfied with the speed of our original viewpoint.

In this part we will discuss both viewpoints and the implementation of our solvers for Shikaku in _ECLiPSe_ and CHR.

## VIEWPOINTS AND PROGRAMS

The __classical viewpoint__ for Shikaku is one where all squares are defined by a top-left coordinate, a width and height.
They must be inside the grid, contain the number, have an area equal to that number and can't overlap.

As an __alternate viewpoint__ (alt) we propose a viewpoint where instead of a top-left coordinate, width and height, we define the top, left, bottom and right coordinate.
This viewpoint is very similar but, as we will show, can give much better results in term of speed.

## Redundant constraint

As per the assignment we introduced a redundant constraint to improve the solution.
In it's simplest form it is not needed to specify that each rectangle can only contain a single number.
Since rectangles can't overlap, and each rectangle contains one of those numbers, it is implied that each rectangle contains exactly one number.
So as an additional constraint we **explicitly** specify that each rectangle contains exactly one number, rather than implicitly through other constraints.

### ECLiPSe implementation

We defined them as rect(Point, c(X,Y))
... ... ... TODO

### CHR implementation

TODO

## EXPERIMENTS SET-UP

The set-up is similar as in  the Sudoku set-up, however here we didn't do channeling (as normally there wouldn't even have been two viewpoints) and the puzzles are in a different file (puzzles.pl).
When opening the CHR or eclipse implementation in the correct GUI, the solve/1 or solve_all/0 predicates can be used to solve a specific or all puzzles respectively.

## RESULTS
**_ECLiPSe_**

![Results of Shikaku _ECLiPSe_ implementation up to problem p(4,5) and with p(4,1) cutoff (original value: 5,382) and p(4,5) (original value: 2,668)\label{shikaku_clp_chart}](images/shikaku_clp_chart.png "Results of Shikaku ECLiPSe implementation")

Puzzle | Normal time (sec) | Normal backtracks | Alt time (sec) | Alt backtracks
-------|------------------:|------------------:|---------------:|---------------:
p(5,1) | 22,090            | 2913              | 0,109          | 0
p(5,2) | 124,551           | 11754             | 0,125          | 0
p(5,3) | 201,616           | 32366             | 0,078          | 0
p(5,4) | 5,085             | 555               | 0,125          | 0
p(5,5) | 350,956           | 25996             | 0,187          | 1
p(6,1) | 177,872           | 4330              | 0,297          | 0
Table XXX: The last 6 results of Shikaku _ECLiPSe_ with the normal and the alternate viewpoint


**_CHR_**

![Results of Shikaku CHR implementation up to problem p(4,5)\label{shikaku_chr_chart}](images/shikaku_chr_chart.png "Results of Shikaku CHR implementation")

Puzzle | Normal time (sec) | Normal inferences | Alt time (sec) | Alt inferences
-------|------------------:|------------------:|---------------:|---------------:
p(5,1) | 11,668            | 82 663 847        | 10,000         | 98 574 181
p(5,2) | 13,400            | 95 476 199        | 11,335         | 112 089 372
p(5,3) | 44,311            | 273 118 213       | 193,563        | 2 280 734 323
p(5,4) | 12,982            | 92 899 324        | 11,068         | 108 855 366
p(5,5) | 57,242            | 366 411 206       | 63,839         | 725 171 392
p(6,1) | 156,980           | 982 498 648       | 124,253        | 1 354 737 953
Table XXX: The last 6 results of Shikaku CHR with the normal and the alternate viewpoint

## DISCUSSION

Because the last 6 methods had the highest time values, we cut them off and put them in a table to keep the chart ratio. By putting them in a table, we could add the backtracks or inferences respectively.

**The _ECLiPSe_ implementation:**

We can see that the alternative view is much faster than the other methods. We can also see that it contains much less backtracks, which makes it seem that the constraints are tighter in the alternative model. Which is what we believe to be the main reason for the difference in speed.

Another reason for their difference in the search tree is that there are simply different values to be found.

**The *CHR* implementation:**

Again, the CHR implementation is slower than the _ECLiPSe_, until we reach problem p(5,1) where we see that the CHR implementation is faster than the normal viewpoint of the _ECLiPSe_ implementation.

in CHR we also see that the alternative viewpoint is usually faster than the normal viewpoint. However, it is interesting to see that in CHR the difference between the different viewpoints is **much** smaller!

Another interesting thing we notice, is that the amount of inferences does not necessary reflect the time needed. A larger amount of inferences is usually in line with a longer time. But we can see that the amount of inferences in the alternative seems higher while it does achieve lower times.

**Conclusion**

The difference a different viewpoint can make really flourishes in this example, as the different viewpoint in the _ECLiPSe_ implementation has a large speed increase.


# EXTRA

While we didn't take one of the given options of the extra assignment, we did extend the given assignments:
- We implemented sudoku for dynamic dimensions (NxN) instead of the regular 9x9
- We implemented a different (and faster) viewpoint for Shikaku (as explained in the 4)

We did this because it came naturally during the development of our solution.
We found it a fun challenge to allow for more sizes of Sudoku, and the alternative viewpoint of Shikaku came from a desire to make a faster solution.

# CONCLUSION

We have experienced first-hand that debugging the CLP can be a challenge, but that the needed code remains surprisingly short without compromising for the readability.
We learned that different heuristics are strongly problem-dependent and can greatly influence the outcome speed.
We learned that a creating a different viewpoint, if not already better than the previous, can enlighten a new angle that improves the previous viewpoint.
And we learned that some counter-intuitive measures, like redundant rules can greatly improve the overall performance in CHR.

In retrospect of the teamwork, we think that we managed to lower the workload by balancing our capacities.
We attempted to distribute the different aspects of the assignment and used peer-programming of one of us got stuck at a problem.
We tried to keep an even distribution over the languages so both of us got a taste of working with both implementations.

Overall, it was interesting to work with CLP languages and see the empirical results rather than the solely the theoretical.

# APPENDIX

We each spent about 50 hours on this assignment.
The implementation of Sudoku took longest, which is probably due to the lack of experience as we finished this assignment in chronological order.

The extra Sudoku puzzles we tested came from: http://www.planetsudoku.net
We used JavaScript code that can be executed in the console to easily get the Sudoku in the right format.
The JavaScript code can be found in the sudex_toledo.pl file.


TODO TODO TODO

Dingen die in de opgave stonden maar nog missen (bovenop andere TODO's in de tekst):

2.1:

* What are the criteria you would use to judge whether a viewpoint is a
good one or not?

2.2:

* Do you get different results when using different search heuristcs? for your 2 implementations? when using different viewpoints? using channeling? Why? -> Meer tests?
 ->Mssn nog iets over: https://docs.google.com/spreadsheets/d/104v0hoB6St5OcGLtnciMYNh6vDZ2OritDcx8gM066NI/edit#gid=1346877380

* What is/are the most difficult puzzle(s) for your programs? Can you explain why this is the case?
* alle XXX onder tables vervangen met actual table references D:
