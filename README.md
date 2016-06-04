# APLAI
Implementation for an assignment for Advanced Programming Languages for Artificial Intelligence course at KU Leuven.

## Useful sources
* Due date: Monday 06/06/2016 13h00.
* You email your report (a pdf file), programs and README file to gerda.janssens@cs.kuleuven.be.
* Drive Excel results: https://drive.google.com/folderview?id=0B2fq4uwDTe99SjhGVTF6SGwyLVU&usp=sharing
* E-learning: http://4c.ucc.ie/~hsimonis/ELearning/index.htm
* CHR info: https://sicstus.sics.se/sicstus/docs/3.7.1/html/sicstus_34.html
* ECLiPSe search parameters explained: http://eclipseclp.org/doc/bips/lib/ic/search-6.html
* Stackoverflow question: http://stackoverflow.com/questions/36356702/profiling-eclipse-clp

## Structure
In the directory "test results", all of our test results can be found in regular text documents (.txt).
For the code files, the names consist of [puzzle, Sudoku or Shikaku]_[implementation, CHR or eclipse]_[alt or nothing if regular].pl

There is also a file `print_shikaku.pl` that was supplied on toledo to get a nice unicode print of the Shikaku results. and there are 2 puzzle files, where `sudex_toledo.pl` contains all of the Sudoku puzzles and `puzzles.pl` contains all of the Shikaku puzzles.

## ECLiPSe
Open the tkeclipse interface and load the file you wish to run.
It is important that you run the file in the same folder as the other files, as it will automatically include the file with the puzzles.
Once opened, you can solve all puzzles `solve_all.` or solve a specific puzzle `solve(name_of_puzzle).` and the program will return the results in the console.

You can also run all Sudoku experiments from the channeling files0
The file defines a method solve_all/0 which will loop over all the different Sudokus defined in the sudex_toledo, using default channeling method.
You can also solve a specific puzzle by using solve/2 using the puzzle name and model ('simple', 'alt' or 'both') as arguments.

## CHR
Open SWI-Prolog and load the file you wish to run.
It is important that you run the file in the same folder as the other files, as it will automatically include the file with the puzzles.
There are similar commands available as in ECLiPSe to run the puzzle you wish.




