# haskell
FUNCTIONAL PROGRAMMING: HASKELL
# tar 1
Tar 1

a) type everything x::zzzz

b) try to be functional, (read some code)

c) try to write your own code without too much help from internet, libraries ….

 

1)  Find n’th generation in the sequence

1 / 1 1 / 2 1 / 1 2 1 1/ …    (how many distinct elements are there?)

2) Define your own list with conversions back and forth to Haskell's List

 

3) Implement your own elem for list in 3 different ways

 

4) sublist from i'th to j'th element

 

5) Define general rooted tree,

implement elem and sum for it


# tar 2
Read  https://www.cs.tufts.edu/~nr/cs257/archive/richard-bird/sudoku.pdf

make a running version of a Sudoku solver to find one solution + add your own heuristics.

 

2) Make a tree that describes mathematical expressions with 1 variable x

constants, x, +,-,^,*,/,sin,cos …

Use Maybe so division by 0 is not a problem

           eval  ::: to evaluate at a given x

      differentiate ::: tree of derivative
      
# tar 3

Hanoi towers, tracking the steps with the writer monad 

 

A function that adds a random number every "even" time it used and subtracts a random number every "odd" time it is used (state monad) 


 Parse a floating point number
 
# tar 4





1)      Use “fix” to compute addition using only “succ”, "pred" and  "test for zero" as primitives

2)      Do same using lambda calculus https://github.com/sdiehl/write-you-a-haskell/tree/master/chapter4/untyped

3)      https://www.schoolofhaskell.com/user/DanBurton/20-intermediate-exercises 3,6,9,20