module HanoiSolver where

type Peg = String
type Move = (Peg, Peg)

-- given the number of disks and names for the three pegs, prints out a list
-- of moves to move the stack of disks from the first disk to the second
-- e.g. hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 x y z   = []
hanoi 1 x y z   = [(x, y)]
hanoi n x y z   = hanoi (n - 1) x z y ++ [(x, y)] ++ hanoi (n - 1) z y x 
