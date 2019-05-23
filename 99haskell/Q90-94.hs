-- @Author: Li Qin
-- @Date:   2019-05-23 11:38:12
-- @Last Modified by:   Li Qin
-- @Last Modified time: 2019-05-23 11:55:10

-- Problem 90
-- (**) Eight queens problem

-- This is a classical problem in computer science. The objective is to place eight queens on a chessboard so that no two queens are attacking each other; i.e., no two queens are in the same row, the same column, or on the same diagonal.

-- Hint: Represent the positions of the queens as a list of numbers 1..N. Example: [4,2,7,3,6,8,5,1] means that the queen in the first column is in row 4, the queen in the second column is in row 2, etc. Use the generate-and-test paradigm.

-- Example in Haskell:

-- λ> length (queens 8)
-- 92
-- λ> head (queens 8)
-- [1,5,8,6,3,7,2,4]



-- Problem 91
-- (**) Knight's tour

-- Another famous problem is this one: How can a knight jump on an NxN chessboard in such a way that it visits every square exactly once? A set of solutions is given on the The_Knights_Tour page.

-- Hints: Represent the squares by pairs of their coordinates of the form X/Y, where both X and Y are integers between 1 and N. (Note that '/' is just a convenient functor, not division!) Define the relation jump(N,X/Y,U/V) to express the fact that a knight can jump from X/Y to U/V on a NxN chessboard. And finally, represent the solution of our problem as a list of N*N knight positions (the knight's tour).

-- There are two variants of this problem:

-- find a tour ending at a particular square
-- find a circular tour, ending a knight's jump from the start (clearly it doesn't matter where you start, so choose (1,1))
-- Example in Haskell:

-- λ> head $ knightsTo 8 (1,1)
-- [(2,7),(3,5),(5,6),(4,8),(3,6),(4,4),(6,5),(4,6),
-- (5,4),(7,5),(6,3),(5,5),(4,3),(2,4),(1,6),(2,8),
-- (4,7),(6,8),(8,7),(6,6),(4,5),(6,4),(5,2),(7,1),
-- (8,3),(6,2),(8,1),(7,3),(8,5),(7,7),(5,8),(3,7),
-- (1,8),(2,6),(3,4),(1,5),(2,3),(3,1),(1,2),(3,3),
-- (1,4),(2,2),(4,1),(5,3),(7,4),(8,2),(6,1),(4,2),
-- (2,1),(1,3),(2,5),(1,7),(3,8),(5,7),(7,8),(8,6),
-- (6,7),(8,8),(7,6),(8,4),(7,2),(5,1),(3,2),(1,1)]
-- λ> head $ closedKnights 8  
-- [(1,1),(3,2),(1,3),(2,1),(3,3),(5,4),(6,6),(4,5),
-- (2,6),(1,8),(3,7),(5,8),(4,6),(2,5),(4,4),(5,6),
-- (6,4),(8,5),(7,7),(6,5),(5,3),(6,1),(4,2),(6,3),
-- (8,2),(7,4),(5,5),(3,4),(1,5),(2,7),(4,8),(3,6),
-- (1,7),(3,8),(5,7),(7,8),(8,6),(6,7),(8,8),(7,6),
-- (8,4),(7,2),(5,1),(4,3),(3,5),(1,4),(2,2),(4,1),
-- (6,2),(8,1),(7,3),(5,2),(7,1),(8,3),(7,5),(8,7),
-- (6,8),(4,7),(2,8),(1,6),(2,4),(1,2),(3,1),(2,3)]



-- Problem 92
-- (***) Von Koch's conjecture

-- Several years ago I met a mathematician who was intrigued by a problem for which he didn't know a solution. His name was Von Koch, and I don't know whether the problem has been solved since.

-- Anyway the puzzle goes like this: Given a tree with N nodes (and hence N-1 edges). Find a way to enumerate the nodes from 1 to N and, accordingly, the edges from 1 to N-1 in such a way, that for each edge K the difference of its node numbers equals to K. The conjecture is that this is always possible.

-- For small trees the problem is easy to solve by hand. However, for larger trees, and 14 is already very large, it is extremely difficult to find a solution. And remember, we don't know for sure whether there is always a solution!

-- Write a predicate that calculates a numbering scheme for a given tree. What is the solution for the larger tree pictured below?


-- Example in Haskell:

-- λ>  head $ vonKoch [(1,6),(2,6),(3,6),(4,6),(5,6),(5,7),(5,8),(8,9),(5,10),(10,11),(11,12),(11,13),(13,14)]
-- [6,7,8,9,3,4,10,11,5,12,2,13,14,1]



-- Problem 93
-- (***) An arithmetic puzzle

-- Given a list of integer numbers, find a correct way of inserting arithmetic signs (operators) such that the result is a correct equation. Example: With the list of numbers [2,3,5,7,11] we can form the equations 2-3+5+7 = 11 or 2 = (3*5+7)/11 (and ten others!).

-- Division should be interpreted as operating on rationals, and division by zero should be avoided.

-- Example in Haskell:

-- λ> mapM_ putStrLn $ puzzle [2,3,5,7,11]
-- 2 = 3-(5+7-11)
-- 2 = 3-5-(7-11)
-- 2 = 3-(5+7)+11
-- 2 = 3-5-7+11
-- 2 = (3*5+7)/11
-- 2*(3-5) = 7-11
-- 2-(3-(5+7)) = 11
-- 2-(3-5-7) = 11
-- 2-(3-5)+7 = 11
-- 2-3+5+7 = 11
-- The other two solutions alluded to in the problem description are dropped by the Haskell solution as trivial variants:

-- 2 = 3-(5+(7-11))
-- 2-3+(5+7) = 11



-- Problem 94
-- (***) Generate K-regular simple graphs with N nodes

-- In a K-regular graph all nodes have a degree of K; i.e. the number of edges incident in each node is K. How many (non-isomorphic!) 3-regular graphs with 6 nodes are there?

-- Sample results

-- Example in Haskell:

-- λ> length $ regular 6 3
-- 2
