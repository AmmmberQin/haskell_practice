-- @Author: Li Qin
-- @Date:   2019-05-23 11:37:48
-- @Last Modified by:   Li Qin
-- @Last Modified time: 2019-05-23 11:54:32

-- Problem 80
-- (***) Conversions

-- Write predicates to convert between the different graph representations. With these predicates, all representations are equivalent; i.e. for the following problems you can always pick freely the most convenient form. The reason this problem is rated (***) is not because it's particularly difficult, but because it's a lot of work to deal with all the special cases.

-- Example in Haskell:

-- λ> graphToAdj Graph ['b','c','d','f','g','h','k'] [('b','c'),('b','f'),('c','f'),('f','k'),('g','h')]
-- Adj [('b', "cf"), ('c', "bf"), ('d', ""), ('f', "bck"), ('g', "h"), ('h', "g"), ('k', "f")]



-- Problem 81
-- (**) Path from one node to another one

-- Write a function that, given two nodes a and b in a graph, returns all the acyclic paths from a to b.

-- Example in Haskell:

-- λ> paths 1 4 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]
-- [[1,2,3,4],[1,3,4]]
-- λ> paths 2 6 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]
-- []



-- Problem 82
-- (*) Cycle from a given node

-- Write a predicate cycle(G,A,P) to find a closed path (cycle) P starting at a given node A in the graph G. The predicate should return all cycles via backtracking.

-- Example in Haskell:

-- λ> cycle 2 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]
-- [[2,3,4,2]]
-- λ> cycle 1 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]
-- []



-- Problem 83
-- (**) Construct all spanning trees

-- Write a predicate s_tree(Graph,Tree) to construct (by backtracking) all spanning trees of a given graph. With this predicate, find out how many spanning trees there are for the graph depicted to the left. The data of this example graph can be found in the file p83.dat. When you have a correct solution for the s_tree/2 predicate, use it to define two other useful predicates: is_tree(Graph) and is_connected(Graph). Both are five-minutes tasks!

-- Example in Haskell:

-- λ> length $ spanningTree k4
-- 16



-- Problem 84
-- (**) Construct the minimal spanning tree

-- Write a predicate ms_tree(Graph,Tree,Sum) to construct the minimal spanning tree of a given labelled graph. Hint: Use the algorithm of Prim. A small modification of the solution of P83 does the trick. The data of the example graph to the right can be found in the file p84.dat.

-- Example in Haskell:

-- λ> prim [1,2,3,4,5] [(1,2,12),(1,3,34),(1,5,78),(2,4,55),(2,5,32),(3,4,61),(3,5,44),(4,5,93)]
-- [(1,2,12),(1,3,34),(2,4,55),(2,5,32)]



-- Problem 85
-- (**) Graph isomorphism

-- Two graphs G1(N1,E1) and G2(N2,E2) are isomorphic if there is a bijection f: N1 -> N2 such that for any nodes X,Y of N1, X and Y are adjacent if and only if f(X) and f(Y) are adjacent.

-- Write a predicate that determines whether two graphs are isomorphic. Hint: Use an open-ended list to represent the function f.

-- Example in Haskell:

-- λ> graphG1 = [1,2,3,4,5,6,7,8] [(1,5),(1,6),(1,7),(2,5),(2,6),(2,8),(3,5),(3,7),(3,8),(4,6),(4,7),(4,8)]
-- λ> graphH1 = [1,2,3,4,5,6,7,8] [(1,2),(1,4),(1,5),(6,2),(6,5),(6,7),(8,4),(8,5),(8,7),(3,2),(3,4),(3,7)]
-- λ> iso graphG1 graphH1
-- True



-- Problem 86
-- (**) Node degree and graph coloration

-- a) Write a predicate degree(Graph,Node,Deg) that determines the degree of a given node.

-- b) Write a predicate that generates a list of all nodes of a graph sorted according to decreasing degree.

-- c) Use Welch-Powell's algorithm to paint the nodes of a graph in such a way that adjacent nodes have different colors.

-- Example in Haskell:

-- λ> kColor ['a','b','c','d','e','f','g','h','i','j'] [('a','b'),('a','e'),('a','f'),('b','c'),('b','g'),('c','d'),('c','h'),('d','e'),('d','i'),('e','j'),('f','h'),('f','i'),('g','i'),('g','j'),('h','j')]
-- [('a',1),('b',2),('c',1),('d',2),('e',3),('f',2),('g',1),('h',3),('i',3),('j',2)]



-- Problem 87
-- (**) Depth-first order graph traversal (alternative solution)

-- Write a predicate that generates a depth-first order graph traversal sequence. The starting point should be specified, and the output should be a list of nodes that are reachable from this starting point (in depth-first order).

-- Example in Haskell:

-- λ> depthFirst ([1,2,3,4,5,6,7], [(1,2),(2,3),(1,4),(3,4),(5,2),(5,4),(6,7)]) 1
-- [1,2,3,4,5]



-- Problem 88
-- (**) Connected components (alternative solution)

-- Write a predicate that splits a graph into its connected components.

-- Example in Haskell:

-- λ> connectedComponents ([1,2,3,4,5,6,7], [(1,2),(2,3),(1,4),(3,4),(5,2),(5,4),(6,7)])
-- [[1,2,3,4,5][6,7]]



-- Problem 89
-- (**) Bipartite graphs

-- Write a predicate that finds out whether a given graph is bipartite.

-- Example in Haskell:

-- λ> bipartite ([1,2,3,4,5],[(1,2),(2,3),(1,4),(3,4),(5,2),(5,4)])
-- True
-- λ> bipartite ([1,2,3,4,5],[(1,2),(2,3),(1,4),(3,4),(5,2),(5,4),(1,3)])
-- False
