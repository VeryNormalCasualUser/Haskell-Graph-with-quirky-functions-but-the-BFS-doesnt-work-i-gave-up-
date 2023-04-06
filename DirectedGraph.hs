{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use foldl" #-}
{-# HLINT ignore "Fuse foldr/map" #-}
{-# HLINT ignore "Use infix" #-}
module DirectedGraph where 

import Data.List

type Vertex = String 
type Edge = (Vertex, Vertex)
type Node = (Vertex, [Vertex])
type DirectedGraph = [Node]

containsVertex :: [(Vertex, [Vertex])] -> Vertex -> Bool
containsVertex graph vertex = elem vertex $ map fst graph

refreshNode :: Vertex -> Node -> Node
refreshNode vertex node
    |not $ elem vertex (snd node) = (fst node, snd node ++ [vertex])
    |otherwise = node


delete' :: Eq a => a -> [a] -> [a]
delete' deleted xs = [ x | x <- xs, x /= deleted ]


find' :: Vertex -> [Node] -> Node
find' vertexNode [] = (vertexNode, [])
find' vertex (x:xs) 
    | vertex == fst x = x
    | otherwise = find' vertex xs

refreshGraph :: Node -> Node -> [Node] -> [Node]
refreshGraph newNode _ [] = [newNode]
refreshGraph newNode oldNode (x:xs)
    |oldNode == x = xs++[newNode]
    |otherwise = refreshGraph newNode oldNode xs ++ [x]

addEdge :: [Node] -> (Vertex, Vertex) -> [Node]
addEdge graph edge = 
    refreshGraph (refreshNode (snd edge) (find' (fst edge) graph)) (find' (fst edge) graph) graph

addEdges :: [Node] -> [Edge] -> [Node]
addEdges graph [] = graph 
addEdges graph (x:xs) = addEdges (addEdge graph x) xs 

createGraph :: [Edge] -> [Node]
createGraph = addEdges []

addEdges1 :: Foldable t => [Node] -> t (Vertex, Vertex) -> [Node]
addEdges1 = foldl addEdge 


--bfsHelper :: [Node] -> [Vertex] -> [Vertex]  -> Vertex -> Int -> Vertex -> Int
--bfsHelper graph visited newAdjacent vertexToFind count currentVertex 
--    |currentVertex == vertexToFind                                                              = count
--    |(snd $ find' currentVertex graph) == [currentVertex]  && vertexToFind /= currentVertex     = maxBound:: Int
--    |isPermutation visited  (map fst graph)                                                     = maxBound:: Int
--    |otherwise                              = foldr min (maxBound:: Int) (map (bfsHelper graph (addExcl newAdjacent visited) (snd (find' currentVertex graph)) vertexToFind (count+1)) (snd (find' currentVertex graph)))
--  |otherwise                                                                                  = 
--        foldr (min . bfsHelper graph (addExcl newAdjacent visited) (snd (find' currentVertex graph)) vertexToFind (count + 1)) (maxBound :: Int) (snd (find' currentVertex graph))

bfsHelper' :: [Node] -> [Vertex] -> [Vertex] -> Vertex -> Int -> Vertex  -> Int
bfsHelper' graph visited toCheck vertexToFind count currentVertex 
    |currentVertex == vertexToFind          = count 
    |elem vertexToFind adjacent             = count+1
    |null toCheck                           = maxBound:: Int
    |isPermutation visited  (map fst graph) = maxBound:: Int
    |otherwise                              = bfsHelper' graph (currentVertex : visited) (drop 1 toCheck `addExcl` deleteAll visited adjacent) vertexToFind (count+1) nextVertex

    where 
        currentNode = find' currentVertex graph
        adjacent = snd currentNode
        nextVertex = head toCheck

deleteAll [] list       = list 
deleteAll (d:ds) theList
    |elem d theList     = deleteAll ds (delete' d theList)
    |otherwise          = deleteAll ds theList
--bfs :: [Node] -> Vertex -> Vertex -> Int
--bfs (g:gs) start toFind = bfsHelper (g:gs) [] [] toFind 0 start

bfs :: [Node] -> Vertex -> Vertex -> Int
bfs (g:gs) start toFind = bfsHelper' (g:gs) [] (snd g) toFind 0 start

addExcl :: [Vertex] -> [Vertex] -> [Vertex]
addExcl [] oldList = oldList 
addExcl (x:xs) oldList
    |elem x oldList = addExcl xs oldList 
    |otherwise      = addExcl xs (oldList++[x])

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation [] _  = False
isPermutation _  [] = False
isPermutation (x:xs) ys
    | elem x ys = isPermutation xs (remove1 x ys) 
    | otherwise = False


remove1 :: Eq a => a -> [a] -> [a]
remove1 e [] = []
remove1 e (x:xs)
    | e == x    =  xs
    | otherwise = x : (remove1 e xs)

 
keepUnique ::  [Vertex] -> [Vertex] -> [Vertex] -> [Vertex]     
keepUnique [] _ newList = newList
keepUnique (x:xs) oldList newList
    |not (elem x oldList) = keepUnique xs oldList (x : newList)
    |otherwise          = keepUnique xs oldList newList