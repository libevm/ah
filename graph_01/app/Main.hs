module Main where

import Data.List (subsequences)

-- Each node has a value, and a bunch of edges
data Node n = Node {label :: n, edges :: [n]} deriving (Eq, Show)

-- Graph is a list of nodes, with their defined edges
newtype Graph n = Graph [Node n] deriving (Eq, Show)

{-
Graph we're building
   b   e
  /|\ /|
 a-+-d |
  \|/ \|
   c   f
-}

graph :: Graph Char
graph =
  Graph
    [ Node 'a' "bdc",
      Node 'b' "acd",
      Node 'c' "abd",
      Node 'd' "abcef",
      Node 'e' "df",
      Node 'f' "de"
    ]

-- Finds the largest array from a list of array
largest :: [[a]] -> [a]
largest = foldl (\acc x -> if length x > length acc then x else acc) []

-- Checks if a list of nodes is a clique
-- A list of nodes forms a clique if each element is pairwise connected
isClique :: (Eq n) => [Node n] -> Bool
isClique [] = True
isClique (n : ns) = all (\x -> label x `elem` edges n) ns && isClique ns

-- Finds the largest clique
clique :: (Eq n) => Graph n -> Maybe [Node n]
clique (Graph nodes) =
  -- If length of the largest clique is > 0 then there's a clique!
  -- Otherwise nothing
  let m = largest $ filter isClique (subsequences nodes)
   in if not $ null m
        then Just m
        else Nothing

-- Maybe clique

main :: IO ()
main = do
  print $ clique graph