{-# LANGUAGE TupleSections #-}

module Day25 where

import Data.Graph
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (sort, group)

readInput :: String -> (Graph, Vertex -> (Int, String, [String]), String -> Maybe Vertex)
readInput s = graphFromEdges s'
  where
    s' =
      zipWith (\x (y, z) -> (x, y, z)) [0 ..]
        . Map.toList
        . Map.unionsWith (<>)
        . map f
        $ lines s
    f x = Map.fromList $ map (,[a]) bs ++ [(a, bs)]
      where
        [a, b] = splitOn ": " x
        bs = words b

day25 :: IO ()
day25 = do
  -- (g, nodeFromVertex, vertexFromKey) <- readInput <$> readFile "input/input25.txt"
  (g, nodeFromVertex, vertexFromKey) <- readInput <$> readFile "input/test25.txt"
  print $ scc g
