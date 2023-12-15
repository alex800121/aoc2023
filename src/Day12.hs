module Day12 where

import Control.Monad (forM)
import Control.Monad.Trans.State
import Data.Bifunctor (Bifunctor (..))
import Data.Bits (Bits (..))
import Data.List (inits, intercalate, tails, transpose, uncons)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace (traceM, traceShow)
import MyLib (toBits)

inputParser :: String -> (String, [Int])
inputParser s = (x, i)
  where
    x : y : _ = words s
    i = map read $ splitOn "," y

calc :: String -> [Int] -> State (Map (String, [Int]) Int) Int
calc [] (x : _) = return 0
calc s [] = if '#' `notElem` s then return 1 else return 0
calc s l@(x : xs) =
  let s' = dropWhile (== '.') s
   in get >>= \m -> case m Map.!? (s', l) of
        Just i -> return i
        Nothing -> do
          let ss =
                map (drop 1 . snd)
                  . filter (\(h, t) -> length h == x && notElem '.' h && maybe True ((/= '#') . fst) (uncons t))
                  . map (splitAt x . snd)
                  . takeWhile (notElem '#' . fst)
                  . (zip <$> inits <*> tails)
                  $ s'
          i <- sum <$> forM ss (`calc` xs)
          modify' (Map.insert (s', l) i . Map.insert (s, l) i)
          return i

day12 :: IO ()
day12 = do
  input <- map inputParser . lines <$> readFile "input/input12.txt"
  let day12a = runState (mapM (uncurry calc) input) Map.empty
      day12b = runState (mapM (\(x, y) -> calc (intercalate "?" $ replicate 5 x) (concat $ replicate 5 y)) input) Map.empty
  putStrLn
    . ("day12a: " ++)
    . show
    . sum
    $ fst day12a
  putStrLn
    . ("day12b: " ++)
    . show
    . sum
    $ fst day12b
