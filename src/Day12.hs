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

stringToMask :: String -> (Word, Word)
stringToMask x = (andMask, orMask)
  where
    f acc _ [] = acc
    f acc i (z : zs) = f acc' (i + 1) zs
      where
        acc' = case z of
          '#' -> setBit acc i
          '.' -> clearBit acc i
          _ -> acc
    andMask = f maxBound 0 x
    orMask = f minBound 0 x

buildWord :: [Int] -> Word
buildWord = f 0 0
  where
    f _ acc [] = acc
    f i acc (x : xs) = g (i + x) (foldr (flip clearBit) acc [i .. (i + x - 1)]) xs
    g _ acc [] = acc
    g i acc (x : xs) = f (i + x) (foldr (flip setBit) acc [i .. (i + x - 1)]) xs

testWord :: (Word, Word) -> Word -> Bool
testWord (andMask, orMask) x = x == (x .&. andMask) .|. orMask

intervalVariant :: Int -> [Int] -> [[Int]]
intervalVariant len l = do
  let x = len - sum l + 2
      y = length l + 1
  zs <- intVariant x y
  let zh = head zs - 1
      zl = last zs - 1
      zs' = zh : tail (init zs) ++ [zl]
  return $ concat $ transpose [zs', l]

intVariant :: Int -> Int -> [[Int]]
intVariant x y
  | y > x || x <= 0 = []
  | y == 1 = [[x]]
  | otherwise =
      [ z : zs
        | z <- [1 .. (x - y + 1)],
          zs <- intVariant (x - z) (y - 1)
      ]

calc :: String -> [Int] -> State (Map (String, [Int]) Int) Int
calc [] (x: _) = return 0
calc s [] = if '#' `notElem` s then return 1 else return 0
calc s l@(x : xs) =
  get >>= \m -> case m Map.!? (s, l) of
    Just i -> return i
    Nothing -> do
      let ss =
            map (drop 1 . snd)
              . filter (\(h, t) -> length h == x && notElem '.' h && maybe True ((/= '#') . fst) (uncons t))
              . map (splitAt x . snd)
              . takeWhile (notElem '#' . fst)
              . (zip <$> inits <*> tails)
              $ dropWhile (== '.') s
      i <- sum <$> forM ss (`calc` xs)
      modify' (Map.insert (dropWhile (== '.') s, l) i . Map.insert (s, l) i)
      return i

day12 :: IO ()
day12 = do
  input <- map inputParser . lines <$> readFile "input/input12.txt"
  let inputA = map (first stringToMask) input
      day12a = runState (mapM (uncurry calc) input) Map.empty
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
