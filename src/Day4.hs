{-# LANGUAGE TupleSections #-}

module Day4 where

import Data.List (intersect, (\\))
import Data.Maybe (mapMaybe)
import MyLib
import Paths_AOC2023
import Text.Megaparsec
import Text.Megaparsec.Char

data Card = C
  { _cardId :: Int,
    _win :: [Int],
    _num :: [Int]
  }
  deriving (Show, Eq, Ord)

cardParser :: Parser Card
cardParser = do
  cardId <- string "Card" >> space >> signedInteger <* char ':' <* space
  win <- someTill (signedInteger <* space) (space >> char '|' <* space)
  num <- sepBy signedInteger space
  return $ C cardId win num

day4a :: Card -> Int
day4a c
  | n <= 0 = 0
  | otherwise = 2 ^ (n - 1)
  where
    n = length $ _num c `intersect` _win c

day4b :: [(Card, Int)] -> [(Card, Int)]
day4b [] = []
day4b ((c, i) : xs) = (c, i) : day4b xs'
  where
    xs' = zipWith f ys xs
    matched = length $ _num c `intersect` _win c
    ys = replicate matched i ++ repeat 0
    f y = fmap (+ y)

day4 :: IO ()
day4 = do
  -- input <- mapMaybe (parseMaybe cardParser) . lines <$> readFile "input/test4.txt"
  input <- mapMaybe (parseMaybe cardParser) . lines <$> (getDataDir >>= readFile . (++ "/input/input4.txt"))
  putStrLn
    . ("day4a: " ++)
    . show
    . sum
    $ map day4a input
  putStrLn
    . ("day4b: " ++)
    . show
    . sum
    . map snd
    . day4b
    $ map (,1) input
