module Day7 where

import Data.Bifunctor (Bifunctor (..))
import Data.Char (digitToInt, intToDigit)
import Data.Function (on)
import Data.List (group, partition, sort, sortBy)
import GHC.Read
import Paths_AOC2023
import Text.ParserCombinators.ReadP (ReadP, get, many)
import Text.ParserCombinators.ReadPrec (lift)

newtype Hand = H {_hand :: [Int]} deriving (Eq)

compareType :: [Int] -> [Int] -> Ordering
compareType h1 h2 = (compare `on` f) h2 h1
  where
    f h = sortBy (flip compare) $ map length $ group $ sort h

-- instance Ord Hand where
compareA (H h1) (H h2) = compareType h1 h2 <> compare h2 h1

convertHand :: Hand -> Hand
convertHand = H . map f . _hand
  where
    f 11 = 1
    f x = x

compareTypeB :: [Int] -> [Int] -> Ordering
compareTypeB = flip compare `on` f
  where
    f h = x : xs
      where
        (joker, others) = partition (== 1) h
        (x : xs) = case sortBy (flip compare) $ map length $ group $ sort others of
          (y : ys) -> y + length joker : ys
          [] -> [5]

compareB (H h1) (H h2) = compareTypeB h1 h2 <> compare h2 h1

fromChar :: Char -> Int
fromChar c = case c of
  'A' -> 14
  'K' -> 13
  'Q' -> 12
  'J' -> 11
  'T' -> 10
  n -> digitToInt n

toChar :: Int -> Char
toChar n = case n of
  14 -> 'A'
  13 -> 'K'
  12 -> 'Q'
  11 -> 'J'
  10 -> 'T'
  1 -> 'J'
  n -> intToDigit n

instance Show Hand where
  show = map toChar . _hand

instance Read Hand where
  readPrec = lift $ H <$> many (fromChar <$> get)

day7 :: IO ()
day7 = do
  input <- map ((\(x : y : _) -> (read @Hand x, read @Int y)) . words) . lines <$> (getDataDir >>= readFile . (++ "/input/input7.txt"))
  -- input <- map ((\(x : y : _) -> (read @Hand x, read @Int y)) . words) . lines <$> readFile "input/test7.txt"
  putStrLn
    . ("day7a: " ++)
    . show
    . sum
    . zipWith (*) [1 ..]
    . map snd
    . sortBy (flip compareA `on` fst)
    $ input
  putStrLn
    . ("day7b: " ++)
    . show
    . sum
    . zipWith (*) [1 ..]
    . map snd
    . sortBy (flip compareB `on` fst)
    $ map (first convertHand) input
