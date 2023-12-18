module Day18 where

import Data.Colour.Word8
import MyLib (Direction (..), Parser, signedInteger)
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.List (foldl')
import Data.Char (digitToInt)

type DigPlan = (Direction, Int, Colour8)

readColour8 :: Parser Colour8
readColour8 = do
  optional $ char '#'
  let f = fromIntegral . foldl' (\acc x -> digitToInt x + 16 * acc) 0
  r <- f <$> count 2 hexDigitChar
  g <- f <$> count 2 hexDigitChar
  b <- f <$> count 2 hexDigitChar
  return $ Colour8 r g b

readInput :: Parser DigPlan
readInput = do
  d <- (char 'R' >> pure East) <|> (char 'D' >> pure South) <|> (char 'L' >> pure West) <|> (char 'U' >> pure North)
  space
  i <- signedInteger
  space
  c <- between (char '(') (char ')') readColour8
  pure (d, i, c)

day18 :: IO ()
day18 = do
  input <- map (parseMaybe readInput) . lines <$> readFile "input/input18.txt"
  print input
  parseTest readInput "R 6 (#70c710)"
