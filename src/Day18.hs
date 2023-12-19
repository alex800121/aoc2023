module Day18 where

import Data.Colour.Word8
import MyLib (Direction, Parser)
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
  eof
  return $ Colour8 r g b

day18 :: IO ()
day18 = do
  -- input <- readFile "input/input18.txt"
  return ()
