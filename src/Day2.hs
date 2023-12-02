module Day2 where

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M
import MyLib (Parser, signedInteger)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char (space)

day2aBag :: Bag
day2aBag = V.fromList [12, 13, 14]

type Bag = Vector Int

data Game = G
  { _id :: Int,
    _bag :: [Bag]
  }
  deriving (Show, Eq, Ord)

parseBag :: Parser Bag
parseBag = do
  n <- signedInteger <* space
  color <- ((string "red" >> return 0) <|> (string "green" >> return 1) <|> (string "blue" >> return 2)) <* space
  (char ',' >> space >> V.modify (\v -> M.modify v (+ n) color) <$> parseBag)
    <|> (((char ';' >> space) <|> eof) >> return (V.modify (\v -> M.modify v (+ n) color) $ V.fromList [0, 0, 0]))

parseGame :: Parser Game
parseGame = do
  string "Game "
  n <- signedInteger
  string ": "
  bags <- many parseBag
  return $ G n bags

validGame :: Bag -> Game -> Bool
validGame b g = all (V.and . V.zipWith (>=) b) (_bag g)

data Color
  = Red
  | Green
  | Blue
  deriving (Show, Eq, Ord, Enum)

day2b :: Game -> Int
day2b = V.foldr (*) 1 . foldr (V.zipWith max) (V.fromList [0, 0, 0]) . _bag

day2 :: IO ()
day2 = do
  Just input <- traverse (parseMaybe parseGame) . lines <$> readFile "input/input2.txt"
  putStrLn 
    . ("day2a: " ++)
    . show
    . sum
    . map _id
    . filter (validGame day2aBag)
    $ input
  putStrLn 
    . ("day2b: " ++)
    . show
    . sum
    . map day2b
    $ input
