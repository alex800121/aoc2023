module Day22 where

import Control.Monad (foldM)
import Control.Monad.Trans.State (State, evalState, get)
import Data.Bifunctor (Bifunctor (..))
import Data.List (partition)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace (traceShow)
import MyLib
import Text.Megaparsec (parseMaybe, parseTest, sepBy)
import Text.Megaparsec.Char

type Brick = Vec (S (S (S Z))) (Int, Int)

data GameState = G
  { _support :: Map Brick (Set Brick),
    _depend :: Map Brick (Set Brick),
    _free :: Set Brick
  }
  deriving (Show, Eq, Ord)

parseBlock :: Parser Brick
parseBlock = do
  a <- sepBy signedInteger (char ',')
  char '~'
  b <- sepBy signedInteger (char ',')
  return $ toVec (SS (SS (SS SZ))) $ reverse $ map (second (+ 1)) $ zipWith (\x y -> (min x y, max x y)) a b

runG :: Int -> GameState -> GameState
runG floor g = case Set.minView (_free g) of
  Nothing -> g
  -- Just (x, xs) | traceShow (length (_free g), length (_support g)) True -> runG floor (dropFree floor x (g {_free = xs}))
  Just (x, xs) -> runG floor (dropFree floor x (g {_free = xs}))

dropFree :: Int -> Brick -> GameState -> GameState
dropFree floor b g
  | fst (vRead b' 0) <= floor =
      g
        { _support = support,
          _depend = Map.insert b (Set.singleton $ pure (0, 0)) $ _depend g
        }
  | not $ Set.null depend =
      g'
        { _support =
            Set.foldl'
              ( \acc x ->
                  Map.insertWith
                    Set.union
                    x
                    (Set.singleton b)
                    acc
              )
              support
              depend
        }
  | otherwise = dropFree floor b' g
  where
    b' = vModify b 0 (bimap (subtract 1) (subtract 1))
    depend = Set.filter (isJust . overlapEucVec b') (Map.keysSet $ _support g)
    support = Map.insert b Set.empty (_support g)
    g' = g {_depend = Map.insert b depend (_depend g)}

removable :: GameState -> Brick -> Bool
removable g b = all ((> 1) . length . (_depend g Map.!)) (_support g Map.! b)

disintegrate :: GameState -> Brick -> Set Brick
disintegrate g = f Set.empty g . Set.singleton
  where
    f acc gs bs
      | Set.null bs = acc
      | otherwise = f acc' gs' bs'
      where
        (bs', depend') =
          first Map.keysSet
            . Map.mapEither
              ( \x ->
                  let x' = x Set.\\ bs
                   in if Set.null x' then Left x' else Right x'
              )
            $ _depend gs
        gs' = gs {_depend = depend'}
        acc' = Set.union acc bs'

day22 :: IO ()
day22 = do
  -- input <- lines <$> readFile "input/test22.txt"
  input <- lines <$> readFile "input/input22.txt"
  let initG = G Map.empty Map.empty (Set.fromList (mapMaybe (parseMaybe parseBlock) input))
      g = runG 0 initG
      (day22a, day22b) = partition (removable g) (Map.keys (_support g))
  putStrLn
    . ("day22a: " ++)
    . show
    $ length day22a
  putStrLn
    . ("day22b: " ++)
    . show
    . sum
    $ map (length . disintegrate g) day22b

-- print $ sum n
