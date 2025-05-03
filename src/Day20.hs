{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}

module Day20 where

import Data.Bifunctor (Bifunctor (..))
import Data.Function (on)
import Data.HashMap.Strict qualified as H
import Data.Hashable (Hashable)
import Data.List (delete, find, findIndex, findIndices, foldl1', (\\))
import Data.Maybe (fromMaybe, mapMaybe)
import Debug.Trace
import GHC.Generics (Generic)
import MyLib
import Paths_AOC2023
import Text.Megaparsec
import Text.Megaparsec.Char

-- data Pulse = Low | High deriving (Show, Ord, Eq)

-- data Switch = On | Off deriving (Show, Ord, Eq)

type Pulse = Bool

type Switch = Bool

pattern On = True

pattern Off = False

pattern Low = True

pattern High = False

-- instance Enum Switch where
--   fromEnum On = 0
--   fromEnum Off = 1
--   toEnum x = case x `mod` 2 of
--     0 -> On
--     1 -> Off
--
-- instance Enum Pulse where
--   fromEnum Low = 0
--   fromEnum High = 1
--   toEnum x = case x `mod` 2 of
--     0 -> Low
--     1 -> High

data Module
  = Broadcaster {_getNext :: [String]}
  | Flip {_getSwitch :: Switch, _getNext :: [String]}
  | Conj {_getPulse :: Signal, _getNext :: [String]}
  | Output {_getReceived :: [(String, Pulse)]}
  deriving (Show, Ord, Eq, Generic, Hashable)

type GameState = H.HashMap String Module

type Signal = H.HashMap String Pulse

sendSignal :: (String, Pulse) -> Module -> (Module, [(String, Pulse)])
sendSignal (_, On) b@(Broadcaster n) = (b, map (,On) n)
sendSignal (_, p) f@(Flip s n) = if p then (Flip (not s) n, map (,s) n) else (f, [])
sendSignal (from, p) (Conj s n) =
  let s' = H.insert from p s
      c' = Conj s' n
      p' = not (or s')
   in (c', map (,p') n)
sendSignal x (Output xs) = (Output (xs <> [x]), [])

parseBroadcaster :: Parser GameState
parseBroadcaster = do
  string "broadcaster -> "
  H.singleton "broadcaster" . Broadcaster <$> sepBy (many letterChar) (char ',' >> space)

parseFlip :: Parser GameState
parseFlip = do
  char '%'
  s <- many letterChar
  string " -> "
  H.singleton s . Flip Off <$> sepBy (many letterChar) (char ',' >> space)

parseConj :: Parser GameState
parseConj = do
  char '&'
  s <- many letterChar
  string " -> "
  H.singleton s . Conj H.empty <$> sepBy (many letterChar) (char ',' >> space)

parseModules :: Parser GameState
parseModules = choice [parseBroadcaster, parseFlip, parseConj]

pushButton :: (GameState, ((Int, ([String], Int)), (Int, Int))) -> (GameState, ((Int, ([String], Int)), (Int, Int)))
pushButton gs@(!g', ((!i, (!targets, !t)), (!low, !high))) = second (first (first (+ 1))) $ f [("broadcaster", ("button", Low))] gs
  where
    f [] g = g
    f ((!to, (!from, !sig)) : xs) (!g, !acc) = f xs' (g', acc')
      where
        acc' = bimap (if to `elem` targets && sig then second (bimap (delete to) (lcm (i + 1))) else id) (if sig then first (+ 1) else second (+ 1)) acc
        (module', nextSig) = sendSignal (from, sig) $ fromMaybe (Output []) (g H.!? to)
        xs' = xs <> map (\(a, b) -> (a, (to, b))) nextSig
        g' = H.insert to module' g

fixConjInput :: GameState -> GameState
fixConjInput g =
  H.mapWithKey
    ( \k a -> case a of
        Conj m n ->
          let ns = H.keys $ H.filter (\x -> k `elem` _getNext x) g
           in Conj (H.fromList (map (,Low) ns)) n
        x -> x
    )
    g

day20 :: IO ()
day20 = do
  -- input <- fixConjInput . H.unions . mapMaybe (parseMaybe parseModules) . lines <$> readFile "input/test20.txt"
  input <- fixConjInput . H.unions . mapMaybe (parseMaybe parseModules) . lines <$> (getDataDir >>= readFile . (++ "/input/input20.txt"))
  let targets = ["js", "zb", "rr", "bs"]
      l = iterate pushButton (input, ((0, (targets, 1)), (0, 0)))
  putStrLn
    . ("day20a: " ++)
    . show
    . uncurry (*)
    . snd
    . snd
    $ l !! 1000
  putStrLn
    . ("day20b: " ++)
    . show
    . fmap snd
    $ find (null . fst) (map (snd . fst . snd) l)
