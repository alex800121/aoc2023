{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}

module Day20 where

import qualified Data.HashMap.Strict as H
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import MyLib
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
  deriving (Show, Ord, Eq, Generic, Hashable)

type GameState = H.HashMap String Module

type Signal = H.HashMap String Pulse

sendSignal :: (String, Pulse) -> Module -> (Module, [(String, Pulse)])
sendSignal (_, On) b@(Broadcaster n) = (b, map (,On) n)
sendSignal (_, p) f@(Flip s n) = if p then (Flip (not s) n, map (,s) n) else (f, [])
sendSignal (from, p) c@(Conj s n) = (Conj (H.insert from p s) n, undefined)

day20 :: IO ()
day20 = do
  -- input <- readFile "input/input20.txt"
  return ()
