{-# LANGUAGE PackageImports #-}
module Main where

import Criterion.Main
import Criterion.Types (Config (..), Verbosity (Quiet))
import Day1
import Day10
import Day11
import Day12
import Day13
import Day14
import Day15
import Day16
import Day17
import Day18
import Day19
import Day2
import Day20
import Day21
import Day22
import Day23
import Day24
import Day25
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8
import Day9
import MyLib

mainBench = do
  day1
  day2
  day3
  day4
  day5
  day6
  day7
  day8
  day9
  day10
  day11
  day12
  day13
  day14
  day15
  day16
  day17
  day18
  day19
  day20
  day21
  day22
  day23
  day24
  day25

main =
  defaultMainWith
    (defaultConfig {verbosity = Quiet, reportFile = Just "./bench.html"})
    [ bench "main" (nfIO mainBench),
      bgroup
        "Day"
        [ bench "day1" (nfIO day1),
          bench "day2" (nfIO day2),
          bench "day3" (nfIO day3),
          bench "day4" (nfIO day4),
          bench "day5" (nfIO day5),
          bench "day6" (nfIO day6),
          bench "day7" (nfIO day7),
          bench "day8" (nfIO day8),
          bench "day9" (nfIO day9),
          bench "day10" (nfIO day10),
          bench "day11" (nfIO day11),
          bench "day12" (nfIO day12),
          bench "day13" (nfIO day13),
          bench "day14" (nfIO day14),
          bench "day15" (nfIO day15),
          bench "day16" (nfIO day16),
          bench "day17" (nfIO day17),
          bench "day18" (nfIO day18),
          bench "day19" (nfIO day19),
          bench "day20" (nfIO day20),
          bench "day21" (nfIO day21),
          bench "day22" (nfIO day22),
          bench "day23" (nfIO day23),
          bench "day24" (nfIO day24),
          bench "day25" (nfIO day25)
        ]
    ]
