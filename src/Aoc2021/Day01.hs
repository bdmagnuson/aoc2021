module Aoc2021.Day01
  ( part1,
    part2,
  )
where

import Aoc2021.AocUtils
import Control.Applicative
import qualified Data.Attoparsec.Text as P

parser = many (P.decimal <* P.endOfLine)

input = getInput "input/day01.txt" parser

do_part1 input = sum increased
  where
    increased = map f $ zip input (tail input)
      where
        f (a, b) = if a < b then 1 else 0

triple l@(a : b : c : xs) = (a + b + c) : (triple (tail l))
triple _ = []

part1 = do_part1 input

part2 = do_part1 (triple input)
