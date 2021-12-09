module Aoc2021.Day07
  ( part1,
    part2,
  )
where

import Aoc2021.AocUtils
import Control.Applicative
import qualified Data.Attoparsec.Text as P

input = getInput "input/day07.txt" parser

parser = P.decimal `P.sepBy` (P.char ',')

part1 = minimum $ map (f input) [minimum input .. maximum input]
  where
    f i x = sum $ map (abs . subtract x) i

part2 = minimum $ map (f input) [minimum input .. maximum input]
  where
    f i x = sum $ map (s . abs . subtract x) i
    s x = (x * (x + 1)) `div` 2
