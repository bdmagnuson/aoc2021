module Aoc2021.Day01
  ( part1,
    part2,
    bench,
  )
where

import Aoc2021.AocUtils
import Control.Applicative
import Data.Attoparsec.Text qualified as P

parser = many (P.decimal <* P.endOfLine)

input = getInput "input/day01.txt" parser

toInt True = 1
toInt False = 0

do_part1 input = (sum . map toInt) (zipWith (<) input (tail input))

triple l@(a : b : c : xs) = (a + b + c) : (triple (tail l))
triple _ = []

part1 x = do_part1 x

part2 x = do_part1 (triple x)

bench = do
  input <- getInput' "input/day01.txt" parser
  let (!x, !y) = (part1 input, part2 input)
  return ()
