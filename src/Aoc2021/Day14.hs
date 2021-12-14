module Aoc2021.Day14
  ( part1,
    part2,
  )
where

import Aoc2021.AocUtils
import Control.Applicative
import Control.Lens
import Data.Attoparsec.Text qualified as P
import Data.Char (isAlpha)
import Data.Function.Memoize
import Data.List (group, sort)
import Data.Map qualified as M
import Data.Text qualified as T

(start, rules) = getInput "input/day14.txt" parser

parser = do
  start <- pString <* P.endOfLine
  P.endOfLine
  rules <- M.fromList <$> many (pRule <* P.endOfLine)
  return (start, rules)
  where
    pString = P.takeWhile1 isAlpha
    pRule = do
      left <- (,) <$> P.anyChar <*> P.anyChar
      P.string " -> "
      right <- P.anyChar
      return (left, right)

emptyCounts = M.fromList $ zip (M.elems rules) (repeat 0)

counts :: Integer -> (Char, Char) -> M.Map Char Integer
counts = memoize2 f
  where
    f 0 (x, y) = emptyCounts & ix x +~ 1 & ix y +~ 1
    f n (x, y) = let c = rules ^?! ix (x, y) in M.unionWith (+) (counts (n - 1) (x, c)) (counts (n - 1) (c, y)) & ix c -~ 1

score x n =
  let correction = T.foldl (\m k -> m & ix k +~ 1) emptyCounts (T.init (T.tail x))
      res = foldl (M.unionWith (+)) emptyCounts ((map (counts n)) (T.zip x (T.tail x)))
      res' = M.unionWith (-) res correction
   in maximum res' - minimum res'

part1 = score start 10

part2 = score start 40
