module Aoc2021.Day05
  ( part1,
    part2,
  )
where

import Aoc2021.AocUtils
import Control.Applicative
import Control.Lens
import qualified Data.Attoparsec.Text as P
import qualified Data.Map as M

input = getInput "input/day05.txt" parser

type Point = (Int, Int)

type VentMap = M.Map Point Int

parser = many pLine
  where
    pLine = do
      x1 <- P.decimal
      P.char ','
      y1 <- P.decimal
      P.string " -> "
      x2 <- P.decimal
      P.char ','
      y2 <- P.decimal
      P.endOfLine
      return ((x1, y1), (x2, y2))

diag :: (Point, Point) -> [Point]
diag (p1@(x1, y1), (x2, y2)) = take (abs (x1 - x2) + 1) (iterate f p1)
  where
    f = bimap f1 f2
    f1 = if x1 > x2 then pred else succ
    f2 = if y1 > y2 then pred else succ

hvLine :: (Point, Point) -> [Point]
hvLine ((x1, y1), (x2, y2))
  | x1 == x2 = [(x1, y) | y <- [min y1 y2 .. max y1 y2]]
  | y1 == y2 = [(x, y1) | x <- [min x1 x2 .. max x1 x2]]
  | otherwise = error "bad"

inc :: VentMap -> Point -> VentMap
inc m p = m & at p . non 0 %~ succ

ventMap1 = foldl go M.empty input
  where
    go m p@((x1, y1), (x2, y2))
      | (x1 == x2) || (y1 == y2) = foldl inc m (hvLine p)
      | otherwise = m

ventMap2 = foldl go M.empty input
  where
    go m p@((x1, y1), (x2, y2))
      | (x1 == x2) || (y1 == y2) = foldl inc m (hvLine p)
      | otherwise = foldl inc m (diag p)

gt2 = M.size . M.filter (\x -> x > 1)

part1 = gt2 ventMap1

part2 = gt2 ventMap2
