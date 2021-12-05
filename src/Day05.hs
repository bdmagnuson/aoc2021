module Day05
  ( part1,
    part2,
  )
where

import AocUtils
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
diag (p1@(x1, y1), p2@(x2, y2)) =
  if p1 == p2 then [p1] else (p1 : (diag (nextPt, p2)))
  where
    nextPt =
      ( if x2 > x1 then x1 + 1 else x1 - 1,
        if y2 > y1 then y1 + 1 else y1 - 1
      )

hvLine :: (Point, Point) -> [Point]
hvLine ((x1, y1), (x2, y2)) = [(x, y) | x <- [min x1 x2 .. max x1 x2], y <- [min y1 y2 .. max y1 y2]]

inc :: VentMap -> Point -> VentMap
inc m p = m & at p . non 0 +~ 1

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

gt2 x = (length . M.keys) (M.filter (\x -> x > 1) x)

part1 = gt2 ventMap1

part2 = gt2 ventMap2
