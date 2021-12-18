module Aoc2021.Day17
  ( part1,
    part2,
  )
where

import Aoc2021.AocUtils
import Control.Applicative
import Data.Attoparsec.Text qualified as P
import Data.Maybe (catMaybes)

pos (px, py) (dx, dy) = (px, py) : (pos (px', py') (dx', dy'))
  where
    px' = px + dx
    py' = py + dy
    dx' = if dx == 0 then 0 else dx - 1
    dy' = dy - 1

type Target = ((Int, Int), (Int, Int))

peak :: (Int, Int) -> Target -> Maybe Int
peak (dx, dy) ((min_x, max_x), (min_y, max_y)) = if any pHit l then Just (maximum (map snd l)) else Nothing
  where
    l = takeWhile (not . pBeyond) (pos (0, 0) (dx, dy))
    pHit (x, y) = (x >= min_x) && (x <= max_x) && (y >= min_y) && (y <= max_y)
    pBeyond (x, y) = x > max_x || y < min_y

solve :: Target -> [Int]
solve r@((_, max_x), (min_y, _)) = catMaybes (map (flip peak $ r) start)
  where
    start = [(dx, dy) | dx <- [1 .. max_x], dy <- [min_y .. (-min_y)]]

input = ((138, 184), (-125, -71))

part1 = maximum (solve input)

part2 = length (solve input)
