module Aoc2021.Day15
  ( part1,
    part2,
  )
where

import Algebra.Graph.AdjacencyIntMap
import Aoc2021.AocUtils
import Control.Applicative hiding (empty)
import Control.Lens
import Data.Attoparsec.Text qualified as P
import Data.Char (ord)
import Data.Heap qualified as H
import Data.IntMap qualified as M
import Data.IntSet qualified as S
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Debug.Trace

parser = many (many ((subtract 48 . ord) <$> P.digit) <* P.endOfLine)

input = getInput "input/day15_test.txt" parser

input' = getInput "input/day15.txt" parser

type Node = Int

type Table = M.IntMap Int

type Next = H.MinPrioHeap Int (Int, Int)

expandGraph x =
  concat
    [ foo [fs !! 0, fs !! 1, fs !! 2, fs !! 3, fs !! 4],
      foo [fs !! 1, fs !! 2, fs !! 3, fs !! 4, fs !! 5],
      foo [fs !! 2, fs !! 3, fs !! 4, fs !! 5, fs !! 6],
      foo [fs !! 3, fs !! 4, fs !! 5, fs !! 6, fs !! 7],
      foo [fs !! 4, fs !! 5, fs !! 6, fs !! 7, fs !! 8]
    ]
  where
    foo xs = foldl1 (zipWith (++)) xs
    fs = map ($ x) (map f [0 .. 8])
    f n = (fmap . fmap) (inc n)
    inc n = \x -> let a = x + n in if a >= 10 then a - 9 else a

mkGraph :: Int -> Int -> AdjacencyIntMap
mkGraph h w = overlay g (transpose g)
  where
    g = edges $ concatMap f [(x, y) | x <- [0 .. w - 1], y <- [0 .. h - 1]]
    f (x, y)
      | (x == w - 1 && y == h - 1) = []
      | (x == w - 1) = [(label, down)]
      | (y == h - 1) = [(label, right)]
      | otherwise = [(label, down), (label, right)]
      where
        label = y * w + x
        down = label + w
        right = label + 1

g x = mkGraph (length x) (length (x !! 0))

c x = M.fromList $ zip [0 ..] (concat x)

minGraph :: AdjacencyIntMap -> Table -> Int -> AdjacencyIntMap
minGraph g c s = foldl' overlay empty (go (newNodes 0 s) (M.singleton s 0))
  where
    go :: Next -> Table -> [AdjacencyIntMap]
    go h m
      | H.size h == 0 = [empty]
      | Just (_, (_, dst)) <- H.viewHead h,
        dst `M.member` m =
          go (H.drop 1 h) m
      | otherwise = (edge src dst) : (go h' m')
      where
        Just (cost, (src, dst)) = H.viewHead h
        m' = m & at dst .~ Just cost
        h' = (H.drop 1 h) `H.union` (newNodes cost dst)
    adj = M.fromList (adjacencyList g)
    newNodes cost dst = H.fromList (map f (adj ^?! ix dst))
      where
        f n = (cost + c ^?! ix n, (dst, n))

-- Direct version which exits with the cost right away vs building the whole min graph
-- Not actually a lot faster since apparently we walk most of the graphs anyway
minGraph' :: AdjacencyIntMap -> Table -> Int -> Int -> Int
minGraph' g c s e = go (newNodes 0 s) (M.singleton s 0)
  where
    go :: Next -> Table -> Int
    go h m
      | H.size h == 0 = error "f'd up"
      | Just (cost, (_, dst)) <- H.viewHead h,
        dst == e =
          cost
      | Just (_, (_, dst)) <- H.viewHead h,
        dst `M.member` m =
          go (H.drop 1 h) m
      | otherwise = go h' m'
      where
        Just (cost, (src, dst)) = H.viewHead h
        m' = m & at dst .~ Just cost
        h' = (H.drop 1 h) `H.union` (newNodes cost dst)
    adj = M.fromList (adjacencyList g)
    newNodes cost dst = H.fromList (map f (adj ^?! ix dst))
      where
        f n = (cost + c ^?! ix n, (dst, n))

minCost :: AdjacencyIntMap -> Table -> Int -> Int -> Int
minCost g c s e = go e
  where
    m = (M.fromList . adjacencyList . transpose) (minGraph g c s)
    go x = if x == s then 0 else (c ^?! ix x + go (m ^?! ix x . ix 0))

solve1 x = minCost (g x) (c x) 0 (length (concat x) - 1)

part1 = solve1 input'

part2 = solve1 (expandGraph input')
