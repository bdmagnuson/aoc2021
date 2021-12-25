module Aoc2021.Day21
  ( part1,
    part2,
  )
where

import Aoc2021.AocUtils
import Control.Applicative
import Control.Lens
import Data.Attoparsec.Text qualified as P
import Data.Function.Memoize
import Data.List (foldl1', unfoldr)

-- input = getInput "input/day09.txt" parser

data Turn
  = Player1_R1
  | Player1_R2
  | Player1_R3
  | Player2_R1
  | Player2_R2
  | Player2_R3
  deriving (Eq)

data Game = Game
  { turn :: Turn,
    loc1 :: Int,
    loc2 :: Int,
    score1 :: Int,
    score2 :: Int,
    rollls :: [Int]
  }

$(makeLenses ''Game)
$(deriveMemoizable ''Turn)
$(deriveMemoizable ''Game)

start = (4, 10)

play (s1, s2) r = unfoldr go (Game Player1_R1 s1 s2 0 0 r)
  where
    go :: Game -> Maybe ((Int, Int), Game)
    go (Game t l1 l2 s1 s2 (r : rs))
      | s1 >= 1000 = Nothing
      | s2 >= 1000 = Nothing
      | t == Player1_R1 =
          let l1' = move l1 r
              s1' = s1 + l1'
           in Just ((s1', s2), Game Player2_R1 l1' l2 s1' s2 rs)
      | t == Player2_R1 =
          let l2' = move l2 r
              s2' = s2 + l2'
           in Just ((s1, s2'), Game Player1_R1 l1 l2' s1 s2' rs)
    move :: Int -> Int -> Int
    move a b = (a - 1 + b) `mod` 10 + 1

build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
build g = g (:) []

chunksOf :: Int -> [e] -> [[e]]
chunksOf i ls = map (take i) (build (splitter ls))
  where
    splitter :: [e] -> ([e] -> a -> a) -> a -> a
    splitter [] _ n = n
    splitter l c n = l `c` splitter (drop i l) c n

rolls :: [Int]
rolls = map sum . chunksOf 3 $ cycle [1 .. 100]

part1 =
  let a = play start rolls
      (s1, s2) = last a
   in if s1 >= 1000
        then s2 * 3 * length a
        else s1 * 3 * length a

solve2 = memoize f
  where
    f (Game t l1 l2 s1 s2 [])
      | start && (s1 >= 21) = [1, 0]
      | start && (s2 >= 21) = [0, 1]
      | otherwise = foldl1' (zipWith (+)) v
      where
        start = (t == Player1_R1 || t == Player2_R1)
        v = case t of
          Player1_R1 -> [solve2 (Game Player1_R2 (move l1 r) l2 s1 s2 []) | r <- [1 .. 3]]
          Player1_R2 -> [solve2 (Game Player1_R3 (move l1 r) l2 s1 s2 []) | r <- [1 .. 3]]
          Player1_R3 -> [solve2 (Game Player2_R1 (move l1 r) l2 (s1 + (move l1 r)) s2 []) | r <- [1 .. 3]]
          Player2_R1 -> [solve2 (Game Player2_R2 l1 (move l2 r) s1 s2 []) | r <- [1 .. 3]]
          Player2_R2 -> [solve2 (Game Player2_R3 l1 (move l2 r) s1 s2 []) | r <- [1 .. 3]]
          Player2_R3 -> [solve2 (Game Player1_R1 l1 (move l2 r) s1 (s2 + (move l2 r)) []) | r <- [1 .. 3]]
    move a b = (a - 1 + b) `mod` 10 + 1

part2 = maximum $ solve2 (Game Player1_R1 4 10 0 0 [])
