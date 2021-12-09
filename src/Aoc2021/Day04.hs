
module Aoc2021.Day04
  ( part1,
    part2,
  )
where

import Aoc2021.AocUtils
import Control.Applicative
import Control.Lens
import qualified Data.Attoparsec.Text as P
import qualified Data.IntMap as M
import Data.List

type Board = M.IntMap Spot

data Spot = Spot
  { _loc :: (Int, Int),
    _covered :: Bool
  }
  deriving (Show)

$(makeLenses ''Spot)

input = getInput "input/day04.txt" parser

parser = do
  draws <- (P.decimal `P.sepBy` (P.char ',')) <* P.endOfLine
  P.endOfLine
  boards <- many pBoard
  return (draws, map toBoard boards)
  where
    pBoard = P.count 25 (P.skipWhile (\x -> (x == ' ') || (x == '\n')) *> P.decimal)
    toBoard b = M.fromList $ zip b [Spot (x, y) False | x <- [0 .. 4], y <- [0 .. 4]]

playMove :: Int -> [Board] -> [Board]
playMove x = map call
  where
    call b = b & ix x . covered .~ True

isBingo :: Board -> Bool
isBingo board = or $ map (\x -> M.size x == 5) (concat [rows, cols])
  where
    rows :: [Board]
    rows = map (\idx -> M.filter (\x -> x ^. loc . _1 == idx && x ^. covered) board) [0 .. 4]
    cols = map (\idx -> M.filter (\x -> x ^. loc . _2 == idx && x ^. covered) board) [0 .. 4]

score x b = (sum . M.keys) (M.filter (\x -> not (x ^. covered)) b) * x

part1 :: Int
part1 = go input
  where
    go ([], _) = error "no winner?"
    go (x : xs, bs) =
      let bs' = playMove x bs
       in case find isBingo bs' of
            Just b -> score x b
            Nothing -> go (xs, bs')

part2 = go input
  where
    go ([], _) = error "no winner?"
    go (x : xs, bs) =
      case partition isBingo (playMove x bs) of
        (b : [], []) -> score x b
        (_, bs') -> go (xs, bs')
