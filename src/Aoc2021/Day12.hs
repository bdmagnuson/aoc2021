module Aoc2021.Day12
  ( part1,
    part2,
  )
where

import Algebra.Graph
import Aoc2021.AocUtils
import Control.Applicative
import Control.Lens
import Data.Attoparsec.Text qualified as P
import Data.Char (isLower, isUpper)
import Data.List qualified as L
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T

input = getInput "input/day12.txt" parser

data Cave
  = Start
  | End
  | Small Text
  | Big Text
  deriving (Show, Eq, Ord)

parser = many (pEdge <* P.endOfLine)
  where
    pEdge = do
      n1 <- pNode
      P.char '-'
      n2 <- pNode
      return (n1, n2)
    pNode = pStart <|> pEnd <|> pBig <|> pSmall
    pStart = P.string "start" *> pure Start
    pEnd = P.string "end" *> pure End
    pSmall = Small <$> P.takeWhile1 isLower
    pBig = Big <$> P.takeWhile1 isUpper

exits :: Graph Cave -> Cave -> Maybe [Cave]
exits g c = M.lookup c (M.fromList (adjacencyList g))

paths :: Graph Cave -> Cave -> [[Cave]]
paths _ End = [[End]]
paths g n =
  let Just e = exits g n
   in map (n :) (concatMap (paths g') e)
  where
    g' = case n of
      Start -> removeVertex n g
      Small _ -> removeVertex n g
      otherwise -> g

isLinear :: Graph Cave -> Cave -> Bool
isLinear g End = True
isLinear g n =
  case exits g n of
    Nothing -> False
    Just (e : []) -> isLinear (removeVertex n g) e
    Just _ -> False

paths2 :: Graph Cave -> Bool -> Cave -> [[Cave]]
paths2 _ _ End = [[End]]
paths2 g True n =
  case n of
    Start -> map (n :) (concatMap (paths2 g' True) e)
    Big _ -> map (n :) (concatMap (paths2 g True) e)
    Small _ -> map (n :) ((concatMap (paths g) e) ++ concatMap (paths2 g' True) (filter notEnd e))
  where
    Just e = exits g n
    g' = removeVertex n g
    notEnd End = False
    notEnd _ = True

start = edges (input ++ (map (\(x, y) -> (y, x)) input))

part1 = length $ paths start Start

part2 = (length . L.group . L.sort) $ paths2 start True Start
