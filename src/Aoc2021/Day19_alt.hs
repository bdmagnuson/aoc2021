module Aoc2021.Day19_alt
  ( part1,
    part2,
  )
where

import Aoc2021.AocUtils
import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Attoparsec.Text qualified as P
import Data.Foldable (asum)
import Data.Function (on)
import Data.List (find, groupBy, permutations, sort, sortOn, tails, (\\))
import Data.Map qualified as M
import Data.Ord (comparing)
import Data.Set qualified as S
import Debug.Trace
import Linear

type Coords = [V3 Int]

type Point = V3 Int

input :: [Coords]
input = getInput "input/day19_test.txt" parser

decimal :: P.Parser Int
decimal = P.signed P.decimal

parser :: P.Parser [Coords]
parser = pScanner `P.sepBy` P.endOfLine
  where
    pScanner :: P.Parser Coords
    pScanner = do
      P.string "--- scanner "
      P.decimal
      P.string " ---"
      P.endOfLine
      c <- many ((decimal `P.sepBy1` P.char ',') <* P.endOfLine)
      return $ map (\(a : b : c : []) -> V3 a b c) c

norm' (V3 a b c) = (a * a + b * b + c * c)

sortV3 (V3 a b c) = let [a', b', c'] = sort [a, b, c] in V3 a' b' c'

pairDists ps =
  M.fromList
    [ (sortV3 $ abs $ p1 - p2, (p1, p2))
      | p1 : ps' <- tails ps,
        p2 <- ps'
    ]

getTransform :: (Point, Point) -> (Point, Point) -> (V3 Int, M33 Int)
getTransform (l1, l2) (r1, r2) = (delta, r)
  where
    dl = l1 - l2
    dr = r1 - r2
    r = vecRotation dl dr
    delta = l1 - r !* r1

tMatrix :: [M33 Int]
tMatrix = [V3 (x * sx) (y * sy) (z * sz) | [x, y, z] <- permutations [V3 1 0 0, V3 0 1 0, V3 0 0 1], sx <- signs, sy <- signs, sz <- signs]
  where
    signs = [1, -1]

vecRotation :: V3 Int -> V3 Int -> M33 Int
vecRotation l r = head [p | p <- tMatrix, p !* r == l]

assocPts p1 p2 = M.elems $ M.intersectionWith (,) (pairDists p1) (pairDists p2)

cc x y = fst x == fst y

findTransform x y = fmap head $ find (\x -> length x >= 12) $ groupBy cc $ sort $ map (uncurry getTransform) $ assocPts x y

locate1 :: Coords -> Coords -> Maybe (Coords, V3 Int)
locate1 r a =
  case findTransform a r of
    Nothing -> Nothing
    Just (offset, rot) -> Just (map (\x -> x *! rot + offset) r, offset)

locateN :: Coords -> [Coords] -> Maybe (Coords, V3 Int)
locateN r as = asum (map (locate1 r) as)

locate :: [Coords] -> [(Coords, V3 Int)]
locate [] = []
locate (x : xs) = go [(x, V3 0 0 0)] xs
  where
    go xs [] = xs
    go xs ys = go (g' : xs) (ys \\ [g])
      where
        (g, g') = foo ys
        foo [] = error "no match"
        foo (y : ys) = case locateN y (map fst xs) of
          Just a -> (y, a)
          Nothing -> foo ys

solve = locate input

part1 = S.size $ foldl (\x y -> S.union x (S.fromList y)) S.empty (map fst solve)

part2 = let a = map snd solve in maximum [norm x y | x <- a, y <- a, x /= y]
  where
    norm (V3 a1 a2 a3) (V3 b1 b2 b3) = abs (a1 - b1) + abs (a2 - b2) + abs (a3 - b3)
