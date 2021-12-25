module Aoc2021.Day19
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
import Data.List (permutations, (\\))
import Data.Set qualified as S
import Debug.Trace
import Linear

type Coords = [V3 Int]

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

rotations :: [M33 Int]
rotations = do
  [x, y, z] <- permutations [V3 1 0 0, V3 0 1 0, V3 0 0 1]
  sx <- [1, -1]
  sy <- [1, -1]
  sz <- [1, -1]
  let m = V3 (x * sx) (y * sy) (z * sz)
  guard (det33 m == 1)
  return m

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

locateN :: Coords -> [Coords] -> Maybe (Coords, V3 Int)
locateN r as = asum (map (locate1 r) as)

locate1 :: Coords -> Coords -> Maybe (Coords, V3 Int)
locate1 rs as = asum $ do
  a <- as
  rot <- rotations
  let rs' = map (*! rot) rs
  r' <- rs'
  let d = a - r'
  let r'' = map (+ d) rs'
  let l = lengthOf (folded . filtered (\x -> x `elem` as)) r''
  return (if l >= 12 then Just (r'', d) else Nothing)

solve = locate input

part1 = S.size $ foldl (\x y -> S.union x (S.fromList y)) S.empty (map fst solve)

part2 = let a = map snd solve in maximum [norm x y | x <- a, y <- a, x /= y]
  where
    norm (V3 a1 a2 a3) (V3 b1 b2 b3) = abs (a1 - b1) + abs (a2 - b2) + abs (a3 - b3)
