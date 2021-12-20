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
import Data.List (sort, (\\))
import Data.Set qualified as S
import Debug.Trace

type Coords = [(Int, Int, Int)]

input :: [Coords]
input = getInput "input/day19.txt" parser

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
      return $ map (\(a : b : c : []) -> (a, b, c)) c

rotations :: [(Int, Int, Int) -> (Int, Int, Int)]
rotations =
  concat
    [ map (. id) g,
      map (. r90x) g,
      map (. r180x) g,
      map (. r270x) g,
      map (. r90y) g,
      map (. r270y) g
    ]
  where
    r90x (x, y, z) = (x, z, -y)
    r90y (x, y, z) = (z, y, -x)
    r90z (x, y, z) = (y, -x, z)
    r180x = r90x . r90x
    r180y = r90y . r90y
    r180z = r90z . r90z
    r270x = r90x . r90x . r90x
    r270y = r90y . r90y . r90y
    r270z = r90z . r90z . r90z
    g = [id, r90z, r180z, r270z]

locate :: [Coords] -> [(Coords, (Int, Int, Int))]
locate [] = []
locate (x : xs) = go [(x, (0, 0, 0))] xs
  where
    go xs [] = xs
    go xs ys = go (g' : xs) (ys \\ [g])
      where
        (g, g') = foo ys
        foo [] = error "no match"
        foo (y : ys) = case locateN y (map fst xs) of
          Just a -> (y, a)
          Nothing -> foo ys

locateN :: Coords -> [Coords] -> Maybe (Coords, (Int, Int, Int))
locateN r as = asum (map (locate1 r) as)

locate1 :: Coords -> Coords -> Maybe (Coords, (Int, Int, Int))
locate1 r a = asum $ do
  (ax, ay, az) <- a
  rot <- rotations
  let r' = map rot r
  (rx, ry, rz) <- r'
  let (dx, dy, dz) = (ax - rx, ay - ry, az - rz)
  let r'' = map (\(x, y, z) -> (x + dx, y + dy, z + dz)) r'
  let l = lengthOf (folded . filtered (\x -> x `elem` a)) r''
  return (if l >= 12 then Just (r'', (dx, dy, dz)) else Nothing)

solve = locate input

part1 = S.size $ foldl (\x y -> S.union x (S.fromList y)) S.empty (map fst solve)

part2 = let a = map snd solve in maximum [norm x y | x <- a, y <- a, x /= y]
  where
    norm (a1, a2, a3) (b1, b2, b3) = abs (a1 - b1) + abs (a2 - b2) + abs (a3 - b3)
