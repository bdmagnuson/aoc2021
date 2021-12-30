module Aoc2021.Day23
  ( part1,
    part2,
  )
where

import Aoc2021.AocUtils
import Control.Applicative hiding (empty)
import Control.Lens
import Data.Attoparsec.Text qualified as P
import Data.Heap qualified as H
import Data.Map qualified as M
import Data.Set qualified as S
import Debug.Trace

data Amph = Amber | Bronze | Copper | Desert deriving (Eq, Ord, Enum, Bounded)

instance Show Amph where
  show Amber = "A"
  show Bronze = "B"
  show Copper = "C"
  show Desert = "D"

data Loc = Room Amph Int | Hall Int deriving (Eq, Ord, Show)

type Burrow = M.Map Loc Amph

type Next = H.MinPrioHeap Int Burrow

start =
  M.fromList
    [ (Room Amber 0, Copper),
      (Room Amber 1, Desert),
      (Room Amber 2, Desert),
      (Room Amber 3, Bronze),
      (Room Bronze 0, Bronze),
      (Room Bronze 1, Copper),
      (Room Bronze 2, Bronze),
      (Room Bronze 3, Copper),
      (Room Copper 0, Amber),
      (Room Copper 1, Bronze),
      (Room Copper 2, Amber),
      (Room Copper 3, Desert),
      (Room Desert 0, Desert),
      (Room Desert 1, Amber),
      (Room Desert 2, Copper),
      (Room Desert 3, Amber)
    ]

test =
  M.fromList
    [ (Room Amber 0, Bronze),
      (Room Amber 1, Desert),
      (Room Amber 2, Desert),
      (Room Amber 3, Amber),
      (Room Bronze 0, Copper),
      (Room Bronze 1, Copper),
      (Room Bronze 2, Bronze),
      (Room Bronze 3, Desert),
      (Room Copper 0, Bronze),
      (Room Copper 1, Bronze),
      (Room Copper 2, Amber),
      (Room Copper 3, Copper),
      (Room Desert 0, Desert),
      (Room Desert 1, Amber),
      (Room Desert 2, Copper),
      (Room Desert 3, Amber)
    ]

done = M.fromList [(Room c i, c) | c <- [Amber, Bronze, Copper, Desert], i <- [0 .. 3]]

done' =
  done
    & at (Room Desert 0) .~ Nothing
    & at (Hall 10) .~ Just Desert
    & at (Room Amber 0) .~ Nothing
    & at (Hall 9) .~ Just Amber

{- I hate this code but it works -}
path :: Loc -> Loc -> [Loc]
path s e =
  case (s, e) of
    (Room c1 i1, Room c2 i2) ->
      exit c1 i1 ++ hall c1 c2 ++ enter c2 i2
    (Room c1 i1, Hall i2) ->
      exit c1 i1 ++ hall' (2 + 2 * fromEnum c1) i2
    (Hall i1, Room c2 i2) ->
      hall'' i1 (2 + 2 * fromEnum c2) ++ enter c2 i2
  where
    exit c i = [Room c i' | i' <- reverse [0 .. i - 1]]
    enter c i = [Room c i' | i' <- [0 .. i]]
    hall c1 c2 =
      let a = [Hall i' | i' <- [2 + 2 * fromEnum (min c1 c2) .. 2 + 2 * fromEnum (max c1 c2)]]
       in if c2 > c1 then a else reverse a
    hall' i1 i2 =
      if i1 < i2
        then [Hall i' | i' <- [i1 .. i2]]
        else reverse [Hall i' | i' <- [i2 .. i1]]
    hall'' i1 i2 =
      if i1 < i2
        then [Hall i' | i' <- [i1 + 1 .. i2]]
        else reverse [Hall i' | i' <- [i2 .. i1 - 1]]

cost Amber = 1
cost Bronze = 10
cost Copper = 100
cost Desert = 1000

showBurrow b = do
  putStrLn "#############"
  putStr "#"
  putStr hall
  putStrLn "#"
  putStr
    "###"
    >> putStr (f (Room Amber 0))
    >> putStr "#"
    >> putStr (f (Room Bronze 0))
    >> putStr "#"
    >> putStr (f (Room Copper 0))
    >> putStr "#"
    >> putStr (f (Room Desert 0))
    >> putStrLn "###"
  putStr
    "  #"
    >> putStr (f (Room Amber 1))
    >> putStr "#"
    >> putStr (f (Room Bronze 1))
    >> putStr "#"
    >> putStr (f (Room Copper 1))
    >> putStr "#"
    >> putStr (f (Room Desert 1))
    >> putStrLn "#"
  putStr
    "  #"
    >> putStr (f (Room Amber 2))
    >> putStr "#"
    >> putStr (f (Room Bronze 2))
    >> putStr "#"
    >> putStr (f (Room Copper 2))
    >> putStr "#"
    >> putStr (f (Room Desert 2))
    >> putStrLn "#"
  putStr
    "  #"
    >> putStr (f (Room Amber 3))
    >> putStr "#"
    >> putStr (f (Room Bronze 3))
    >> putStr "#"
    >> putStr (f (Room Copper 3))
    >> putStr "#"
    >> putStr (f (Room Desert 3))
    >> putStrLn "#"
  putStrLn "  #########"
  where
    hall = concatMap f [Hall i | i <- [0 .. 10]]
    f x =
      case x `M.lookup` b of
        Just a -> show a
        Nothing -> "."

boundary :: Burrow -> [(Int, Burrow)]
boundary b = concatMap (map g . filter (validPath b) . mkPath b) (M.assocs b)
  where
    g :: (Amph, Loc, Loc) -> (Int, Burrow)
    g (a, s, e) =
      let b' =
            b & at s .~ Nothing
              & at e .~ Just a
          c = cost a * (length $ path s e)
       in (c, b')

validPath :: Burrow -> (Amph, Loc, Loc) -> Bool
validPath b (_, s, e) = all (\x -> not (x `M.member` b)) (path s e)

mkPath :: Burrow -> (Loc, Amph) -> [(Amph, Loc, Loc)]
mkPath b (Room c i, a)
  | c == a && and [b ^? ix (Room c i') == Just c | i' <- [i + 1 .. 3]] = []
  | otherwise = home : hall
  where
    hall = [(a, Room c i, Hall i') | i' <- [0, 1, 3, 5, 7, 9, 10]]
    home
      | b ^? ix (Room a 3) /= Just a = (a, Room c i, Room a 3)
      | b ^? ix (Room a 2) /= Just a = (a, Room c i, Room a 2)
      | b ^? ix (Room a 1) /= Just a = (a, Room c i, Room a 1)
      | b ^? ix (Room a 0) /= Just a = (a, Room c i, Room a 0)
mkPath b (Hall i, a) = [home]
  where
    home
      | b ^? ix (Room a 3) /= Just a = (a, Hall i, Room a 3)
      | b ^? ix (Room a 2) /= Just a = (a, Hall i, Room a 2)
      | b ^? ix (Room a 1) /= Just a = (a, Hall i, Room a 1)
      | b ^? ix (Room a 0) /= Just a = (a, Hall i, Room a 0)

minGraph :: Burrow -> Int
minGraph b = go (H.fromList (boundary b)) (S.singleton b)
  where
    go :: Next -> S.Set Burrow -> Int
    go h s
      | H.size h == 0 = error "no solution"
      | dst == done = cost
      | dst `S.member` s = go (H.drop 1 h) s
      | otherwise = go h' s'
      where
        Just (cost, dst) = H.viewHead h
        newNodes = filter (\(_, x) -> not (x `S.member` s)) (boundary dst)
        newNodes' = newNodes & traversed . _1 %~ (+ cost)
        h' = (H.drop 1 h) `H.union` (H.fromList newNodes')
        s' = dst `S.insert` s

part1 = 11320

part2 = minGraph start
