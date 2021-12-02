{-# LANGUAGE OverloadedStrings #-}
module Day02
  ( part1
  , part2
  ) where

import AocUtils

import qualified Data.Attoparsec.Text as P
import Control.Applicative
import Control.Monad.State.Strict

data Dir
  = Forward Int
  | Down Int
  | Up Int deriving (Show)

data Loc = Loc
  { horz  :: Int
  , depth :: Int
  , aim   :: Int
  } deriving (Show)

input = getInput "input/day02.txt" parser

parser = many ((pForward <|> pDown <|> pUp) <* P.endOfLine)
  where
    pForward = Forward <$> (P.string "forward " *> P.decimal)
    pDown    =    Down <$> (P.string    "down " *> P.decimal)
    pUp      =      Up <$> (P.string      "up " *> P.decimal)

move1 :: Dir -> State Loc ()
move1 x = do
  case x of
    Forward d -> modify (\x -> x { horz  = horz x  + d })
    Down    d -> modify (\x -> x { depth = depth x + d })
    Up      d -> modify (\x -> x { depth = depth x - d })
  return ()

move2 :: Dir -> State Loc ()
move2 x = do
  case x of
    Forward d -> modify (\x -> x { horz  = horz x + d,
                                   depth = depth x + (aim x * d) })
    Down    d -> modify (\x -> x { aim   = aim  x + d })
    Up      d -> modify (\x -> x { aim   = aim x - d })
  return ()

part1 = let Loc h z _ = execState (traverse move1 input) (Loc 0 0 0) in h * z
part2 = let Loc h z _ = execState (traverse move2 input) (Loc 0 0 0) in h * z

