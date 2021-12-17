{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}

module Aoc2021.Day16
  ( part1,
    part2,
  )
where

import Aoc2021.AocUtils
import Control.Applicative
import Control.Lens
import Control.Monad.State.Strict
import Data.Attoparsec.Text qualified as P
import Data.Char (isAlphaNum)
import Data.Data
import Data.List (foldl')
import Data.Text qualified as T

type Stream = [Integer]

data Packet = Packet
  { _pkt_version :: Integer,
    _pkt_type :: Integer,
    _payload :: Payload
  }
  deriving (Show, Data)

data Payload
  = Literal Integer
  | Operator [Packet]
  deriving (Show, Data)

$(makeLenses ''Packet)
$(makePrisms ''Payload)

deriving instance Plated Packet

input = getInput "input/day16.txt" (P.takeWhile1 isAlphaNum <* P.endOfLine)

toL = concatMap f . T.unpack
  where
    f '0' = [0, 0, 0, 0]
    f '1' = [0, 0, 0, 1]
    f '2' = [0, 0, 1, 0]
    f '3' = [0, 0, 1, 1]
    f '4' = [0, 1, 0, 0]
    f '5' = [0, 1, 0, 1]
    f '6' = [0, 1, 1, 0]
    f '7' = [0, 1, 1, 1]
    f '8' = [1, 0, 0, 0]
    f '9' = [1, 0, 0, 1]
    f 'A' = [1, 0, 1, 0]
    f 'B' = [1, 0, 1, 1]
    f 'C' = [1, 1, 0, 0]
    f 'D' = [1, 1, 0, 1]
    f 'E' = [1, 1, 1, 0]
    f 'F' = [1, 1, 1, 1]

pPacket :: State Stream Packet
pPacket = do
  v <- pInt 3
  t <- pInt 3
  case t of
    4 -> do
      l <- pLiteral
      return $ Packet v t (Literal l)
    otherwise -> do
      t' <- pInt 1
      case t' of
        0 -> do
          l <- pInt 15
          r <- pTake l
          let p = evalState parsePackets r
          return $ Packet v t (Operator p)
        1 -> do
          l <- pInt 11
          p <- parseNPackets l
          return $ Packet v t (Operator p)

parsePackets :: State Stream [Packet]
parsePackets = do
  s <- get
  case null s of
    True -> return []
    False -> do
      p <- pPacket
      ps <- parsePackets
      return (p : ps)

parseNPackets :: Integer -> State Stream [Packet]
parseNPackets n = replicateM (fromIntegral n) pPacket

pLiteral :: State [Integer] Integer
pLiteral = pLitDig >>= (\x -> return (b2i x))
  where
    b2i :: [Integer] -> Integer
    b2i = foldl' (\n d -> n * 2 + d) 0
    pLitDig :: State [Integer] [Integer]
    pLitDig = do
      l <- pTake 1
      r <- pTake 4
      case l of
        [0] -> return r
        [1] -> pLitDig >>= (\x -> return (r ++ x))
        x -> error (show x)

pTake :: Integer -> State Stream [Integer]
pTake n = do
  s <- get
  let (t, r) = splitAt (fromIntegral n) s
  put r
  return t

pInt :: Integer -> State Stream Integer
pInt n = do
  i <- pTake n
  return $ foldl' go 0 i
  where
    go n d = n * 2 + d

eval :: Packet -> Integer
eval p =
  case p ^. pkt_type of
    4 -> p ^?! payload . _Literal
    0 -> sum sub
    1 -> product sub
    2 -> minimum sub
    3 -> maximum sub
    5 -> if first > second then 1 else 0
    6 -> if first < second then 1 else 0
    7 -> if first == second then 1 else 0
  where
    sub = map eval (p ^. payload . _Operator)
    first = eval (p ^?! payload . _Operator . ix 0)
    second = eval (p ^?! payload . _Operator . ix 1)

part1 = let a = evalState pPacket (toL input) in sum $ universe a ^.. traverse . pkt_version

part2 = eval (evalState pPacket (toL input))
