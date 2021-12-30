module Aoc2021.Day24
  ( part1,
    part2,
  )
where

import Aoc2021.AocUtils
import Control.Applicative
import Control.Lens
import Control.Monad.State.Strict
import Data.Attoparsec.Text qualified as P
import Data.Map qualified as M
import Debug.Trace
import System.Random

data Var = W | X | Y | Z | Lit Int deriving (Show, Ord, Eq)

data Instruction
  = Inp Var
  | Add Var Var
  | Mul Var Var
  | Div Var Var
  | Mod Var Var
  | Eql Var Var
  deriving (Show)

data ALU = ALU
  { _regs :: M.Map Var Int,
    _input :: [Int]
  }
  deriving (Show)

$(makeLenses ''ALU)

program = getInput "input/day24.txt" parser

parser = many ((pInp <|> pOther) <* P.endOfLine)
  where
    pInp = P.string "inp " *> (Inp <$> pVar)
    pVar =
      (P.char 'x' *> pure X)
        <|> (P.char 'y' *> pure Y)
        <|> (P.char 'z' *> pure Z)
        <|> (P.char 'w' *> pure W)
    pOther =
      (uncurry Add <$> pInstr "add ")
        <|> (uncurry Mul <$> pInstr "mul ")
        <|> (uncurry Div <$> pInstr "div ")
        <|> (uncurry Mod <$> pInstr "mod ")
        <|> (uncurry Eql <$> pInstr "eql ")
    pInstr c = do
      P.string c
      arg1 <- pVar
      P.char ' '
      arg2 <- pVar <|> Lit <$> P.signed P.decimal
      return $ (arg1, arg2)

runProgram x i = execState (traverse eval x) (ALU M.empty i)

search = do
  g <- newStdGen
  let r = take 14 $ randomRs (1, 9) g
  let p = runProgram program r
  if p ^?! regs . ix Z == 0 then putStrLn (show r) else search

getArg (Lit n) = return n
getArg var = use (regs . at var . non 0)

eval :: Instruction -> State ALU ()
eval (Inp r1) = do
  x <- use input
  regs . at r1 .= Just (head x)
  input .= tail x
  return ()
eval (Add r1 r2) = do
  arg1 <- getArg r1
  arg2 <- getArg r2
  regs . at r1 .= Just (arg1 + arg2)
  return ()
eval (Mul r1 r2) = do
  arg1 <- getArg r1
  arg2 <- getArg r2
  regs . at r1 .= Just (arg1 * arg2)
  return ()
eval (Div r1 r2) = do
  arg1 <- getArg r1
  arg2 <- getArg r2
  regs . at r1 .= Just (arg1 `div` arg2)
  return ()
eval (Mod r1 r2) = do
  arg1 <- getArg r1
  arg2 <- getArg r2
  regs . at r1 .= Just (arg1 `mod` arg2)
  return ()
eval (Eql r1 r2) = do
  arg1 <- getArg r1
  arg2 <- getArg r2
  regs . at r1 .= Just (if arg1 == arg2 then 1 else 0)
  return ()

evalRev :: Instruction -> State ALU ()
evalRev (Inp r1) = do
  x <- use input
  regs . at r1 .= Just (head x)
  input .= tail x
  return ()
evalRev (Add r1 r2) = do
  arg1 <- getArg r1
  arg2 <- getArg r2
  regs . at r1 .= Just (arg1 + arg2)
  return ()
evalRev (Mul r1 r2) = do
  arg1 <- getArg r1
  arg2 <- getArg r2
  regs . at r1 .= Just (arg1 * arg2)
  return ()
evalRev (Div r1 r2) = do
  arg1 <- getArg r1
  arg2 <- getArg r2
  regs . at r1 .= Just (arg1 `div` arg2)
  return ()
evalRev (Mod r1 r2) = do
  arg1 <- getArg r1
  arg2 <- getArg r2
  regs . at r1 .= Just (arg1 `mod` arg2)
  return ()
evalRev (Eql r1 r2) = do
  arg1 <- getArg r1
  arg2 <- getArg r2
  regs . at r1 .= Just (if arg1 == arg2 then 1 else 0)
  return ()

part1 = 89959794919939

part2 = 17115131916112
