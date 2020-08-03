{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Pinky.Brainfuck.Language
  ( BfInstr (..),
    Bf (..),
    printBfInstr,
    printBf,
  )
where

-- | Single Brainfuck instruction (loop is counted as a single instruction)
--
-- Includes the main instructions and a Debug command.
data BfInstr
  = Increment
  | Decrement
  | MoveLeft
  | MoveRight
  | Loop Bf -- TODO Just use [BfInstr]?
  | Input
  | Output
  | Debug
  deriving (Show, Eq)

-- | Full program program / code snippet
newtype Bf = Bf {_bfCode :: [BfInstr]} deriving (Show, Eq, Semigroup, Monoid)

printBfInstr :: BfInstr -> String
printBfInstr Increment = "+"
printBfInstr Decrement = "-"
printBfInstr MoveLeft = "<"
printBfInstr MoveRight = ">"
printBfInstr Input = ","
printBfInstr Output = "."
printBfInstr Debug = "#"
printBfInstr (Loop bf) = "[" ++ printBf bf ++ "]"

printBf :: Bf -> String
printBf (Bf bf) = concatMap printBfInstr bf
