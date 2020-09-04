module Pinky.Brainfuck.Naive
  ( interpretBasic,
    interpretBasicTape,
  )
where

import Data.Maybe (fromMaybe)
import Control.Monad (unless)
import Data.Foldable (traverse_)
import Control.Monad.Trans
import Pinky.Brainfuck.Language
import Pinky.Brainfuck.Machine
import Pinky.Brainfuck.Tape

-- This is needed because lifting interpretBasic causes the program to
-- never halt
interpretBasicTape ::
  (BfCell c, BrainfuckMachine m c) => Bf -> BrainfuckTape c m ()
interpretBasicTape = traverse_ interpInstr . _bfCode
  where
    interpInstr Increment = modifyCell (+ 1)
    interpInstr Decrement = modifyCell (\n -> n - 1)
    interpInstr MoveLeft = moveHeadLeft
    interpInstr MoveRight = moveHeadRight
    interpInstr Input = do
      char <- lift bfGetChar
      -- TODO Abstract out EOF handling
      setCell (fromMaybe 0 char)
    interpInstr Output = readCell >>= (lift . bfPutChar)
    interpInstr Debug = pure () -- TODO
    interpInstr (Loop code) = interpLoop
      where
        interpLoop = do
          cell <- readCell
          unless (cell == 0) (interpretBasicTape code >> interpLoop)

interpretBasic :: (BfCell c, BrainfuckMachine m c) => Bf -> m ()
interpretBasic = evalBfTape . interpretBasicTape
