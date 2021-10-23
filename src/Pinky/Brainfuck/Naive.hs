{-# LANGUAGE PartialTypeSignatures #-}

module Pinky.Brainfuck.Naive
  ( interpretBasic,
    interpretBasicTape,
  )
where

import Control.Monad (unless)
import Control.Monad.Trans
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Pinky.Brainfuck.Language
import Pinky.Brainfuck.Machine
import Pinky.Brainfuck.Tape
import Pinky.Brainfuck.Tape.List

-- This is needed because lifting interpretBasic causes the program to
-- never halt
interpretBasicTape ::
  (BfCell c, BrainfuckMachine c m) => Bf -> BfListTape c m ()
interpretBasicTape = traverse_ interpInstr . _bfCode
  where
    interpInstr Increment = modifyCell (+ 1)
    interpInstr Decrement = modifyCell (\n -> n - 1)
    interpInstr MoveLeft = moveHeadLeft
    interpInstr MoveRight = moveHeadRight
    interpInstr Input = do
      char <- lift bfGetChar
      -- TODO Abstract out EOF handling
      writeCell (fromMaybe 0 char)
    interpInstr Output = readCell >>= (lift . bfPutChar)
    interpInstr Debug = pure () -- TODO
    interpInstr (Loop code) = interpLoop
      where
        interpLoop = do
          cell <- readCell
          unless (cell == 0) (interpretBasicTape code >> interpLoop)

interpretBasic :: (BfCell c, BrainfuckMachine c m) => Bf -> m ()
interpretBasic = evalBfListTape . interpretBasicTape
