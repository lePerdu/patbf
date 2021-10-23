{-# LANGUAGE FunctionalDependencies #-}

module Pinky.Brainfuck.Tape
  ( MonadBfTape (..),
    BfError (..),
  )
where

import Control.Exception (Exception (..))
import Control.Monad (replicateM_)
import Control.Monad.Trans (MonadTrans)
import Pinky.Brainfuck.Machine (BfCell)
import Type.Reflection (Typeable)

data BfError
  = BadToCharConv
  | BadFromCharConv Char
  | NegativeTapeIndex Int
  deriving (Show, Typeable)

instance Exception BfError where
  displayException BadToCharConv = "failed converting cell to character"
  displayException (BadFromCharConv c) =
    "could not convert character " ++ show c ++ " to cell"
  displayException (NegativeTapeIndex i) =
    "tried to modify negative tape index " ++ show i

-- | Monad class for describing Brainfuck tape manipulations
--
-- Contains an infinite (single-directional) tape of cells storing type c, all
-- initialized to 0 and a tape head, starting at the left-most cell of the
-- tape.
--
-- On runtime errors, such as overflowing off the left side of the tape, a
-- BfError is thrown. Note that errors are thrown "lazily"; for example, a tape
-- overflow error may not be thrown as long as no reads or writes are
-- performed off the edge of the tape.
--
-- While the primitive Brainfuck instructions are supported, there are also
-- some operations which can operate on parts of the tape not under the head
-- for optimizing implementations to use.
class (BfCell c, Monad m) => MonadBfTape c m | m -> c where
  moveHeadLeft :: m ()
  moveHeadLeft = moveHead (-1)

  moveHeadRight :: m ()
  moveHeadRight = moveHead 1

  moveHead :: Int -> m ()
  moveHead n
    | n > 0 = replicateM_ n moveHeadRight
    | n < 0 = replicateM_ (- n) moveHeadLeft
    | otherwise = pure ()

  readCell :: m c
  readCell = readCellOffset 0

  readCellOffset :: Int -> m c
  readCellOffset n = do
    moveHead n
    v <- readCell
    moveHead (- n)
    pure v

  -- TODO Make writeCell in terms of modifyCell instead of the other way around?

  writeCell :: c -> m ()
  writeCell = writeCellOffset 0

  writeCellOffset :: Int -> c -> m ()
  writeCellOffset n v = do
    moveHead n
    writeCell v
    moveHead (- n)

  modifyCell :: (c -> c) -> m ()
  modifyCell = modifyCellOffset 0

  modifyCellOffset :: Int -> (c -> c) -> m ()
  modifyCellOffset n f = do
    v <- readCellOffset n
    writeCellOffset n (f v)
