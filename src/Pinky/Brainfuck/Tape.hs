{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Pinky.Brainfuck.Tape
  ( BfState (..),
    BrainfuckTape,
    BfCell (..),
    BfError (..),
    truncateTape,
    moveHeadLeft,
    moveHeadRight,
    moveHead,
    readCell,
    readCellOffset,
    modifyCell,
    modifyCellOffset,
    setCell,
    setCellOffset,
    runBfTape,
    evalBfTape,
  )
where

import Control.Exception
import Control.Monad.State.Strict
import Data.Char
import Data.Functor
import Data.Word
import Type.Reflection
import Pinky.Brainfuck.Machine

data BfState t = BfState
  { _bfTapeLeft :: ![t],
    _bfHead :: !t,
    _bfTapeRight :: ![t]
  }
  deriving (Show, Eq)

initTape :: BfCell t => BfState t
initTape = BfState [] 0 (repeat 0)

-- | Trim the right side of the tape.
truncateTape :: Int -> BfState c -> BfState c
truncateTape size (BfState l h r) = BfState l h (take size r)

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

-- | Monad for describing Brainfuck tape manipulations
--
-- Contains an infinite type (single-directional) of cells storing type t, all
-- initialized to 0 and a tape head, starting at the left-most cell of the
-- tape. There is some minimal debugging capability which debugs the current
-- head and tape contents.
--
-- On runtime errors, such as overflowing off the left side of the tape, a
-- BfError is thrown. Note that errors are thrown "lazily"; for example, a tape
-- overflow error will not be thrown as long as no reads or writes are
-- performed off the edge of the tape.
--
-- While the primitive Brainfuck instructions are supported, there are also
-- some operations which can operate on parts of the tape not under the head
-- for optimizing implementations to use.
newtype BrainfuckTape t m a = BrainfuckTape
  { unBfTape :: StateT (BfState t) m a
  }
  deriving (Functor, Applicative, Monad)

instance MonadTrans (BrainfuckTape t) where
  lift = BrainfuckTape . lift

moveHeadLeft :: Monad m => BrainfuckTape t m ()
moveHeadLeft = BrainfuckTape $ do
  BfState l h r <- get
  put $ BfState (tail l) (head l) (h : r)

moveHeadRight :: Monad m => BrainfuckTape t m ()
moveHeadRight = BrainfuckTape $ do
  BfState l h r <- get
  put $ BfState (h : l) (head r) (tail r)

moveHead :: Monad m => Int -> BrainfuckTape t m ()
moveHead n
  | n > 0 = replicateM_ n moveHeadRight
  | n < 0 = replicateM_ (- n) moveHeadLeft
  | otherwise = pure ()

readCell :: (Monad m, BfCell t) => BrainfuckTape t m t
readCell = BrainfuckTape $ gets _bfHead

readCellOffset :: (Monad m, BfCell t) => Int -> BrainfuckTape t m t
readCellOffset offset = do
  moveHead offset
  cell <- readCell
  moveHead (- offset)
  return cell

modifyCell :: (Monad m, BfCell t) => (t -> t) -> BrainfuckTape t m ()
modifyCell f = BrainfuckTape $ do
  tape <- get
  put $ tape {_bfHead = f (_bfHead tape)}

modifyCellOffset ::
  (Monad m, BfCell t) => Int -> (t -> t) -> BrainfuckTape t m ()
modifyCellOffset offset f = do
  moveHead offset
  modifyCell f
  moveHead (- offset)

setCell :: (Monad m, BfCell t) => t -> BrainfuckTape t m ()
setCell = modifyCell . const

setCellOffset :: (Monad m, BfCell t) => Int -> t -> BrainfuckTape t m ()
setCellOffset off = modifyCellOffset off . const

runBfTape :: (Monad m, BfCell t) => BrainfuckTape t m a -> m (a, BfState t)
runBfTape bf = runStateT (unBfTape bf) initTape

evalBfTape :: (Monad m, BfCell t) => BrainfuckTape t m a -> m a
evalBfTape = fmap fst . runBfTape

{-}
debugBf :: (Monad m, BfCell t) => BrainfuckTape t IO ()
debugBf = do
  BfState tape pos <- getState
  let len = M.length tape
  liftIO $ do
    putStrLn ""
    putStrLn $ "head = " ++ show pos
    forM_
      [0 .. len - 1]
      ( \i -> do
          val <- M.read tape i
          putStr $ show val ++ ","
      )
    putStrLn ""
    -}
