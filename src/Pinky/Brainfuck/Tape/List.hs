{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Pinky.Brainfuck.Tape.List
  ( BfTapeState (..),
    BfListTape,
    truncateTape,
    runBfListTape,
    evalBfListTape,
  )
where

import Control.Monad.State.Strict
import Pinky.Brainfuck.Machine
import Pinky.Brainfuck.Tape

data BfTapeState c = BfTapeState
  { _bfTapeLeft :: ![c],
    _bfHead :: !c,
    _bfTapeRight :: ![c]
  }
  deriving (Show, Eq)

initTape :: BfCell c => BfTapeState c
initTape = BfTapeState [] 0 (repeat 0)

-- | Trim the right side of the tape.
truncateTape :: Int -> BfTapeState c -> BfTapeState c
truncateTape size (BfTapeState l h r) = BfTapeState l h (take size r)

newtype BfListTape c m a = BfListTape
  { unBfTape :: StateT (BfTapeState c) m a
  }
  deriving (Functor, Applicative, Monad)

instance MonadTrans (BfListTape c) where
  lift = BfListTape . lift

instance (BfCell c, BrainfuckMachine c m) => BrainfuckMachine c (BfListTape c m) where
  bfPutChar = lift . bfPutChar
  bfGetChar = lift bfGetChar

instance (BfCell c, Monad m) => MonadBfTape c (BfListTape c m) where
  moveHeadLeft = BfListTape $ do
    BfTapeState l h r <- get
    put $ BfTapeState (tail l) (head l) (h : r)

  moveHeadRight = BfListTape $ do
    BfTapeState l h r <- get
    put $ BfTapeState (h : l) (head r) (tail r)

  readCell = BfListTape $ gets _bfHead

  modifyCell f = BfListTape $ do
    tape <- get
    put $ tape {_bfHead = f (_bfHead tape)}

  writeCell = modifyCell . const

runBfListTape :: (BfCell c, Monad m) => BfListTape c m a -> m (a, BfTapeState c)
runBfListTape bf = runStateT (unBfTape bf) initTape

evalBfListTape :: (BfCell c, Monad m) => BfListTape c m a -> m a
evalBfListTape = fmap fst . runBfListTape
