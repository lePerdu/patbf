{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Pinky.Brainfuck.Tape.Vector where

import Control.Monad.ST
import Control.Monad.ST.Trans
import Control.Monad.State.Strict
import Control.Monad.Trans
import Data.Array.Base (MArray)
import Data.Array.ST
import Lens.Micro.Platform
import Pinky.Brainfuck.Machine
import Pinky.Brainfuck.Tape

data OptTapeState s c = OptTapeState
  { _optTapeHead :: !Int,
    _optTapeArray :: STArray s Int c
  }

makeLenses ''OptTapeState

newtype BfOptTape c m a
  = BfOptTape (forall s. StateT (OptTapeState s c) (STT s m) a)

instance (Functor m) => Functor (BfOptTape c m) where
  fmap f (BfOptTape a) = BfOptTape $ fmap f a

instance (Monad m) => Applicative (BfOptTape c m) where
  pure a = BfOptTape $ pure a
  BfOptTape f <*> BfOptTape a = BfOptTape (f <*> a)

instance (Monad m) => Monad (BfOptTape c m) where
  (BfOptTape s) >>= f =
    BfOptTape $ s >>= \a -> let BfOptTape s' = f a in s'

instance MonadTrans (BfOptTape c) where
  lift ma = BfOptTape $ lift $ lift ma

instance (BfCell c, BrainfuckMachine c m) => BrainfuckMachine c (BfOptTape c m) where
  bfPutChar = lift . bfPutChar
  bfGetChar = lift bfGetChar

initTapeLength :: Int
initTapeLength = 30000

initTape :: (BfCell c, Monad m) => STT s m (OptTapeState s c)
initTape = OptTapeState 0 <$> newArray (0, initTapeLength) 0

instance (BfCell c, Monad m) => MonadBfTape c (BfOptTape c m) where
  moveHead n = BfOptTape $ modifying optTapeHead (+ n)

  readCellOffset n = BfOptTape $ do
    OptTapeState head tape <- get
    lift $ readArray tape (head + n)

  writeCellOffset n v = BfOptTape $ do
    OptTapeState head tape <- get
    lift $ writeArray tape (head + n) v

evalBfOptTape :: (BfCell c, Monad m) => BfOptTape c m a -> m a
evalBfOptTape (BfOptTape s) = runSTT $ do
  tape <- initTape
  evalStateT s tape
