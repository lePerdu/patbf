{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Pinky.Brainfuck.Machine
  ( BfCell (..),
    BrainfuckMachine (..),
    IOMachine (..),
    BufferMachine,
    runBufferMachine,
    execBufferMachine,
  )
where

import Control.Monad.State
import Control.Monad.ST
import Control.Monad.Primitive
import Data.Char
import Data.Maybe (fromMaybe, maybe)
import Data.Word
import Lens.Micro.Platform
import System.IO

-- | Type of cell which can be stored in a brainfuck machine
class (Integral t, Show t) => BfCell t

instance BfCell Word8

instance BfCell Word

-- | Type class for Brainfuck I/O operations.
--
-- This is parameterized by the cell type 'c', although most implementations
-- should work for any cell type. Instances are responsable for deciding whether
-- I/O is byte-oriented or character-oriented in some encoding, and converting
-- to and from the cell type.
class (Monad m, BfCell c) => BrainfuckMachine c m | m -> c where
  -- | Output a single cell.
  bfPutChar :: c -> m ()

  -- | Input into a cell.
  --
  -- Should return Nothing on EOF.
  bfGetChar :: m (Maybe c)

-- | BrainfuckMachine running in IO
--
-- Input is processed in Haskell Chars.
newtype IOMachine c a = IOMachine {runIOMachine :: IO a}
  deriving (Functor, Applicative, Monad)

instance BfCell c => BrainfuckMachine c (IOMachine c) where
  bfPutChar = IOMachine . putChar . chr . fromIntegral
  bfGetChar = IOMachine $ do
    eof <- isEOF
    if eof
      then return Nothing
      else fmap (Just . fromIntegral . ord) getChar

-- | BrainfuckMachine using pre-set buffers for input and output
--
-- Useful for testing Brainfuck execution in a pure context.
newtype BufferMachine c a = BufferMachine
  {_unBufferMachine :: State ([c], [c]) a}
  deriving (Functor, Applicative, Monad)

instance BfCell c => BrainfuckMachine c (BufferMachine c) where
  bfPutChar val = BufferMachine $ modifying _2 (++ [val])
  bfGetChar = BufferMachine $ do
    input <- use _1
    case input of
      c : rest -> do
        _1 .= rest
        return $ Just c
      [] -> return Nothing

-- Runs a BufferMachine with given input, returning computed value, unread
-- input, and the output
runBufferMachine ::
  BfCell c => BufferMachine c a -> [c] -> (a, ([c], [c]))
runBufferMachine machine input =
  runState (_unBufferMachine machine) (input, [])

-- Runs a BufferMachine with given input, returning unread input and the output.
execBufferMachine ::
  BfCell c => BufferMachine c () -> [c] -> ([c], [c])
execBufferMachine machine input =
  execState (_unBufferMachine machine) (input, [])
