{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
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
import Data.Char
import Data.Maybe (fromMaybe, maybe)
import Data.Word
import Lens.Micro.Platform
import System.IO

-- | Type of cell which can be stored in a brainfuck machine
class (Integral t, Show t) => BfCell t where
  -- TODO Remove these unsafe default implementations?

  toChar :: t -> Maybe Char
  toChar = Just . chr . fromIntegral

  fromChar :: Char -> Maybe t
  fromChar = Just . fromIntegral . ord

instance BfCell Word8 where
  fromChar c =
    let int = ord c
     in if int <= fromIntegral (maxBound :: Word8)
          then Just (fromIntegral int)
          else Nothing

instance BfCell Word where
  toChar n =
    let maxChar = fromIntegral (fromEnum (maxBound :: Char)) :: Word
     in if n <= maxChar then Just (chr (fromIntegral n)) else Nothing

-- | Brainfuck execution environment
--
-- Describes the semantics of the execution (i.e. cell size and wrapping) and
-- I/O.
--
-- TODO Rename?
class Monad m => BrainfuckMachine m where
  type MachineCell m

  -- | Output a single character
  bfPutChar :: MachineCell m -> m ()

  -- | Input a single character
  --
  -- This may return Nothing on end of input.
  bfGetChar :: m (Maybe (MachineCell m))

-- BrainfuckMachine running in IO
newtype IOMachine c a = IOMachine {runIOMachine :: IO a}
  deriving (Functor, Applicative, Monad)

instance BfCell c => BrainfuckMachine (IOMachine c) where
  type MachineCell (IOMachine c) = c

  bfPutChar = IOMachine . maybe (pure ()) putChar . toChar
  bfGetChar = IOMachine $ do
    eof <- isEOF
    if eof
      then return $ Just 0
      else fromChar <$> getChar

-- BrainfuckMachine using Strings buffers for input and output
--
-- Useful for testing Brainfuck execution in a pure context.
--
-- TODO Use Text or ShowS for more efficient appending to output
newtype BufferMachine c a = BufferMachine
  {_unBufferMachine :: State (String, String) a}
  deriving (Functor, Applicative, Monad)

instance BfCell c => BrainfuckMachine (BufferMachine c) where
  type MachineCell (BufferMachine c) = c

  -- TODO Decide/define behavior when char conversion fails
  bfPutChar val =
    let s = maybe "" (: "") (toChar val)
     in BufferMachine $ modifying _2 (++ s)
  bfGetChar = BufferMachine $ do
    input <- use _1
    case input of
      c : rest -> do
        _1 .= rest
        return $ fromChar c
      [] -> return $ Just 0

-- Runs a BufferMachine with given input, returning computed value, output,
-- and, unread input.
runBufferMachine ::
  BfCell c => BufferMachine c a -> String -> (a, (String, String))
runBufferMachine machine input =
  runState (_unBufferMachine machine) (input, "")

-- Runs a BufferMachine with given input, returning output and unread input.
execBufferMachine ::
  BfCell c => BufferMachine c () -> String -> (String, String)
execBufferMachine machine input =
  execState (_unBufferMachine machine) (input, "")
