{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | High-level wrappers functions for common use cases.
module Pinky
  ( OptimizeLevel (..),
    BfOptions (..),
    Bf (..),
    bfOptLevel,
    runBfParser,
    interpretBf,
  )
where

import Data.Either.Combinators
import Data.Text (Text, pack)
import Data.Word
import Lens.Micro.Platform
import Pinky.Brainfuck.Language
import Pinky.Brainfuck.Machine
import Pinky.Brainfuck.Naive
import Pinky.Brainfuck.Optimizer
import Pinky.Brainfuck.Parser
import Text.Megaparsec

-- | Possible optimization levels
--
-- NoOptimize runs the original syntax tree directly.
--
-- FullOptimize does the "simple" optimizations and also removes certain types
-- of loops.
--
-- See Pinky.Brainfuck.Optimizer for more details
data OptimizeLevel = NoOptimize | FullOptimize deriving (Eq)

-- | A record of options for running Brainfuck code
newtype BfOptions = BfOptions {_bfOptLevel :: OptimizeLevel} deriving (Eq)

makeLenses ''BfOptions

-- | Parse Brainfuck code into the basic syntax tree.
runBfParser ::
  -- | Name of the source file (for error messages)
  String ->
  -- | Brainfuck code to parse
  Text ->
  -- | Parsed code or an error message
  Either String Bf
runBfParser source text =
  mapLeft errorBundlePretty $ runParser parseBf source text

-- | Convert brainfuck code into a BrainfuckMachine action.
--
-- This is simply a dispatcher for various interpreters.
interpretBf ::
  (BfOptCell c, BrainfuckMachine c m) =>
  -- | Options for interpreting the Brainfuck code
  BfOptions ->
  -- | Code to interpret
  Bf ->
  -- | BrainfuckM action
  m ()
interpretBf options = case _bfOptLevel options of
  NoOptimize -> interpretBasic
  FullOptimize -> interpretOptimized
