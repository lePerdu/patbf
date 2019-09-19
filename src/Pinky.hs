{-# LANGUAGE TemplateHaskell #-}

-- | High-level wrappers functions for common use cases.
module Pinky
    ( OptimizeLevel(..)
    , BFOptions(..)
    , BF
    , bfOptLevel
    , runBFParser
    , interpretBF
    , parseInterpretBF
    , module Language.Brainfuck.Machine
    )
where

import           Data.Either.Combinators

import           Lens.Micro.Platform

import           Text.Megaparsec

import           Language.Brainfuck.Interpreter
import           Language.Brainfuck.Machine
import           Language.Brainfuck.Optimizer
import           Language.Brainfuck.Parser

-- | Possible optimization levels
--
-- NoOptimize runs the original syntax tree directly.
--
-- SimpleOptimize collapses sequences of increments and shifts, but does not
-- change control flow.
--
-- FullOptimize does the "simple" optimizations and also removes certain types
-- of loops.
--
-- See Language.Brainfuck.Optimizer for more details
data OptimizeLevel = NoOptimize | SimpleOptimize | FullOptimize deriving (Eq)

-- | A record of options for running Brainfuck code
newtype BFOptions = BFOptions { _bfOptLevel :: OptimizeLevel } deriving (Eq)

makeLenses ''BFOptions

-- | Parse Brainfuck code into the basic syntax tree.
runBFParser
    :: String -- ^ Name of the source file (for error messages)
    -> String -- ^ Brainfuck code to parse
    -> Either String [BF] -- ^ Parsed code or an error message
runBFParser source text = mapLeft show $ runParser parseBF source text

-- | Convert brainfuck code into a BrainfuckM action.
--
-- This is simply a dispatcher for various functions in
-- Language.Brainfuck.Interpreter and Language.Brainfuck.Optimizer based on the
-- options passed.
interpretBF
    :: BFCell t
    => BFOptions -- ^ Options for interpreting the Brainfuck code
    -> [BF] -- ^ Code to interpret
    -> BrainfuckM t () -- ^ BrainfuckM action
interpretBF options = case _bfOptLevel options of
    NoOptimize     -> interpretBasic
    SimpleOptimize -> interpretCollapsed . collapseBF
    FullOptimize   -> interpretOpt . optimizeBF

-- | Parse and interpret Brainfuck code.
--
-- This combines interpretBF and runBFParser into a single step.
parseInterpretBF
    :: BFCell t
    => BFOptions -- ^ Options for interpreting the Brainfuck code
    -> String -- ^ Name of the source file (for error messages)
    -> String -- ^ Brainfuck code to parse
    -> Either String (BrainfuckM t ()) -- ^ Brainfuck action or an error message
parseInterpretBF options source text =
    fmap (interpretBF options) (runBFParser source text)

