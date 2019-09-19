module Lib
    ( processBFSimple
    , processBFCollapsed
    , processBFOpt
    , runBFMachine
    )
where

import           Brainfuck.Parser
import           Brainfuck.Interpret
import           Brainfuck.Machine
import           Brainfuck.Optimizer

import           Data.Either.Combinators
import           Text.Megaparsec

-- Helper for the other functions
processBFCode :: String -> String -> Either String [BF]
processBFCode source text = mapLeft show $ runParser parseBF source text

processBFSimple
    :: BFCell t => String -> String -> Either String (BrainfuckM t ())
processBFSimple source text = fmap interpretBF (processBFCode source text)

processBFCollapsed
    :: BFCell t => String -> String -> Either String (BrainfuckM t ())
processBFCollapsed source text =
    fmap (interpretCollapsed . collapseBF) (processBFCode source text)

processBFOpt :: BFCell t => String -> String -> Either String (BrainfuckM t ())
processBFOpt source text =
    fmap (interpretOpt . optimizeBF) (processBFCode source text)

