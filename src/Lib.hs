module Lib
    ( processBFSimple
    , processBFCollapsed
    , processBFOpt
    , runBFMachine
    )
where

import           Data.Parser
import           Brainfuck.Parser
import           Brainfuck.Interpret
import           Brainfuck.Machine
import           Brainfuck.Optimizer

-- Helper for the other functions
processBFCode :: String -> Either String [BF]
processBFCode text = case runParser parseBF text of
    (Right code, []) -> Right code
    (Right _, rest) -> Left $ "expected end of input, found " ++ show rest
    (Left err, _) -> Left err

processBFSimple :: BFCell t => String -> Either String (BrainfuckM t ())
processBFSimple = fmap interpretBF . processBFCode

processBFCollapsed :: BFCell t => String -> Either String (BrainfuckM t ())
processBFCollapsed = fmap (interpretCollapsed . collapseBF) . processBFCode

processBFOpt :: BFCell t => String -> Either String (BrainfuckM t ())
processBFOpt = fmap (interpretOpt . optimizeBF) . processBFCode

