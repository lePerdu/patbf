module Language.Brainfuck.Interpreter
    ( interpretBasic
    , interpretCollapsed
    , interpretOpt
    )
where

import qualified Data.IntMap.Strict            as M

import           Language.Brainfuck.Parser
import           Language.Brainfuck.Machine
import           Language.Brainfuck.Optimizer
import           Control.Monad.IO.Class
import           Control.Monad

convertEnum :: (Enum a, Enum b) => a -> b
convertEnum = toEnum . fromEnum

interpretBasic :: BFCell t => [BF] -> BrainfuckM t ()
interpretBasic = mapM_ interpInstr  where
    interpInstr Increment   = modifyCell (+ 1)
    interpInstr Decrement   = modifyCell (\n -> n - 1)
    interpInstr MoveLeft    = moveHead (-1)
    interpInstr MoveRight   = moveHead 1
    interpInstr Input       = liftIO getChar >>= setCell . convertEnum
    interpInstr Output      = readCell >>= liftIO . putChar . convertEnum
    interpInstr (Loop code) = do
        cell <- readCell
        unless (cell == 0) (interpretBasic code >> interpInstr (Loop code))
    interpInstr Debug = debugBF

interpretCollapsed :: BFCell t => [CollapsedBF t] -> BrainfuckM t ()
interpretCollapsed = mapM_ interpInstr  where
    interpInstr (Incr n)          = modifyCell (+ n)
    interpInstr (Move n)          = moveHead n
    interpInstr CInput            = liftIO getChar >>= setCell . convertEnum
    interpInstr COutput           = readCell >>= liftIO . putChar . convertEnum
    interpInstr loop@(CLoop code) = do
        cell <- readCell
        unless (cell == 0) (interpretCollapsed code >> interpInstr loop)
    interpInstr CDebug = debugBF

interpretOpt :: BFCell t => [OptBF t] -> BrainfuckM t ()
interpretOpt = mapM_ interpInstr  where
    interpInstr (OIncr n)         = modifyCell (+ n)
    interpInstr (OMove n)         = moveHead n
    interpInstr OInput            = liftIO getChar >>= setCell . convertEnum
    interpInstr OOutput           = readCell >>= liftIO . putChar . convertEnum
    interpInstr loop@(OLoop code) = do
        cell <- readCell
        unless (cell == 0) (interpretOpt code >> interpInstr loop)
    interpInstr SetZero = setCell 0
    interpInstr (LinearLoop (LinearIncr mults loopIncr)) = do
        initCell <- readCell
        case solveWithWrap initCell loopIncr of
            Just solution -> do
                setCell 0
                forM_ (M.toList mults) $ \(offset, inc) ->
                    modifyCellOffset (+ (inc * solution)) offset
            Nothing ->
                -- If there is no solution, the loop will not terminate
                fail "infinite loop detected"
    interpInstr ODebug = debugBF
