module Language.Brainfuck.Interpreter
    ( interpretBasic
    , interpretCollapsed
    , interpretOpt
    )
where

import qualified Data.IntMap.Strict            as M
import           Data.Maybe
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class

import           Language.Brainfuck.Machine
import           Language.Brainfuck.Optimizer
import           Language.Brainfuck.Parser

toCharThrow :: BFCell t => t -> Char
toCharThrow n = fromMaybe (throw BadToCharConv) (toChar n)

fromCharThrow :: BFCell t => Char -> t
fromCharThrow c = fromMaybe (throw (BadFromCharConv c)) (fromChar c)

interpretBasic :: BFCell t => [BF] -> BrainfuckM t ()
interpretBasic = mapM_ interpInstr  where
    interpInstr Increment   = modifyCell (+ 1)
    interpInstr Decrement   = modifyCell (\n -> n - 1)
    interpInstr MoveLeft    = moveHead (-1)
    interpInstr MoveRight   = moveHead 1
    interpInstr Input       = liftIO getChar >>= setCell . fromCharThrow
    interpInstr Output      = readCell >>= liftIO . putChar . toCharThrow
    interpInstr (Loop code) = do
        cell <- readCell
        unless (cell == 0) (interpretBasic code >> interpInstr (Loop code))
    interpInstr Debug = debugBF

interpretCollapsed :: BFCell t => [CollapsedBF t] -> BrainfuckM t ()
interpretCollapsed = mapM_ interpInstr  where
    interpInstr (Incr n)          = modifyCell (+ n)
    interpInstr (Move n)          = moveHead n
    interpInstr CInput            = liftIO getChar >>= setCell . fromCharThrow
    interpInstr COutput           = readCell >>= liftIO . putChar . toCharThrow
    interpInstr loop@(CLoop code) = do
        cell <- readCell
        unless (cell == 0) (interpretCollapsed code >> interpInstr loop)
    interpInstr CDebug = debugBF

interpretOpt :: BFCell t => [OptBF t] -> BrainfuckM t ()
interpretOpt = mapM_ interpInstr  where
    interpInstr (OIncr n)         = modifyCell (+ n)
    interpInstr (OMove n)         = moveHead n
    interpInstr OInput            = liftIO getChar >>= setCell . fromCharThrow
    interpInstr OOutput           = readCell >>= liftIO . putChar . toCharThrow
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
