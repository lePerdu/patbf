{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Data.Word
import           Data.Maybe
import           Data.Either.Combinators
import           System.Environment
import           System.IO
import           System.Exit
import           System.IO.Error
import           Control.Exception

import           Control.Monad.Reader
import           Control.Monad.State.Strict

import           Control.Monad

import           Options.Applicative

import           System.Console.Haskeline
import           Lens.Micro.Platform
import           Text.Megaparsec                ( runParser )

import           Pinky
import           Types
import           Commands

cellSizeReader :: ReadM CellSize
cellSizeReader =
    -- TODO Find out how to print the error nicely
    eitherReader (mapLeft show . runParser cellSizeParser "argument")

runOptParse :: Parser RunOptions
runOptParse =
    RunOptions
        <$> switch
                (short 'u' <> long "unbuffered" <> help
                    "whether to unbuffer the input stream"
                )
        <*> option
                cellSizeReader
                (  short 'c'
                <> long "cell"
                <> metavar "(8|word)"
                <> value CellWord8
                <> help "cell size to use"
                )
        <*> (BFOptions <$> flag
                NoOptimize
                FullOptimize
                (short 'o' <> long "optimize" <> help
                    "whether to optimize the brainfuck code"
                )
            )

optparse :: Parser Options
optparse =
    Options
        <$> optional
                (argument
                    str
                    (metavar "FILE" <> help
                        (  "file to execute; "
                        ++ "if no file is given, this runs an interactive "
                        ++ "REPL"
                        )
                    )
                )
        <*> runOptParse

optinfo :: ParserInfo Options
optinfo =
    info (optparse <**> helper) (fullDesc <> progDesc "Run brainfuck code")

clearStdin :: IO ()
clearStdin = do
    ready <- hReady stdin
    when ready (getChar >> clearStdin)

withUnbuffering :: Bool -> IO a -> IO a
withUnbuffering unbuf act = if unbuf
    then do
        orig <- hGetBuffering stdin
        res  <- act
        hSetBuffering stdin orig
        return res
    else act

-- TODO Is there a better way to do this (i.e. a way that duplicates less code)?
-- This is weird because it has to choose the type at runtime.
getBFAction :: CellSize -> BFOptions -> [BF] -> IO ()
getBFAction size options code = case size of
    CellWord8 -> runBFMachine (interpretBF options code :: BrainfuckM Word8 ())
    CellWordDefault ->
        runBFMachine (interpretBF options code :: BrainfuckM Word ())

runCode :: (MonadReader RunOptions m, MonadIO m) => String -> String -> m ()
runCode source code = do
    opts       <- view interpOptions
    size       <- view cellSize
    unbuffered <- view unbufferedInput
    liftIO $ case fmap (getBFAction size opts) (runBFParser source code) of
        Right action -> do
            putStrLn "[Running]"
            -- let _ = bf :: BrainfuckM Word8 ()
            withUnbuffering unbuffered (catchIOError action exit)
            -- Clear stdin so that unread keys from the execution don't remain
            -- in the input buffer and come out after the program if done
            clearStdin
            putStrLn "[Done]"
        Left e -> putStr "[Error] " >> putStrLn e

runFile :: (MonadReader RunOptions m, MonadIO m) => FilePath -> m ()
runFile path = do
    code <- liftIO $ readFile path
    runCode path code

exit :: Exception e => e -> IO a
exit e = putStrLn (displayException e) >> exitFailure

runRepl :: (MonadReader RunOptions m, MonadIO m, MonadException m) => m ()
runRepl = computeInitState >>= evalStateT (runInputT defaultSettings loop)
  where
    computeInitState = ReplState <$> pure "% " <*> ask

    runReplCommand (SetPrompt p) = lift $ replPrompt .= p
    runReplCommand (SetUnbuffered b) =
        lift $ replRunOpts . unbufferedInput .= b
    runReplCommand (SetOptLevel o) =
        lift $ replRunOpts . interpOptions . bfOptLevel .= o
    runReplCommand (SetCellSize c) = lift $ replRunOpts . cellSize .= c

    processInput input = case tryParseCommand input of
        Right (Just cmd) -> runReplCommand cmd
        Left  err        -> outputStrLn (show err)
        Right Nothing    -> do
            -- Not a command, so run as Brainfuck
            stateOpts <- lift $ use replRunOpts
            lift $ local (const stateOpts) $ runCode "" input

    loop = do
        prompt <- lift $ use replPrompt
        input  <- getInputLine prompt
        maybe (pure ()) processInput input
        loop

main :: IO ()
main = do
    Options { _fileName = fname, _runOptions = opts } <- execParser optinfo
    let act = case fname of
            Just name -> runFile name
            Nothing   -> runRepl
    runReaderT act opts

