{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Commands
import Control.Exception
import Control.Monad
import Control.Monad.Catch (MonadMask)
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Either.Combinators
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Word
import Lens.Micro.Platform
import Options.Applicative
import Pinky
import Pinky.Brainfuck
import System.Console.Haskeline
import System.IO
import Text.Megaparsec (runParser)
import Types

cellSizeReader :: ReadM CellSize
cellSizeReader =
  -- TODO Find out how to print the error nicely
  eitherReader (mapLeft show . runParser cellSizeParser "argument" . T.pack)

runOptParse :: Parser RunOptions
runOptParse =
  RunOptions
    <$> switch
      ( short 'u' <> long "unbuffered"
          <> help
            "whether to unbuffer the input stream"
      )
    <*> option
      cellSizeReader
      ( short 'c'
          <> long "cell"
          <> metavar "(8|word)"
          <> value CellWord8
          <> help "cell size to use"
      )
    <*> ( BfOptions
            <$> flag
              NoOptimize
              FullOptimize
              ( short 'o' <> long "optimize"
                  <> help
                    "whether to optimize the brainfuck code"
              )
        )

optparse :: Parser Options
optparse =
  Options
    <$> optional
      ( argument
          str
          ( metavar "FILE"
              <> help
                ( "file to execute; "
                    ++ "if no file is given, this runs an interactive REPL"
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
withUnbuffering unbuf act =
  if unbuf
    then do
      orig <- hGetBuffering stdin
      hSetBuffering stdin NoBuffering
      res <- act
      hSetBuffering stdin orig
      return res
    else act

-- TODO Is there a better way to do this (i.e. a way that duplicates less code)?
-- This is weird because it has to choose the type at runtime.
getBfAction :: CellSize -> BfOptions -> Bf -> IO ()
getBfAction size options code = case size of
  CellWord8 ->
    runIOMachine (interpretBf options code :: IOMachine Word8 ())
  CellWordDefault ->
    runIOMachine (interpretBf options code :: IOMachine Word ())

runCode :: (MonadReader RunOptions m, MonadIO m) => String -> T.Text -> m ()
runCode source code = do
  opts <- view interpOptions
  size <- view cellSize
  unbuffered <- view unbufferedInput
  liftIO $ case fmap (getBfAction size opts) (runBfParser source code) of
    Right action -> flip catches handlers $ do
      withUnbuffering unbuffered action
      -- Clear stdin so that unread keys from the execution don't
      -- remain in the input buffer and come out after the program
      -- is done
      clearStdin
    Left e -> putStr "[Error] " >> putStrLn e
  where
    handleIOException :: IOException -> IO ()
    handleIOException e = putStr "[IO Error] " >> putStrLn (displayException e)

    handleBfException :: BfError -> IO ()
    handleBfException e =
      putStr "[Execution Error] " >> putStrLn (displayException e)

    handleOther :: SomeException -> IO ()
    handleOther e = putStr "[Unknown Error] " >> putStrLn (displayException e)

    handlers =
      [ Handler handleIOException,
        Handler handleBfException,
        Handler handleOther
      ]

runFile :: (MonadReader RunOptions m, MonadIO m) => FilePath -> m ()
runFile path = liftIO (TIO.readFile path) >>= runCode path

runStdInput :: (MonadReader RunOptions m, MonadIO m) => m ()
runStdInput = liftIO TIO.getContents >>= runCode "stdin"

runRepl :: (MonadReader RunOptions m, MonadIO m, MonadMask m) => m ()
runRepl = computeInitState >>= evalStateT (runInputT defaultSettings loop)
  where
    computeInitState = asks $ ReplState "% "

    runReplCommand (SetPrompt p) = replPrompt .= p
    runReplCommand (SetUnbuffered b) = replRunOpts . unbufferedInput .= b
    runReplCommand (SetOptLevel o) =
      replRunOpts . interpOptions . bfOptLevel .= o
    runReplCommand (SetCellSize c) = replRunOpts . cellSize .= c
    runReplCommand ReplHelp = liftIO $ TIO.putStrLn commandHelp

    processInput input = case tryParseCommand input of
      Right (Just cmd) -> lift $ runReplCommand cmd
      Left err -> outputStrLn (show err)
      Right Nothing -> do
        -- Not a command, so run as Brainfuck
        stateOpts <- lift $ use replRunOpts
        lift $ local (const stateOpts) $ runCode "" input

    loop = do
      prompt <- lift $ use replPrompt
      input <- getInputLine prompt
      case input of
        Just input -> processInput (T.pack input) >> loop
        Nothing -> return () -- Just exit

main :: IO ()
main = do
  Options {_fileName = fname, _runOptions = opts} <- execParser optinfo
  let act = case fname of
        Just name -> runFile name
        Nothing -> do
          isTerm <- liftIO $ hIsTerminalDevice stdin
          if isTerm then runRepl else runStdInput
  runReaderT act opts
