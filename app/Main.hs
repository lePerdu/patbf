{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Data.Word
import           System.Environment
import           System.IO
import           System.Exit
import           System.IO.Error
import           Control.Exception

import           Control.Monad.Reader

import           Control.Monad                  ( when )

import           Options.Applicative

import           System.Console.Haskeline

import           Lib
import           Brainfuck.Machine

data OptLevel = None | Collapse | FullOpt deriving (Eq)

data CellSize = CellWord8 | CellWordDefault -- CellInf

data RunOptions = RunOptions
    { unbufferedInput :: Bool
    , optLevel :: OptLevel
    , cellSize :: CellSize
    , debug :: Bool
    }

data Options = Options { fileName :: Maybe String, runOptions :: RunOptions }

cellSizeReader :: ReadM CellSize
cellSizeReader = eitherReader $ \case
    "8"    -> Right CellWord8
    "word" -> Right CellWordDefault
    s      -> Left $ "Invalid cell size: " ++ s

runOptParse :: Parser RunOptions
runOptParse =
    RunOptions
        <$> switch
                (short 'u' <> long "unbuffered" <> help
                    "whether to unbuffer the input stream"
                )
        <*> flag
                None
                FullOpt
                (short 'o' <> long "optimize" <> help
                    "whether to optimize the brainfuck code"
                )
        <*> option
                cellSizeReader
                (  short 'c'
                <> long "cell"
                <> metavar "(8|word)"
                <> value CellWord8
                <> help "cell size to use"
                )
        <*> switch
                (short 'd' <> long "debug" <> help "whether to enable debugging"
                )

optparse :: Parser Options
optparse =
    Options
        <$> optional
                (argument
                    str
                    (  metavar "FILE"
                    <> help
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

getBFProcessor
    :: BFCell t
    => OptLevel
    -> String
    -> String
    -> Either String (BrainfuckM t ())
getBFProcessor level = case level of
    None     -> processBFSimple
    Collapse -> processBFCollapsed
    FullOpt  -> processBFOpt

getBFAction :: CellSize -> OptLevel -> String -> String -> Either String (IO ())
getBFAction size level source code = case size of
    CellWord8 ->
        let
            p =
                getBFProcessor level source code :: Either
                        String
                        (BrainfuckM Word8 ())
        in  fmap runBFMachine p
    CellWordDefault ->
        let
            p =
                getBFProcessor level source code :: Either
                        String
                        (BrainfuckM Word ())
        in  fmap runBFMachine p

runCode :: (MonadReader RunOptions m, MonadIO m) => String -> String -> m ()
runCode source code = do
    opt        <- view optLevel
    size       <- view cellSize
    unbuffered <- view unbufferedInput
    liftIO $ case getBFAction size opt source code of
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

repl :: (MonadReader RunOptions m, MonadIO m, MonadException m) => m ()
repl = runInputT defaultSettings loop  where
    loop = do
        input <- getInputLine "% "
        case input of
            Nothing   -> return ()
            Just code -> do
                lift $ runCode "" code
                loop

main :: IO ()
main = do
    options <- execParser optinfo
    let act = case fileName options of
            Just name -> runFile name
            Nothing   -> repl
    runReaderT act (runOptions options)

