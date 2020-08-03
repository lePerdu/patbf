{-# LANGUAGE OverloadedStrings #-}

module Commands
  ( ReplCommand (..),
    optLevelParser,
    cellSizeParser,
    commandParser,
    possibleCommandParser,
    tryParseCommand,
    commandHelp,
  )
where

import Data.Char
import Data.Either
import Data.Functor
import Data.Void
import qualified Data.Text as T
import Pinky
import Text.Megaparsec
import Text.Megaparsec.Char
import Types

data ReplCommand
  = SetPrompt String
  | SetUnbuffered Bool
  | SetOptLevel OptimizeLevel
  | SetCellSize CellSize
  | ReplHelp

type Parser = Parsec Void T.Text

type Error = ParseErrorBundle T.Text Void

stringMap :: [(T.Text, a)] -> Parser a
stringMap = choice . map (\(s, o) -> string s $> o)

-- Parses a whole identifier, optionally forcing it to be the last part of the
-- command.
argument :: Parser a -> Parser a
argument p = p <* takeWhileP Nothing isSpace

endArgument :: Parser a -> Parser a
endArgument p = argument p <* eof

-- TODO Read full words and then test against the known values to get better
-- error messages ("unknown value" instead of "expected EOF").

optLevelParser :: Parser OptimizeLevel
optLevelParser =
  stringMap
    [("none", NoOptimize), ("full", FullOptimize)]

cellSizeParser :: Parser CellSize
cellSizeParser =
  stringMap [("8", CellWord8), ("byte", CellWord8), ("word", CellWordDefault)]

-- | Command name followed by either whitespace or EOF
nameParser :: Parser T.Text
nameParser =
  takeWhile1P (Just "command name") isAlpha
    <* (void (takeWhile1P (Just "command separator") isSpace) <|> eof)

commandParser :: Parser ReplCommand
commandParser = do
  cmd <- nameParser
  case cmd of
    "prompt" -> SetPrompt . T.unpack <$> takeRest
    "buffer" -> eof $> SetUnbuffered False
    "unbuffer" -> eof $> SetUnbuffered True
    "opt" -> fmap SetOptLevel (endArgument optLevelParser)
    "cell" -> fmap SetCellSize (endArgument cellSizeParser)
    "help" -> pure ReplHelp
    _ -> fail $ "Unknown command: " ++ T.unpack cmd

possibleCommandParser :: Parser (Maybe ReplCommand)
possibleCommandParser = do
  -- If the first character is ':', run the command parser (and fail if that
  -- parser fails). Otherwise just return Nothing
  isCmd <- fmap isRight (observing (char ':'))
  if isCmd then fmap Just commandParser else return Nothing

tryParseCommand :: T.Text -> Either Error (Maybe ReplCommand)
tryParseCommand = runParser possibleCommandParser ""

-- TODO Generate this instead of hard-coding it
commandHelp :: T.Text
commandHelp =
  "instructions:\n\
  \prompt <prompt>\t\tSet prompt\n\
  \buffer\t\t\tSet buffered mode\n\
  \unbuffer\t\tSet non-buffered mode\n\
  \opt <level>\t\tSet optimization level (none|full)\n\
  \cell <cell-type>\tSet cell type (8|byte|word)\n\
  \help\t\t\tShow this help message\n"
