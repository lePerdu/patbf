module Commands
    ( ReplCommand(..)
    , optLevelParser
    , cellSizeParser
    , commandParser
    , possibleCommandParser
    , tryParseCommand
    )
where

import           Data.Char
import           Data.Functor
import           Data.Void
import           Data.Either

import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Pinky
import           Types

data ReplCommand
    = SetPrompt String
    | SetUnbuffered Bool
    | SetOptLevel OptimizeLevel
    | SetCellSize CellSize

-- TODO Use Text?
type Parser = Parsec Void String

type Error = ParseErrorBundle String Void

stringMap :: [(String, a)] -> Parser a
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
optLevelParser = stringMap
    [("none", NoOptimize), ("simple", SimpleOptimize), ("full", FullOptimize)]

cellSizeParser :: Parser CellSize
cellSizeParser =
    stringMap [("8", CellWord8), ("byte", CellWord8), ("word", CellWordDefault)]

-- | Command name followed by either whitespace or EOF
nameParser :: Parser String
nameParser =
    takeWhile1P (Just "command name") isAlpha
        <* (void (takeWhile1P (Just "command separator") isSpace) <|> eof)

commandParser :: Parser ReplCommand
commandParser = do
    cmd <- nameParser
    case cmd of
        "prompt"   -> fmap SetPrompt takeRest
        "buffer"   -> eof $> SetUnbuffered False
        "unbuffer" -> eof $> SetUnbuffered True
        "opt"      -> fmap SetOptLevel (endArgument optLevelParser)
        "cell"     -> fmap SetCellSize (endArgument cellSizeParser)
        _          -> fail $ "Unknown command: " ++ cmd

possibleCommandParser :: Parser (Maybe ReplCommand)
possibleCommandParser = do
    -- If the first character is ':', run the command parser (and fail if that
    -- parser fails). Otherwise just return Nothing
    isCmd <- fmap isRight (observing (char ':'))
    if isCmd then fmap Just commandParser else return Nothing

tryParseCommand :: String -> Either Error (Maybe ReplCommand)
tryParseCommand = runParser possibleCommandParser ""

