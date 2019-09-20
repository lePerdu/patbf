module Language.Brainfuck.Parser
    ( BF(..)
    , parseBF
    )
where

import           Data.Functor
import           Data.Void

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as Lex

data BF
    = Increment
    | Decrement
    | MoveLeft
    | MoveRight
    | Loop [BF]
    | Input
    | Output
    | Debug
    deriving (Show, Eq)

-- TODO Use Text instead of String?
type Parser = Parsec Void String

command :: Parser BF
command = choice $ zipWith
    (\c bf -> char c $> bf)
    "+-<>,.#"
    [Increment, Decrement, MoveLeft, MoveRight, Input, Output, Debug]

loop :: Parser BF
loop = Loop <$> between (char '[') (char ']') parseBFInstrs

comment :: Parser ()
comment = void $ takeWhileP (Just "comment") (`notElem` "+-<>[].,#")

parseInstr :: Parser BF
parseInstr = command <|> loop

-- TODO Should this require matching eof?
parseBFInstrs :: Parser [BF]
parseBFInstrs = comment *> many (Lex.lexeme comment parseInstr)

parseBF :: Parser [BF]
parseBF = parseBFInstrs <* eof

