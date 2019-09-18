module Brainfuck.Parser
    ( BF(..)
    , parseBF
    )
where

import           Data.Functor
import           Control.Applicative.Combinators

import           Data.Parser

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

command :: Parser Char BF
command = choice $ zipWith
    (\c bf -> token c $> bf)
    "+-<>,.#"
    [Increment, Decrement, MoveLeft, MoveRight, Input, Output, Debug]

loop :: Parser Char BF
loop = Loop <$> between (token '[') (token ']') parseBF

comment :: Parser Char ()
comment = void $ many (satisfy (`notElem` "+-<>[].,#"))

parseBFInstr :: Parser Char BF
parseBFInstr = command <|> loop

parseBF :: Parser Char [BF]
parseBF = comment *> many (parseBFInstr <* option () comment)

