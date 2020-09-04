module Pinky.Brainfuck.Parser
  ( parseBf,
  )
where

import Data.Functor
import Data.Void
import Data.Text (Text)
import Pinky.Brainfuck.Language
-- TODO Does this really warrant megaparsec dependency?
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lex

type Parser = Parsec Void Text

command :: Parser BfInstr
command =
  choice $
    zipWith
      (\c bf -> char c $> bf)
      "+-<>,.#"
      [Increment, Decrement, MoveLeft, MoveRight, Input, Output, Debug]

loop :: Parser BfInstr
loop =
  Loop
    <$> between
      (char '[' <?> "'[' (loop start)")
      (char ']' <?> "']' (loop end)")
      instrs

comment :: Parser ()
comment = void $ takeWhileP Nothing (`notElem` "+-<>[].,#")

instr :: Parser BfInstr
instr = command <|> loop

instrs :: Parser Bf
instrs = Bf <$> (comment *> many (Lex.lexeme comment instr))

parseBf :: Parser Bf
parseBf = instrs <* eof
