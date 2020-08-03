module Pinky.Brainfuck.Internal.Helpers where

import           Data.Maybe                     ( fromMaybe )

import           System.IO.Error                ( isEOFError )
import           Control.Exception

import           Pinky.Brainfuck.Tape

getCharOr0 :: IO Char
getCharOr0 = catch getChar (\e -> if isEOFError e then pure '\0' else throw e)

toCharThrow :: BfCell t => t -> Char
toCharThrow n = fromMaybe (throw BadToCharConv) (toChar n)

fromCharThrow :: BfCell t => Char -> t
fromCharThrow c = fromMaybe (throw (BadFromCharConv c)) (fromChar c)
