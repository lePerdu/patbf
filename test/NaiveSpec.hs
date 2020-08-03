{-# LANGUAGE ScopedTypeVariables #-}

module NaiveSpec where

import Control.Monad
import Data.Word
import ParserSpec
import Pinky.Brainfuck
import Pinky.Brainfuck.Machine
import Programs
import Test.Hspec
import Test.QuickCheck hiding (output)
import Utils

cellTypeSpec :: forall c. (BfCell c, Arbitrary c) => CellTypeSpec c
cellTypeSpec = CellTypeSpec $ do
  it "empty program" $
    property $
      \input -> runBfNaive emptyProgram input === (input, "")

  it "cat program" $
    property $
      \(NonNull input) -> runBfNaive catProgram input === ("", input)

  it "reverse program" $
    property $
      \(NonNull input) ->
        runBfNaive reverseProgram input === ("", reverse input)

  it "print0 program" $
    property $
      \(NonNull input) -> runBfNaive print0Program input === (input, "0")

  it "hello world program" $
    property $
      \(NonNull input) ->
        runBfNaive helloWorldProgram input === (input, "Hello World!\n")
  where
    runBfNaive code input =
      execBufferMachine (interpretBasic code :: BufferMachine c ()) input

spec = do
  context "with Word8" $
    runCellType (cellTypeSpec :: CellTypeSpec Word8)

  context "with Word" $
    runCellType (cellTypeSpec :: CellTypeSpec Word)
