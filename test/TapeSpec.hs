{-# LANGUAGE ScopedTypeVariables #-}

module TapeSpec where

import Control.Monad.Identity
import Data.Word
import Pinky.Brainfuck.Machine
import Pinky.Brainfuck.Tape
import Pinky.Brainfuck.Tape.List
import Test.Hspec
import Test.QuickCheck
import Utils

-- runBfTape in the Identity monad
runBfTapeId :: BfCell c => BfListTape c Identity a -> a
runBfTapeId = runIdentity . evalBfListTape

-- | Helper for running all tests with multiple cell types
--
-- Takes a phantom type argument which can be specified at call site.
cellTypeSpec :: forall c. (BfCell c, Arbitrary c) => CellTypeSpec c
cellTypeSpec = CellTypeSpec $ do
  describe "read reflects write" $ do
    it "reads value after write at initial location" $
      property $
        \value -> (value :: c) == runBfTapeId (writeCell value >> readCell)

    it "reads value after move and move back" $
      property $
        \(value, Positive shift) ->
          (value :: c)
            == runBfTapeId
              ( do
                  writeCell value
                  moveHead shift
                  moveHead (- shift)
                  readCell
              )

    it "reads value after write with offset" $
      property $
        \(value, Positive shift) ->
          (value :: c)
            == runBfTapeId
              ( do
                  writeCellOffset shift value
                  moveHead shift
                  readCell
              )

    it "reads value with offset after write with offset" $
      property $
        \(value, Positive shift) ->
          (value :: c)
            == runBfTapeId
              ( do
                  writeCellOffset shift value
                  readCellOffset shift
              )

  describe "writes are independent" $ do
    it "write leaves other locations alone" $
      property $
        \(value, Positive writePos, Positive readPos) ->
          -- Only read from different position
          writePos /= readPos
            ==> (0 :: c)
              == runBfTapeId
                ( do
                    writeCellOffset writePos value
                    readCellOffset readPos
                )

spec = do
  context "with Word8" $
    runCellType (cellTypeSpec :: CellTypeSpec Word8)

  context "with Word" $
    runCellType (cellTypeSpec :: CellTypeSpec Word)
