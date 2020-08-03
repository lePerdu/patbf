{-# LANGUAGE OverloadedStrings #-}

module ParserSpec where

import Data.Either.Combinators
import qualified Data.Text as T
import Pinky
import Pinky.Brainfuck
import Test.Hspec
import Utils
import Test.QuickCheck

doParse :: T.Text -> Either String Bf
doParse = runBfParser ""

doParse' :: T.Text -> Either String [BfInstr]
doParse' = fmap _bfCode . doParse

trivialSpec = do
  it "parses empty file" $
    doParse' "" `shouldBe` Right []

  it "parses `+`" $
    doParse' "+" `shouldBe` Right [Increment]

  it "parses `-`" $
    doParse' "-" `shouldBe` Right [Decrement]

  it "parses `<`" $
    doParse' "<" `shouldBe` Right [MoveLeft]

  it "parses `>`" $
    doParse' ">" `shouldBe` Right [MoveRight]

  it "parses `,`" $
    doParse' "," `shouldBe` Right [Input]

  it "parses `.`" $
    doParse' "." `shouldBe` Right [Output]

  it "parses empty loop" $
    doParse' "[]" `shouldBe` Right [Loop (Bf [])]

  context "with unbalanced brackets" $ do
    it "fails on `[`" $
      doParse' "[" `shouldSatisfy` isLeft

    it "fails on `]`" $
      doParse' "]" `shouldSatisfy` isLeft

    it "fails on `][`" $
      doParse' "][" `shouldSatisfy` isLeft

inverseSpec = do
  it "parse inverts print" $
    property $
      \bf -> (doParse . T.pack . printBf) bf == Right bf

  it "parse inverts print with comments" $
    property $ do
      bf <- arbitrary
      commented <- insertComments (printBf bf)
      return (doParse (T.pack commented) === Right bf)

spec = do
  trivialSpec
  inverseSpec
