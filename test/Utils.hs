module Utils where

import Control.Monad (foldM)
import Data.Char
import Pinky.Brainfuck.Language
import Test.Hspec
import Test.QuickCheck

-- | Generates size-bounded Brainfuck code.
--
-- Note: May require some tuning to make the generated code more realistic
instance Arbitrary Bf where
  arbitrary = Bf <$> sized arb
    where
      basics =
        [ Increment,
          Decrement,
          MoveLeft,
          MoveRight,
          Input,
          Output,
          Debug
        ]
      arbBasic = elements basics

      arbWithLoop n = do
        -- Put part of the size in the loop and the rest after it
        m <- choose (0, n)
        loopCode <- arb m
        afterCode <- arb (n - m)
        return $ Loop (Bf loopCode) : afterCode

      arb 0 = pure []
      arb n =
        frequency
          [ (length basics, (:) <$> arbBasic <*> arb (n - 1)),
            (1, arbWithLoop n)
          ]

-- | Brainfuck code which is only composed of shifts and increments.
newtype PureBf = PureBf Bf deriving (Show)

-- | Generates size-bounded Brainfuck code with only increments and moves.
--
-- Note: May require some tuning to make the generated code more realistic
instance Arbitrary PureBf where
  arbitrary = PureBf . Bf <$> listOf (elements pureInstrs)
    where
      pureInstrs =
        [ Increment,
          Decrement,
          MoveLeft,
          MoveRight
        ]

-- | Generates random comments in a string of Brainfuck code
insertComments :: String -> Gen String
insertComments code =
  do
    leadingComment <- genComment
    commented <- foldM appendCommented leadingComment code
    return commented
  where
    -- Arbitrary (pun intended) amount of comment
    genComment = filter (`notElem` "+-<>,.[]#") <$> resize 12 arbitrary

    appendCommented source instr = do
      comment <- genComment
      return (source ++ [instr] ++ comment)

-- | String without NUL characters for use as input to Brainfuck programs.
--
-- Because NUL is treated as EOF, the input strings cannot contain NUL
-- TODO Implement different null-handling functions
newtype NonNullAscii = NonNull String deriving (Show)

instance Arbitrary NonNullAscii where
  arbitrary = NonNull . filter (\c -> isAscii c && isPrint c) <$> arbitrary
  shrink (NonNull s) = NonNull <$> shrink s

-- | Type with a phantom type parameter 'c' to indicate cell type
--
-- Useful for making test cases generic over the cell type:
-- > cellTypeSpec :: forall c. CellTypeSpec c
-- > cellTypeSpec = CellTypeSpec $ do
-- >   ...
-- >
-- > spec = do
-- >   runCellType (cellTypeSpec :: CellTypeSpec Word8)
-- >   runCellType (cellTypeSpec :: CellTypeSpec Word)
-- >   ...
newtype CellTypeSpec c = CellTypeSpec {runCellType :: Spec}
