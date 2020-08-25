{-# LANGUAGE ScopedTypeVariables #-}

module OptimizerSpec where

import Data.Word
import Pinky.Brainfuck
import Pinky.Brainfuck.Naive
import Pinky.Brainfuck.Optimizer.Internal
import Pinky.Brainfuck.Tape
import Programs
import Test.Hspec
import Test.Invariant
import Test.QuickCheck
import Utils

-- | Generates CellExpr, cannonized by using cellAdd and cellMul.
instance (Integral c, Arbitrary c) => Arbitrary (CellExpr c) where
  arbitrary = sized arb
    where
      simple =
        [ CellExact <$> arbitrary,
          CellRef <$> arbitrary
        ]

      nested n = map (arbTree n) [cellAdd, cellMul]

      arb n =
        oneof
          ( if n <= 1
              then simple
              else simple ++ nested n
          )
      arbTree n f = f <$> half <*> half
        where
          half = arb (n `div` 2)

-- | Returns upper bounds for how many cells are used to the left and right of
-- the head.
getTapeBounds :: Bf -> (Int, Int)
getTapeBounds (Bf code) =
  (length (filter (== MoveLeft) code), length (filter (== MoveRight) code))

runBoundedTape ::
  BfCell c => Bf -> BrainfuckTape c (BufferMachine c) a -> BfState c
runBoundedTape bf action =
  let (lBound, rBound) = getTapeBounds bf
      machine = runBfTape (moveHead lBound >> action)
      ((_, tape), _) = runBufferMachine machine ""
   in truncateTape rBound tape

runOptBounded :: BfOptCell c => Bf -> BfState c
runOptBounded bf =
  runBoundedTape bf $ runBfEffects $ runBfOptState $ parseRaw bf

cellTypeSpec :: forall c. (BfOptCell c, Arbitrary c) => CellTypeSpec c
cellTypeSpec = CellTypeSpec $ do
  describe "CellExpr" $ do
    describe "cellAdd" $ do
      it "is associative" $
        property $ associative cellAdd'

      it "is commutative" $
        property $ commutative cellAdd'

    describe "cellMul" $ do
      it "is associative" $
        property $ associative cellMul'

      it "is commutative" $
        property $ commutative cellMul'

  describe "TapeEffect" $ do
    -- TODO Run with all cell types
    it "pure instructions generate single TapeEffect" $
      property $
        \(PureBf bf) ->
          bf /= Bf [] -- non-empty code
            ==> case ((runBfOptState $ parseRaw bf) :: [BfEffect c]) of
              [TapeEff _] -> True
              _ -> False

    it "has same effect as naive interpreter" $
      property $
        \(PureBf bf) ->
          let optTape = runOptBounded bf
              naiveTape = runBoundedTape bf $ interpretBasicTape bf
           in (optTape :: BfState c) === naiveTape

    it "loop performs multiplication" $
      property $
        \n m ->
          let bf = intoBf $ do
                incrementN n
                loop $ do
                  decrement
                  moveRight
                  incrementN m
                  moveLeft
                moveRight
              tape = runOptBounded bf
           in _bfHead tape === (n * m :: Word8)

  it "empty program" $
    property $
      \input -> runBfOpt emptyProgram input === (input, "")

  it "cat program" $
    property $
      \(NonNull input) -> runBfOpt catProgram input === ("", input)

  it "reverse program" $
    property $
      \(NonNull input) -> runBfOpt reverseProgram input === ("", reverse input)

  it "print0 program" $
    property $
      \(NonNull input) -> runBfOpt print0Program input === (input, "0")

  it "hello world program" $
    property $
      \(NonNull input) ->
        runBfOpt helloWorldProgram input === (input, "Hello World!\n")
  where
    cellAdd' = cellAdd :: CellExpr c -> CellExpr c -> CellExpr c
    cellMul' = cellMul :: CellExpr c -> CellExpr c -> CellExpr c

    runBfOpt code input =
      execBufferMachine (interpretOptimized code :: BufferMachine c ()) input

spec = do
  context "with Word8" $
    runCellType (cellTypeSpec :: CellTypeSpec Word8)

  context "with Word" $
    runCellType (cellTypeSpec :: CellTypeSpec Word)
