{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Pinky.Brainfuck.Optimizer.Internal where

import Control.Applicative
import Data.Foldable
import Control.Monad.State
-- TODO Use strict version?
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Maybe
  ( fromMaybe,
    maybe,
  )
import Data.Word
import Lens.Micro.Platform
import Pinky.Brainfuck.Language
import Pinky.Brainfuck.Machine
import Pinky.Brainfuck.Monad
import Pinky.Brainfuck.Tape

-- | Tighter restriction on the cell type for the optimizer
--
-- Requires being able to determine the solution to a linear equation, even if
-- the type wraps.
class BfCell t => BfOptCell t where
  -- | Attempts to solve the equation a + b*x = 0, modulo the maximum value of
  -- the cell if applicable.
  -- TODO Rename this
  solveWithWrap :: t -> t -> Maybe t

gcdRem :: Integral t => t -> t -> (t, t, t)
gcdRem a b = if r == 0 then (b, 0, 1) else (g, y, x - y * q)
  where
    (q, r) = a `divMod` b
    (g, x, y) = gcdRem b r

-- | Equivalent to gcdRem MOD b, where MOD is the modulus of the integer type.
-- A hack is required to avoid dividing by MOD, which would be 0 as represented
-- in a fixed-sized integer.
gcdRemMax :: Integral t => t -> (t, t, t)
gcdRemMax b = if r == 0 then (b, 0, 1) else (g, y, x - y * q)
  where
    (q', r) = (- b) `divMod` b
    q = q' + 1
    (g, x, y) = gcdRem b r

solveModulo :: (Integral t, Num t) => t -> t -> Maybe t
solveModulo a b =
  let (g, _, x) = gcdRemMax b
      (ag, agr) = a `divMod` g
   in if agr == 0 then Just (- ag * x) else Nothing

instance BfOptCell Word8 where
  solveWithWrap = solveModulo

instance BfOptCell Word where
  solveWithWrap = solveModulo

-- | Expression for a cell's value based on other cells
data CellExpr c
  = CellExact !c
  | CellRef !Int -- relative index (0 being the current cell)
  | CellAdd (CellExpr c) (CellExpr c)
  | CellMul (CellExpr c) (CellExpr c)
  deriving (Show, Eq, Ord)

-- | An sequence of shifts and increments/decrements, or other recognized
-- constructs which have a "pure" effect on the tape.
data TapeEffect c = TapeEffect
  { _tapeEffExprs :: IntMap (CellExpr c),
    _tapeEffOffset :: !Int
  }
  deriving (Show)

-- | Representation of all effects on the tape
data BfEffect c
  = TapeEff (TapeEffect c)
  | InputEff
  | OutputEff
  | LoopEff [BfEffect c]
  deriving (Show)

newtype BfOptState c = BfOptState {_bfOptEffects :: [BfEffect c]}

makeLenses ''BfOptState

newtype BfOptimizer c a = BfMonadOpt {unBfOpt :: State (BfOptState c) a}
  deriving (Functor, Applicative, Monad, MonadState (BfOptState c))

runBfOptState :: BfOptimizer c () -> [BfEffect c]
runBfOptState opt = _bfOptEffects $ execState (unBfOpt opt) (BfOptState [])

parseRaw :: BfOptCell c => Bf -> BfOptimizer c ()
parseRaw = traverse_ parseInstr . _bfCode
  where
    incEff n =
      IntMap.singleton
        0
        (cellAdd (CellExact n) (CellRef 0))

    parseInstr Increment = pushTapeEff $ TapeEffect (incEff 1) 0
    parseInstr Decrement = pushTapeEff $ TapeEffect (incEff (-1)) 0
    parseInstr MoveLeft = pushTapeEff $ TapeEffect IntMap.empty (-1)
    parseInstr MoveRight = pushTapeEff $ TapeEffect IntMap.empty 1
    parseInstr Input = pushEff InputEff
    parseInstr Output = pushEff OutputEff
    parseInstr Debug = pure () -- Just ignore this
    parseInstr (Loop inner) =
      let innerEffects = runBfOptState (parseRaw inner)
          -- Fallback action
          pushRegLoop = pushEff (LoopEff innerEffects)
       in -- Try to convert the loop into a TapeEff, then fallback to just
          -- a LoopEff
          case innerEffects of
            [TapeEff tapeEff] ->
              maybe pushRegLoop pushTapeEff (tapeEffLoop tapeEff)
            _ -> pushRegLoop

pushEff :: Integral c => BfEffect c -> BfOptimizer c ()
pushEff eff = bfOptEffects %= (eff :)

pushTapeEff :: Integral c => TapeEffect c -> BfOptimizer c ()
pushTapeEff tapeEff = bfOptEffects %= pushOrMerge
  where
    pushOrMerge (TapeEff existing : rest) =
      TapeEff (existing <> tapeEff) : rest
    pushOrMerge rest = TapeEff tapeEff : rest

-- | Like CellAdd, but simplify and cannonize the expression
cellAdd :: Integral c => CellExpr c -> CellExpr c -> CellExpr c
cellAdd (CellExact a) (CellExact b) = CellExact (a + b)
cellAdd (CellExact a) (CellAdd (CellExact b) c) = cellAdd (CellExact (a + b)) c
cellAdd (CellExact 0) b = b
cellAdd (CellAdd a b) c = cellAdd a (cellAdd b c)
cellAdd a b@(CellAdd c d)
  | a <= c = CellAdd a b
  | otherwise = cellAdd c (cellAdd a d)
cellAdd a b
  | a <= b = CellAdd a b
  | otherwise = cellAdd b a

-- | Like CellMul, but simplify and cannonize the expression
cellMul :: Integral c => CellExpr c -> CellExpr c -> CellExpr c
cellMul (CellExact a) (CellExact b) = CellExact (a * b)
cellMul (CellExact a) (CellMul (CellExact b) c) = cellMul (CellExact (a * b)) c
cellMul (CellExact 1) b = b
cellMul (CellMul a b) c = cellMul a (cellMul b c)
cellMul a b@(CellMul c d)
  | a <= c = CellMul a b
  | otherwise = cellMul c (cellMul a d)
cellMul a b
  | a <= b = CellMul a b
  | otherwise = cellMul b a

-- | Shift all refernces in a CellExpr
cellExprShift :: Integral c => Int -> CellExpr c -> CellExpr c
cellExprShift shift = doShift
  where
    doShift (CellExact v) = CellExact v
    doShift (CellRef ref) = CellRef (ref + shift)
    doShift (CellAdd x y) = cellAdd (doShift x) (doShift y)
    doShift (CellMul x y) = cellMul (doShift x) (doShift y)

resolveCellRefs ::
  Integral c => Int -> IntMap (CellExpr c) -> CellExpr c -> CellExpr c
resolveCellRefs offset context = resolve
  where
    resolve exact@(CellExact _) = exact
    resolve refExpr@(CellRef ref) =
      maybe
        refExpr
        -- Offset the result since it is being spliced into a different
        -- offset
        (cellExprShift ref)
        -- Offset the lookup since the map is based at the offset
        (IntMap.lookup (ref + offset) context)
    resolve (CellAdd x y) = cellAdd (resolve x) (resolve y)
    resolve (CellMul x y) = cellMul (resolve x) (resolve y)

instance Integral c => Semigroup (CellExpr c) where
  x <> y = resolveCellRefs 0 (IntMap.singleton 0 x) y

instance Integral c => Monoid (CellExpr c) where
  mempty = CellRef 0

mergeCellExprs ::
  Integral c =>
  IntMap (CellExpr c) ->
  IntMap (CellExpr c) ->
  IntMap (CellExpr c)
mergeCellExprs xs ys =
  let resolvedYs = IntMap.mapWithKey (`resolveCellRefs` xs) ys
   in IntMap.unionWith (\_ y -> y) xs resolvedYs

instance Integral c => Semigroup (TapeEffect c) where
  eff1 <> eff2 =
    -- let eff2ShiftedExprs' = fmap (cellExprShift (_tapeEffOffset eff1))
    --                             (_tapeEffExprs eff2)
    let eff2ShiftedExprs =
          IntMap.mapKeysMonotonic
            (+ _tapeEffOffset eff1)
            (_tapeEffExprs eff2)
     in TapeEffect
          (mergeCellExprs (_tapeEffExprs eff1) eff2ShiftedExprs)
          (_tapeEffOffset eff1 + _tapeEffOffset eff2)

instance Integral c => Monoid (TapeEffect c) where
  mempty = TapeEffect IntMap.empty 0

cellApplyShifted ::
  Integral c =>
  Int ->
  (CellExpr c -> CellExpr c) ->
  CellExpr c ->
  CellExpr c
cellApplyShifted shift f expr =
  cellExprShift (- shift) $ f $ cellExprShift shift expr

mapEffects ::
  Integral c => (CellExpr c -> CellExpr c) -> TapeEffect c -> TapeEffect c
mapEffects f effect =
  effect
    { _tapeEffExprs =
        IntMap.mapWithKey
          (`cellApplyShifted` f)
          (_tapeEffExprs effect)
    }

cellExprDecr :: Integral c => CellExpr c
cellExprDecr = cellAdd (CellExact (-1)) (CellRef 0)

tapeEffZeroHead :: Integral c => TapeEffect c
tapeEffZeroHead = TapeEffect (IntMap.singleton 0 (CellExact 0)) 0

cellExprRepeat :: Integral c => Int -> CellExpr c -> Maybe (CellExpr c)
cellExprRepeat timesOff expr = case expr of
  exact@(CellExact _) -> Just exact
  CellAdd x@(CellExact _) y ->
    Just $ cellAdd (cellMul x (CellRef timesOff)) y
  _ -> Nothing

cellExprIsEmpty :: Integral c => CellExpr c -> Bool
cellExprIsEmpty (CellRef ref) | ref == 0 = True
cellExprIsEmpty _ = False

tapeEffIsEmpty :: Integral c => TapeEffect c -> Bool
tapeEffIsEmpty (TapeEffect exprs offset) =
  offset == 0 && all cellExprIsEmpty exprs

-- | Attempt to reduce a loop with a TapeEffect into another TapeEffect.
tapeEffLoop :: Integral c => TapeEffect c -> Maybe (TapeEffect c)
tapeEffLoop (TapeEffect exprs endOffset)
  | endOffset /= 0 = Nothing
  | IntMap.lookup 0 exprs == Just (cellAdd (CellExact (-1)) (CellRef 0)) = do
    repeated <-
      IntMap.traverseWithKey
        (\offset -> cellExprRepeat (- offset))
        exprs
    return $ TapeEffect repeated endOffset <> tapeEffZeroHead
  | otherwise = Nothing

-- | Runs a series of effects on a Brainfuck tape
--
-- Note that the effects are run in reverse order since that is how they are
-- built.
runBfEffects ::
  (BfCell c, BrainfuckMachine m c) =>
  [BfEffect c] ->
  BrainfuckTape c m ()
runBfEffects [] = pure ()
runBfEffects (eff : rest) = runBfEffects rest >> runBfEffect eff

-- | Runs a single effect on a Brainfuck tape
runBfEffect ::
  (BfCell c, BrainfuckMachine m c) => BfEffect c -> BrainfuckTape c m ()
runBfEffect (TapeEff eff) = runTapeEffect eff
runBfEffect InputEff = do
  input <- lift bfGetChar
  -- TODO Abstract out EOF handling
  setCell (fromMaybe 0 input)
runBfEffect OutputEff = readCell >>= lift . bfPutChar
runBfEffect (LoopEff loopCode) = runLoop
  where
    runLoop = do
      cell <- readCell
      unless (cell == 0) (runBfEffects loopCode >> runLoop)

-- | Runs a "pure" tape effect on a Brainfuck tape
runTapeEffect ::
  (BfCell c, BrainfuckMachine m c) => TapeEffect c -> BrainfuckTape c m ()
runTapeEffect (TapeEffect exprs offset) = do
  -- Evaluate the cells first since they should not be inter-dependantly
  -- calculated
  newCellVals <- IntMap.traverseWithKey evalCellExpr exprs
  -- liftIO (print (TapeEffect exprs offset))
  IntMap.traverseWithKey setCellOffset newCellVals
  moveHead offset

-- | Use tape state to evaluate a CellExpr
evalCellExpr ::
  (BfCell c, BrainfuckMachine m c) => Int -> CellExpr c -> BrainfuckTape c m c
evalCellExpr offset = eval
  where
    eval (CellExact n) = pure n
    eval (CellRef ref) = readCellOffset (offset + ref)
    eval (CellAdd l r) = liftA2 (+) (eval l) (eval r)
    eval (CellMul l r) = liftA2 (*) (eval l) (eval r)

-- bfOptToRaw :: BfOptimizer c a -> [Bf]
-- bfOptToRaw bf = _bfOptEffects $ execState (unBfOpt bf) (BfOptState [])

-- | Convert a list of BfEffects to raw Brainfuck code.
--
-- Since effects are put in reverse order, this can't just be concatMap
-- TODO Is calling reverse the most efficient way?
-- bfEffectsToRaw :: BfCell c => [BfEffect c] -> [Bf]
-- bfEffectsToRaw = concatMap bfEffectToRaw . reverse

-- bfEffectToRaw :: BfCell c => BfEffect c -> [Bf]
-- bfEffectToRaw InputEff       = [Input]
-- bfEffectToRaw OutputEff      = [Output]
-- bfEffectToRaw DebugEff       = [Debug]
-- bfEffectToRaw (LoopEff code) = [Loop (bfEffectsToRaw code)]
-- bfEffectToRaw (TapeEff eff ) = tapeEffectToRaw eff

-- | Convert a TapeEffect into raw Bf code.
--
-- This is very difficult since it has to figure out dependencies between cells
-- to correctly output Bf code.
-- tapeEffectToRaw :: BfCell c => TapeEffect c -> [Bf]
-- tapeEffectToRaw
