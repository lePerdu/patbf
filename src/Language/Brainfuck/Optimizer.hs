{-# LANGUAGE LambdaCase #-}

module Language.Brainfuck.Optimizer
    ( CollapsedBF(..)
    , OptBF(..)
    , LinearIncr(..)
    , collapseBF
    , optimizeCollapsed
    , optimizeBF
    )
where

import           Pinky.Internal.Parser
import           Control.Applicative.Combinators
import           Data.Functor
import           Control.Monad                  ( foldM )
import           Data.Maybe
import qualified Data.IntMap                   as M

import           Language.Brainfuck.Parser               ( BF(..) )

data CollapsedBF a
    = Incr !a
    | Move !Int
    | CLoop [CollapsedBF a]
    | CInput
    | COutput
    | CDebug
    deriving (Show)

-- Collapse Increment/Decrement
collapseIncr :: Integral a => Parser BF [CollapsedBF a]
collapseIncr =
    (\incs -> case sum incs of
            0 -> []
            n -> [Incr n]
        )
        <$> some (token Increment $> 1 <|> token Decrement $> (-1))

-- Collapse MoveLeft/MoveRight
collapseMove :: Parser BF [CollapsedBF a]
collapseMove =
    (\incs -> case sum incs of
            0 -> []
            n -> [Move n]
        )
        <$> some (token MoveRight $> 1 <|> token MoveLeft $> (-1))

-- "Collapse" sequential loops
-- A loop immediately following another loop will never execute (since the
-- previous one will have zeroed out the cell), so it can be removed
collapseLoops :: Integral a => Parser BF (CollapsedBF a)
collapseLoops = CLoop . collapseBF . head <$> some
    (getToken >>= \case
        Loop code -> pure code
        _         -> fail "expected Loop"
    )

collapseBF :: Integral a => [BF] -> [CollapsedBF a]
collapseBF code = case runParser' collapseParser code of
    Right res -> res
    _         -> error "unexpected parse failure"
  where
    passThrough    = token Input $> CInput <|> token Output $> COutput
    collapseParser = mconcat <$> many
        (collapseIncr <|> collapseMove <|> fmap
            (: [])
            (collapseLoops <|> passThrough)
        )

data LinearIncr a = LinearIncr { multipliers :: M.IntMap a
                               , loopIncrement :: a
                               } deriving (Show)

data OptBF a
    = OIncr !a
    | OMove !Int
    | OInput
    | OOutput
    | SetZero
    | LinearLoop (LinearIncr a)
    | OLoop [OptBF a]
    | ODebug
    deriving (Show)

optimizeLinearLoop :: Integral a => [OptBF a] -> OptBF a
optimizeLinearLoop [] = SetZero
optimizeLinearLoop [OIncr _] =
    -- Any increment will either zero out the cell or loop indefinitely
    SetZero
optimizeLinearLoop code = case foldM combineInstr (M.empty, 0) code of
    Just (incs, 0) ->
        -- Remove the increment at the original position (the one which is
        -- used as the condition). Nothing means 0
        let (maybeCondIncr, incs') =
                    M.updateLookupWithKey (\_ _ -> Nothing) 0 incs
            condIncr = fromMaybe 0 maybeCondIncr
        in  LinearLoop (LinearIncr incs' condIncr)
    _ -> OLoop code -- Fallback to a normal loop
  where
    -- Combine an increment, removing it if the resulting total increment is 0
    combineInc new maybeOrig = case fromMaybe 0 maybeOrig + new of
        0 -> Nothing
        n -> Just n
    combineInstr (increments, pos) instr = case instr of
        OIncr n -> Just (M.alter (combineInc n) pos increments, pos)
        OMove n -> Just (increments, pos + n)
        _       -> Nothing

-- | Optimize brainfuck code.
-- Some optimizations change the semantics of the program when said semantics
-- are to (potentially) loop indefinitely, but as long as the original program
-- is halting, this will not change the behavior.
optimizeInstr :: Integral a => CollapsedBF a -> OptBF a
optimizeInstr (CLoop contents) =
    optimizeLinearLoop (map optimizeInstr contents)
optimizeInstr (Incr n) = OIncr n
optimizeInstr (Move n) = OMove n
optimizeInstr CInput   = OInput
optimizeInstr COutput  = OOutput

optimizeCollapsed :: Integral a => [CollapsedBF a] -> [OptBF a]
optimizeCollapsed = map optimizeInstr

optimizeBF :: Integral a => [BF] -> [OptBF a]
optimizeBF = optimizeCollapsed . collapseBF

