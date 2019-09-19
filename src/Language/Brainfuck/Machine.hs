{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Brainfuck.Machine
    ( BFMachine
    , BrainfuckM
    , BFCell(..)
    , modifyHead
    , moveHead
    , readCell
    , setCell
    , modifyCell
    , modifyCellOffset
    , debugBF
    , runBFMachine
    )
where

import           Data.Word
import           Data.Functor                   ( void )
import qualified Data.Vector.Unboxed.Mutable   as M
import           Control.Monad.State.Strict
import           Lens.Micro.Platform

-- | Type of cell which can be stored in a brainfuck machine
-- TODO Make Boxed version so that Integer can be used
class (Integral t, Num t, M.Unbox t, Show t) => BFCell t where
    -- | Attempts to solve the equation a + b*x = 0, modulo the maximum value of
    -- the cell if applicable.
    -- TODO Rename this
    solveWithWrap :: t -> t -> Maybe t

gcdRem :: Integral t => t -> t -> (t, t, t)
gcdRem a b = if r == 0 then (b, 0, 1) else (g, y, x - y * q)  where
    (q, r)    = a `divMod` b
    (g, x, y) = gcdRem b r

-- | Equivalent to gcdRem MOD b, where MOD is the modulus of the integer type.
-- A hack is required to avoid dividing by MOD, which would be 0 as represented
-- in a fixed-sized integer.
gcdRemMax :: Integral t => t -> (t, t, t)
gcdRemMax b = if r == 0 then (b, 0, 1) else (g, y, x - y * q)  where
    (q', r) = (-b) `divMod` b
    q = q' + 1
    (g, x, y) = gcdRem b r

solveModulo :: (Integral t, Num t) => t -> t -> Maybe t
solveModulo a b =
    let (g, _, x) = gcdRemMax b
        (ag, agr) = a `divMod` g
    in  if agr == 0 then Just (-ag * x) else Nothing

instance BFCell Word8 where
    solveWithWrap = solveModulo

instance BFCell Word where
    solveWithWrap = solveModulo

data BFMachine t = BFMachine { _bfTape :: !(M.IOVector t)
                             , _bfHead :: !Int
                             }

makeLenses ''BFMachine

initSize :: Int
initSize = 256

initMachine :: BFCell t => IO (BFMachine t)
initMachine = do
    initTape <- M.new initSize
    pure $ BFMachine initTape 0

newtype BrainfuckM t a = BrainfuckM { unBF :: StateT (BFMachine t) IO a }
    deriving (Functor, Applicative, Monad, MonadState (BFMachine t), MonadIO)

modifyHead :: (Int -> Int) -> BrainfuckM t ()
modifyHead f = bfHead %= f

moveHead :: Int -> BrainfuckM t ()
moveHead n = bfHead += n

expandTape :: BFCell t => BrainfuckM t ()
expandTape = do
    BFMachine tape pos <- get
    let len = M.length tape
    when (pos >= len) (void $ liftIO $ M.grow tape (pos - len + 1))

readCell :: BFCell t => BrainfuckM t t
readCell = do
    expandTape
    BFMachine tape pos <- get
    liftIO $ M.read tape pos

modifyCell :: BFCell t => (t -> t) -> BrainfuckM t ()
modifyCell f = do
    expandTape
    BFMachine tape pos <- get
    liftIO $ M.modify tape f pos

setCell :: BFCell t => t -> BrainfuckM t ()
setCell = modifyCell . const

modifyCellOffset :: BFCell t => (t -> t) -> Int -> BrainfuckM t ()
modifyCellOffset f offset = do
    -- TODO Get rid of the increment / decrement and just modify the vector
    -- directly
    bfHead += offset
    modifyCell f
    bfHead -= offset

debugBF :: BFCell t => BrainfuckM t ()
debugBF = do
    BFMachine tape pos <- get
    let len = M.length tape
    liftIO $ do
        putStrLn ""
        putStrLn $ "head = " ++ show pos
        forM_ [0..len-1] (\i -> do
            val <- M.read tape i
            putStr $ show val ++ ",")
        putStrLn ""

runBFMachine :: BFCell t => BrainfuckM t () -> IO ()
runBFMachine bf = void $ initMachine >>= execStateT (unBF bf)

