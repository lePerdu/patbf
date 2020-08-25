{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Pinky.Brainfuck.Monad
  ( BrainfuckM,
    increment,
    decrement,
    incrementN,
    moveRight,
    moveLeft,
    moveN,
    input,
    output,
    debug,
    loop,
    intoBf,
    fromBf,
  )
where

import Control.Monad.Writer
import Data.Foldable
import Pinky.Brainfuck.Language

-- | Monadic DSL for writing Brainfuck code
newtype BrainfuckM a = BrainfuckM {unBfM :: Writer Bf a}
  deriving (Functor, Applicative, Monad)

tellInstr :: BfInstr -> BrainfuckM ()
tellInstr i = BrainfuckM $ tell $ Bf [i]

-- | Helper function for moveN and incrementN
leftRightN :: Integral a => BfInstr -> BfInstr -> a -> BrainfuckM ()
leftRightN left right n
  | n < 0 = tellList (- n') left
  | otherwise = tellList n' right
  where
    n' = fromIntegral n
    tellList n = BrainfuckM . tell . Bf . replicate n

moveRight :: BrainfuckM ()
moveRight = tellInstr MoveRight

moveLeft :: BrainfuckM ()
moveLeft = tellInstr MoveLeft

moveN :: Integral a => a -> BrainfuckM ()
moveN = leftRightN MoveLeft MoveRight

increment :: BrainfuckM ()
increment = tellInstr Increment

decrement :: BrainfuckM ()
decrement = tellInstr Decrement

incrementN :: Integral a => a -> BrainfuckM ()
incrementN = leftRightN Decrement Increment

input :: BrainfuckM ()
input = tellInstr Input

output :: BrainfuckM ()
output = tellInstr Output

debug :: BrainfuckM ()
debug = tellInstr Debug

-- | Perform an action in a loop
loop :: BrainfuckM () -> BrainfuckM ()
loop = tellInstr . Loop . intoBf

-- | Convert into raw Brainfuck code
intoBf :: BrainfuckM () -> Bf
intoBf = execWriter . unBfM

-- | Convert from raw Brainfuck code
fromBf :: Bf -> BrainfuckM ()
fromBf (Bf code) = traverse_ tellInstr code
