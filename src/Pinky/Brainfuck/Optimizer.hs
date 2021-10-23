module Pinky.Brainfuck.Optimizer
  ( BfOptCell (..),
    interpretOptimized,
  )
where

import Pinky.Brainfuck.Language
import Pinky.Brainfuck.Optimizer.Internal
import Pinky.Brainfuck.Machine
import Pinky.Brainfuck.Tape.Vector

interpretOptimized :: (BfOptCell c, BrainfuckMachine c m) => Bf -> m ()
interpretOptimized = evalBfOptTape . runBfEffects . runBfOptState . parseRaw
