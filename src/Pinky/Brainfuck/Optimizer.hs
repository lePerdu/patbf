{-# LANGUAGE TypeFamilies #-}

module Pinky.Brainfuck.Optimizer
  ( BfOptCell (..),
    interpretOptimized,
  )
where

import Pinky.Brainfuck.Language
import Pinky.Brainfuck.Optimizer.Internal
import Pinky.Brainfuck.Machine
import Pinky.Brainfuck.Tape

interpretOptimized ::
  (BrainfuckMachine m, BfOptCell c, MachineCell m ~ c) => Bf -> m ()
interpretOptimized = evalBfTape . runBfEffects . runBfOptState . parseRaw
