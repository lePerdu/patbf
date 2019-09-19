{-# LANGUAGE TemplateHaskell #-}

module Types where

import           Lens.Micro.Platform

import           Pinky

data CellSize = CellWord8 | CellWordDefault

data RunOptions = RunOptions
    { _unbufferedInput :: Bool
    , _cellSize :: CellSize
    , _interpOptions :: BFOptions
    }

makeLenses ''RunOptions

data Options = Options { _fileName :: Maybe String, _runOptions :: RunOptions }

makeLenses ''Options

data ReplState = ReplState
    { _replPrompt :: String
    , _replRunOpts :: RunOptions
    }

makeLenses ''ReplState

