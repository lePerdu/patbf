# Changelog for pinky

Versions follow the Haskell [PVP](https://pvp.haskell.org) specification.

## [0.2.0.0] - 2019-09-18
### Changes
- Switch to Megaparsec from the custom parsing library. This allows for slightly
  better error messages (mostly because they can include the source file name).

## [0.1.0.1] - 2019-09-18
### Changes
- Change default prompt from `>` to `%` to avoid confusion with the Brainfuck
  command `>`

## [0.1.0.0] - 2019-09-18
### Initial version features
- Brainfuck parsing with extra `#` command for debugging
- Add and move collapsing
- "Linear loop" optimizations for loops with a balanced number of moves and only
  containing moves and increments/decrements.
- Basic command-line interface and REPL

[Unreleased]: https://github.com/lePerdu/pinky

