# Changelog for pinky

## Unreleased changes

### Added

- `fromBf :: Bf -> BrainfuckM ()`
- Proper error messages (using `errorBundlePretty` and some custom labels)

### Changed

- Replace `BrainfuckTape` with `MonadBfTape` to allow multiple implementations.
  - `Pinky.Brainfuck.Tape.List` is the original, naive list-based
    implementation.
  - `Pinky.Brainfuck.Tape.Vector` is a more performant, array-based
    implementation.
- Rename `trimTape` -> `truncateTape` and move into `Brainfuck.Tape` module
- Convert `BrainfuckMachine` to use functional dependencies instead of type
  families. This makes type declarations using it more straight-forward and
  avoids having to add `{-# LANGUAGE TypeFamilies #-}` everywhere it's used.
- `BufferMachine` now stores buffers of the cell type rather than strings to
  get rid of difficulties related to failed character conversions.

### Removed

- `toChar` and `fromChar` from `BfCell`. Character conversion is now the
  responsability of `BrainfuckMachine` instances.

## 0.1.0

- Initial release
