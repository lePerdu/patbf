# Pinky

Pinky is an optimizing Brainfuck interpreter written in Haskell.

## Why "Pinky"

[Pinky and the Brain](https://en.wikipedia.org/wiki/Pinky_and_the_Brain)

(Note: the executable is named `pinkybf` because `pinky` conflicts with a
common Linux utility)

## Features

- Interactive REPL with basic command-line editing capabilities (provided by
  [haskeline](https://github.com/judah/haskeline))
- 8-bit or machine-word cell types (infinite precision cells coming soon)
- Line-buffered or unbuffered input
- Some basic optimizations to speed up execution (see below for details)

## Building

This project uses [stack](https://docs.haskellstack.org/en/stable/README/). To
build it, just run `stack build`.

## Installation

Running `stack install` will build the application and install it to a system
directory (`~/.local/bin/` on Linux).

## Running

Running `pinkybf` without any giving it a file to execute will start the
REPL. To quit the REPL, press CTRL-D (or just kill it). Passing a file to
`pinkybf` will parse and run the file as Brainfuck and then exit.

### REPL Options

Settings can be changed at the REPL using some commands:
- `:prompt <string>` Set the prompt to everything following the command name
  (including spaces). This currently does not have any form of quoting, but
  that will be added later.
- `:opt (none|full)` Set the optimization level. See documentation for
  what each level entails (TODO Add a description in this README).
- `:cell (8|byte|word)` Set the cell size to either a single-byte (`8` or
  `byte`) or a machine word.
- `:buffer` Line-buffer the input steam.
- `:unbuffer` Make the input stream unbuffered when running Brainfuck code.

Commands are only parsed if the first character in the line is a colon;
otherwise the line is just parsed as Brainfuck.

### Command-line Options

Pink supports the following options (Note: these can be seen by running
`pinkybf --help`):
- `-u,--unbuffered` Unbuffer the input stream while running brainfuck code.
  This only applies while Brainfuck code is running; when entering in code at
  the REPL, the input stream is still buffered.
- `-o,--optimize` Optimize the code being run. Note that this may change the
  brhavior of the program if the program would have exhibited "undefined
  behavior" such as moving before the initial cell or looping indefinitely.
- `-c,--cell (8|byte|word)` Set type of integer stored in cells. Currently this
  supports 8-bit numbers and the default word size of the host machine.
