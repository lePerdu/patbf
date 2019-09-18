# pinky

Pinky is an optimizing Brainfuck interpreter written in Haskell.

## Features

- Interactive REPL with basic command-line editing capabilities (provided by
  [haskeline](https://github.com/judah/haskeline))
- 8-bit or machine-word cell types (infinite precision cells coming soon)
- Line-buffered or unbuffered input
- Debug command (`#`) which prints out the current state to help debugging
- Some basic optimizations to speed up execution (see below for details)

## Building

This project uses [stack](https://docs.haskellstack.org/en/stable/README/). To
build it, just run `stack build`.

## Installation

Running `stack install` will build the application and install it to
`~/.local/bin` (probably somewhere else on non-Linux systems).

## Running

Running Pinky without any giving it a file to execute will start the REPL. To
quit the REPL, press CTRL-D (or just kill it). Passing a file to Pinky will
parse and run the file as Brainfuck and then exit.

### Options

Pink supports the following options (Note: these can be seen by running
`pinky --help`):
- `-u,--unbuffered` Unbuffer the input stream while running brainfuck code. This
  only applies while Brainfuck code is running; when entering in code at the
  REPL, the input stream is still buffered.
- `-o,--optimize` Optimize the code being run. Note that this may change the
  brhavior of the program if the program would have exhibited "undefined
  behavior" such as moving before the initial cell or looping indefinitely.
- `-c,--cell (8|word)` Set type of integer stored in cells. Currently this
  supports 8-bit numbers and the default word size of the host machine.

