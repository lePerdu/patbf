-- | Small brainfuck programs for testing interpreters
module Programs where

import Pinky.Brainfuck

-- | Does nothing
emptyProgram :: Bf
emptyProgram = Bf []

-- | Echoes input to output
catProgram :: Bf
catProgram =
  Bf
    [ Input,
      Loop $
        Bf
          [ Output,
            Input
          ]
    ]

-- | Echoes input to output in reverse order
reverseProgram :: Bf
reverseProgram =
  Bf
    [ MoveRight,
      Input,
      Loop $
        Bf
          [ MoveRight,
            Input
          ],
      MoveLeft,
      Loop $
        Bf
          [ Output,
            MoveLeft
          ]
    ]

-- | Prints "0"
print0Program :: Bf
print0Program = intoBf $ do
  -- This one would be a pain to write in "raw" Brainfuck
  incrementN 8
  loop $ do
    decrement
    moveRight
    incrementN 6
    moveLeft
  moveRight
  output

-- | Prints "Hello World!\n"
--
-- Source: https://esolang.org/wiki/Brainfuck#Examples
--
-- +++++ +++               Set Cell #0 to 8
-- [
--     >++++               Add 4 to Cell #1; this will always set Cell #1 to 4
--     [                   as the cell will be cleared by the loop
--         >++             Add 4*2 to Cell #2
--         >+++            Add 4*3 to Cell #3
--         >+++            Add 4*3 to Cell #4
--         >+              Add 4 to Cell #5
--         <<<<-           Decrement the loop counter in Cell #1
--     ]                   Loop till Cell #1 is zero
--     >+                  Add 1 to Cell #2
--     >+                  Add 1 to Cell #3
--     >-                  Subtract 1 from Cell #4
--     >>+                 Add 1 to Cell #6
--     [<]                 Move back to the first zero cell you find; this will
--                         be Cell #1 which was cleared by the previous loop
--     <-                  Decrement the loop Counter in Cell #0
-- ]                       Loop till Cell #0 is zero
--
-- The result of this is:
-- Cell No :   0   1   2   3   4   5   6
-- Contents:   0   0  72 104  88  32   8
-- Pointer :   ^
--
-- >>.                     Cell #2 has value 72 which is 'H'
-- >---.                   Subtract 3 from Cell #3 to get 101 which is 'e'
-- +++++ ++..+++.          Likewise for 'llo' from Cell #3
-- >>.                     Cell #5 is 32 for the space
-- <-.                     Subtract 1 from Cell #4 for 87 to give a 'W'
-- <.                      Cell #3 was set to 'o' from the end of 'Hello'
-- +++.----- -.----- ---.  Cell #3 for 'rl' and 'd'
-- >>+.                    Add 1 to Cell #5 gives us an exclamation point
-- >++.                    And finally a newline from Cell #6
helloWorldProgram :: Bf
helloWorldProgram = intoBf $ do
  incrementN 8
  loop $ do
    moveRight
    incrementN 4
    loop $ do
      moveRight
      incrementN 2
      moveRight
      incrementN 3
      moveRight
      incrementN 3
      moveRight
      increment
      moveN (-4)
      decrement

    moveRight
    increment
    moveRight
    increment
    moveRight
    decrement
    moveN 2
    increment
    loop moveLeft

    moveLeft
    decrement

  moveN 2
  output

  moveRight
  incrementN (-3)
  output

  incrementN 7
  output
  output

  incrementN 3
  output

  moveN 2
  output

  moveLeft
  decrement
  output

  moveLeft
  output

  incrementN 3
  output

  incrementN (-6)
  output

  incrementN (-8)
  output

  moveN 2
  increment
  output

  moveRight
  incrementN 2
  output
