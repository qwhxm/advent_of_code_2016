-- * adventofcode.com/2016/day/2
import Control.Monad.Writer

-- | instructions to move right/up/left/down on a keypad
data Instruction = R | U | L | D

-- | line of instructions that, when followed, leads to one button in the
-- | bathroom code
type InstructionLine = [Instruction]

-- | instruction lines given as puzzle input
instructionLines :: [InstructionLine]
instructionLines =
    [[R, D, L, U, L, D, L, D, D, R, L, L, L, R, L, R, U, L, D, R, L, D, D, R, R,
      R, R, U, R, L, R, L, D, L, U, L, D, L, D, L, D, R, U, L, D, D, L, L, D, R,
      D, R, U, D, L, L, D, D, R, D, U, L, L, L, U, L, L, D, U, L, R, R, L, D, U,
      R, U, L, D, R, U, U, L, L, L, U, U, D, U, R, U, R, R, D, D, L, D, L, D, R,
      R, D, D, L, R, U, R, L, L, D, R, R, R, D, U, L, D, R, U, L, U, R, U, R, U,
      R, U, R, L, L, R, R, L, U, D, U, L, D, R, U, L, L, D, U, R, R, R, L, L, D,
      U, R, D, R, R, U, U, U, R, D, R, L, L, D, R, U, R, U, L, R, U, D, U, L, R,
      R, R, R, R, D, L, R, L, L, D, R, R, R, D, L, D, U, U, D, D, D, U, D, L, D,
      R, U, U, R, R, L, L, U, D, U, D, D, R, R, L, R, R, D, R, U, U, D, U, U, U,
      L, D, U, U, D, L, R, D, L, D, L, L, D, L, L, L, L, R, R, U, R, D, L, D, U,
      U, R, R, L, L, D, L, D, L, L, R, L, L, R, U, L, D, D, R, L, D, L, U, D, L,
      D, D, L, R, D, R, R, D, L, U, L, R, L, L, L, R, U, D, D, U, R, L, D, L, L,
      U, L, R, D, U, U, D, R, R, L, D, U, D, U, D, L, U, U, R, D, U, R, R, D, D,
      L, L, D, R, R, R, L, U, D, U, L, D, U, L, D, D, L, L, U, L, D, D, D, R, R,
      L, L, D, U, R, U, R, U, R, U, U, U, R, R, U, R, R, U, U, D, U, U, U, R, U,
      L, D, L, R, U, L, R, U, R, D, L, D, R, D, D, U, L, D, D, U, L, L, U, R, D,
      D, U, D, D, R, D, R, R, U, L, R, U, U, R, R, D, D, R, L, L, U, U, R, D, R,
      D, D, R, U, D, L, U, U, D, U, R, R, R, L, L, R, R],
     [R, D, R, R, L, U, R, D, D, D, D, L, D, U, D, L, D, R, U, R, R, L, D, L, L,
      L, D, D, L, U, R, L, L, R, U, L, L, U, L, U, U, U, R, L, D, U, R, U, R, U,
      L, D, L, U, R, R, L, R, U, L, D, D, U, U, L, U, L, L, L, R, L, L, R, D, R,
      R, U, U, D, L, U, U, D, D, U, D, D, D, R, D, U, R, L, U, D, D, R, U, L, R,
      U, L, D, D, D, L, U, L, R, D, D, U, R, R, U, U, R, L, R, R, L, R, U, L, L,
      U, R, R, D, U, R, R, R, U, R, L, D, U, L, U, L, U, R, U, L, R, R, L, R, L,
      U, U, R, R, R, U, D, D, L, U, R, R, D, D, U, U, D, R, D, L, L, D, R, L, R,
      U, R, U, D, L, D, L, L, L, L, D, L, R, U, R, D, L, L, R, D, D, U, D, D, L,
      D, L, D, R, R, D, L, R, D, R, D, L, R, R, R, R, U, D, U, U, D, D, R, D, L,
      U, L, U, D, L, U, U, R, L, D, U, D, R, R, R, R, R, L, U, U, U, D, R, R, D,
      L, U, L, L, R, R, L, R, L, D, D, D, L, L, D, L, L, R, D, D, U, U, U, U, D,
      D, U, L, U, D, D, D, U, U, L, D, D, U, U, D, U, R, R, D, L, U, R, L, L, R,
      U, U, U, U, D, U, D, R, L, D, D, D, U, R, D, R, L, D, R, L, R, D, R, U, L,
      R, R, D, D, D, R, D, R, R, R, L, R, D, U, L, U, U, U, L, D, L, D, D, D, U,
      U, R, R, U, R, L, D, L, D, L, L, D, L, U, D, D, L, D, L, R, U, D, R, L, R,
      L, D, U, R, U, D, D, U, R, L, D, R, D, D, L, L, D, D, L, D, R, U, R, R, U,
      L, L, U, R, U, L, U, U, U, U, D, L, R, L, U, U, U, D, L, D, R, U, D, U, R,
      L, R, U, L, L, R, L, L, U, U, U, L, U, R, L, L, L, D, U, L, L, U, D, L, L,
      R, U, L, R, R, L, U, R, R, R, R, L, R, D, R, R, L, L, U, L, L, L, D, U, R,
      D, L, L, D, L, U, D, L, D, U, D, U, R, L, U, R, D, L, U, U, R, R, R, L, R,
      L, L, D, R, L, D, L, D, R, L, R, U, U, U, D, R, L, R, U, D, U, U, U, R],
     [L, L, L, L, U, L, R, D, U, U, D, U, U, D, R, D, U, U, U, R, D, L, L, R, R,
      L, U, D, D, D, R, L, D, U, U, D, D, U, R, L, D, U, D, U, L, D, R, R, R, D,
      D, L, L, L, R, D, D, U, D, D, L, L, L, R, R, L, U, R, D, U, L, R, U, U, D,
      D, R, R, D, L, R, L, R, U, U, U, L, D, D, U, L, D, U, U, U, D, D, L, L, D,
      D, D, D, D, U, R, L, D, R, L, D, D, D, D, R, R, D, U, R, R, D, R, R, R, U,
      U, D, U, U, D, R, L, R, R, R, U, U, R, U, D, U, R, L, R, L, D, U, R, D, D,
      D, U, D, D, U, D, D, D, U, U, D, R, U, D, U, L, D, D, R, D, L, U, L, R, U,
      R, D, U, U, D, L, R, R, D, D, R, R, D, L, R, D, L, R, D, L, U, L, R, L, L,
      R, L, R, L, D, L, R, U, L, D, D, D, D, R, L, D, U, U, R, L, U, U, D, L, L,
      R, R, L, L, L, U, U, U, L, U, R, U, U, D, U, L, R, R, R, U, L, U, R, U, U,
      R, L, D, L, L, R, U, R, U, U, D, U, D, L, L, U, D, L, D, R, L, L, R, R, U,
      U, D, D, R, L, U, D, U, D, R, D, D, R, R, D, D, D, U, R, D, R, U, D, L, L,
      D, L, U, U, D, R, U, R, D, L, L, U, L, L, L, L, U, D, L, R, R, R, U, U, L,
      L, R, R, D, D, U, D, D, D, U, D, D, R, D, R, R, U, L, U, R, R, U, U, D, L,
      U, D, L, D, R, L, L, L, L, D, L, U, U, L, L, U, L, L, D, D, U, D, L, U, L,
      R, D, R, L, D, R, D, L, U, D, U, D, R, R, R, R, L, R, D, L, L, L, D, U, R,
      L, U, L, U, D, D, R, U, R, R, D, R, U, D, L, L, D, R, U, R, R, U, U, D, D,
      D, R, D, U, U, U, L, D, U, R, R, U, L, D, L, L, D, L, D, L, R, D, U, D, U,
      R, R, R, R, D, L, D, R, R, L, U, D, U, R, L, U, D, R, R, L, U, D, D, L, L,
      D, U, U, L, L, D, U, R, R, L, R, D, R, L, U, R, U, R, L, U, U, U, R, R, L,
      U, D, R, R, L, L, U, L, U, U, L, U, D, R, U, D, R, D, L, U, L],
     [L, R, U, U, L, R, R, U, D, U, D, D, L, R, R, D, U, R, R, U, U, R, D, U, R,
      U, R, L, U, L, R, D, U, U, D, U, D, L, D, R, R, U, L, U, R, U, D, U, R, U,
      R, D, R, L, D, D, L, R, U, U, R, L, L, R, D, L, U, R, R, U, L, R, R, R, U,
      D, U, L, R, R, U, L, D, L, U, U, L, D, U, L, L, U, L, L, D, U, D, L, L, U,
      U, U, L, D, L, R, D, R, R, L, U, U, R, U, R, L, L, U, U, U, D, D, L, L, U,
      R, D, U, D, U, R, U, L, R, D, L, D, U, U, L, D, D, R, U, L, L, U, U, U, U,
      R, D, D, R, U, U, R, D, D, D, R, U, U, U, D, R, U, U, L, D, L, L, U, L, D,
      L, U, R, L, R, R, L, R, U, L, R, L, D, L, D, U, R, L, R, L, D, L, R, R, R,
      U, U, R, L, U, U, D, U, L, L, L, R, R, U, R, R, R, L, R, U, L, L, R, L, U,
      U, D, U, L, D, U, L, R, D, D, R, D, R, R, U, R, D, D, R, R, L, U, L, R, D,
      U, R, D, D, D, D, D, L, L, R, R, D, L, L, U, U, U, R, U, U, L, U, D, L, L,
      D, D, U, L, D, U, D, U, U, D, D, R, U, R, D, D, U, R, D, D, R, L, U, R, U,
      D, R, D, R, R, U, L, L, L, U, R, L, U, U, L, R, L, U, D, U, D, D, U, U, U,
      L, D, R, R, R, R, D, L, R, L, D, L, L, D, R, R, D, U, D, U, U, U, R, L, R,
      U, R, D, D, D, R, U, R, R, U, D, R, U, U, R, U, U, D, L, R, D, D, D, L, U,
      D, L, R, U, U, R, U, L, R, R, L, D, D, U, L, R, U, L, D, R, L, R, L, L, D,
      R, L, U, R, R, U, U, D, R, R, R, L, R, D, D, R, L, D, D, L, L, U, R, L, L,
      U, D, L],
     [U, L, U, R, L, R, D, L, R, U, D, L, L, D, U, D, D, R, U, U, U, L, U, L, U,
      D, D, D, D, D, R, R, D, R, U, L, U, D, R, R, U, D, L, R, R, R, L, U, D, L,
      R, U, U, L, R, D, D, R, R, L, R, U, D, L, U, D, U, L, R, U, L, L, U, U, R,
      L, L, R, L, L, L, L, D, R, D, U, U, R, D, U, U, U, L, L, R, U, L, U, U, U,
      D, R, D, R, D, R, U, U, L, U, R, D, U, L, D, L, R, R, U, L, U, U, R, U, R,
      D, U, L, U, L, D, R, R, U, R, D, L, R, U, D, L, U, L, U, L, U, L, U, D, L,
      L, U, U, R, U, L, D, L, L, L, R, D, U, D, D, R, R, L, U, L, U, D, D, R, L,
      L, L, R, U, R, D, D, L, D, L, R, L, L, L, R, D, L, D, R, R, U, U, U, L, R,
      L, R, D, D, D, D, R, U, D, R, U, U, L, D, D, R, R, U, L, L, D, R, R, L, D,
      D, R, R, U, D, R, L, L, D, U, D, R, R, U, D, D, R, D, L, R, U, D, D, R, D,
      D, D, R, L, L, R, D, U, U, L, R, D, R, L, D, U, D, R, L, D, D, L, L, D, D,
      D, U, U, D, D, R, U, L, L, D, L, L, D, R, D, R, R, U, D, D, U, U, U, R, L,
      L, U, U, R, D, L, U, L, U, D, R, U, U, U, D, U, R, U, R, L, R, R, D, U, L,
      L, D, R, D, D, R, L, R, D, U, L, R, D, R, U, R, R, U, D, L, D, D, R, R, R,
      L, U, D, R, L, R, R, R, R, L, L, D, D, L, L, R, L, D, U, D, U, D, D, R, R,
      R, U, U, L, D, R, U, R, D, L, L, D, L, U, U, L, D, L, D, L, D, U, U, D, D,
      U, L, U, D, U, D, R, R, D, R, L, D, R, D, U, R, D, U, U, L, D, U, R, D, R,
      R, D, R, R, L, L, R, L, D, L, U]]

-- | representation of a keypad: two-dimensional rectangular "array" (list of
-- | lists) of characters, where characters corresponding to buttons are
-- | surrounded by "boundary" NUL characters
type Keypad = [[Char]]

-- | keypad used in part one of the puzzle
keypad1 :: Keypad
keypad1 = [['\0', '\0', '\0', '\0', '\0'],
           ['\0',  '1',  '2',  '3', '\0'],
           ['\0',  '4',  '5',  '6', '\0'],
           ['\0',  '7',  '8',  '9', '\0'],
           ['\0', '\0', '\0', '\0', '\0']]

-- | coordinates of a button on keypad: first coordinate is row (from top),
-- | second is column (from left), both zero-indexed and including the boundary
-- | characters in keypad definition
type Coordinates = (Int, Int)

-- | returns the coordinates reached by moving according to the given
-- | instruction from the given coordinates, without considering boundaries of
-- | any particular keypad
move :: Instruction -> Coordinates -> Coordinates
move R (r, c) = (r, c + 1)
move U (r, c) = (r - 1, c)
move L (r, c) = (r, c - 1)
move D (r, c) = (r + 1, c)

-- | returns the button located at given coordinates on the given keypad
buttonAtCoordinates :: Keypad -> Coordinates -> Char
buttonAtCoordinates k (r, c) = k !! r !! c

-- | returns the coordinates reached on the given keypad by following the given
-- | instruction starting from the given coordinates
followInstruction :: Keypad -> Instruction -> Coordinates -> Coordinates
followInstruction k i c =
    let
        c' = move i c
    in
        case buttonAtCoordinates k c' of
            '\0' -> c -- don't move (would leave keypad boundaries)
            _    -> c'

-- | returns the coordinates reached on the given keypad by following the given
-- | instruction line starting from the given coordinates
followInstructionLine :: Keypad -> InstructionLine -> Coordinates -> Coordinates
followInstructionLine k is = foldr1 (flip (.)) $ map (followInstruction k) is

-- | produces a Writer that, when run, returns the coordinates reached on the
-- | given keypad by following the given instruction line starting from the
-- | given coordinates, and writes the button located at these coordinates (as
-- | a single-element list)
followInstructionLine' :: Keypad -> InstructionLine -> Coordinates ->
                          Writer [Char] Coordinates
followInstructionLine' k is c =
    let
        c' = followInstructionLine k is c
    in
        writer (c', [buttonAtCoordinates k c'])

-- | produces a Writer that, when run, returns the coordinates reached on the
-- | given keypad by following the given sequence of instruction lines starting
-- | from the given coordinates, and writes list of buttons reached at the end
-- | of each instruction line
followInstructionLines :: Keypad -> [InstructionLine] -> Coordinates ->
                          Writer [Char] Coordinates
followInstructionLines k is = foldr1 (>=>) $ map (followInstructionLine' k) is

-- | solution to part one of the puzzle
solution1 =
    let
        -- initial coordinates (coordinates of the '5' button on keypad1)
        c = (2, 2)
        -- buttons reached at the end of each instruction line (i.e. the
        -- bathroom code)
        bs = snd $ runWriter $ followInstructionLines keypad1 instructionLines c
    in
        bs

-- | keypad used in part two of the puzzle
keypad2 :: Keypad
keypad2 = [['\0', '\0', '\0', '\0', '\0', '\0', '\0'],
           ['\0', '\0', '\0',  '1', '\0', '\0', '\0'],
           ['\0', '\0',  '2',  '3',  '4', '\0', '\0'],
           ['\0', '5',   '6',  '7',  '8', '9',  '\0'],
           ['\0', '\0',  'A',  'B',  'C', '\0', '\0'],
           ['\0', '\0', '\0',  'D', '\0', '\0', '\0'],
           ['\0', '\0', '\0', '\0', '\0', '\0', '\0']]

-- | solution to part two of the puzzle
solution2 =
    let
        -- initial coordinates (coordinates of the '5' button on keypad2)
        c = (3, 1)
        -- buttons reached at the end of each instruction line (i.e. the
        -- bathroom code)
        bs = snd $ runWriter $ followInstructionLines keypad2 instructionLines c
    in
        bs
