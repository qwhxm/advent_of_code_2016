-- * adventofcode.com/2016/day/8
import Data.List

-- | instructions for the screen (rect AxB, rotate row y=A by B,
-- | rotate column x=A by B)
data Instruction = Rect Int Int | RotateRow Int Int | RotateColumn Int Int

-- | instructions given as puzzle input
instructions :: [Instruction]
instructions =
    [Rect 1 1, RotateRow 0 5, Rect 1 1, RotateRow 0 6, Rect 1 1, RotateRow 0 5,
     Rect 1 1, RotateRow 0 2, Rect 1 1, RotateRow 0 5, Rect 2 1, RotateRow 0 2,
     Rect 1 1, RotateRow 0 4, Rect 1 1, RotateRow 0 3, Rect 2 1, RotateRow 0 7,
     Rect 3 1, RotateRow 0 3, Rect 1 1, RotateRow 0 3, Rect 1 2, RotateRow 1 13,
     RotateColumn 0 1, Rect 2 1, RotateRow 0 5, RotateColumn 0 1, Rect 3 1,
     RotateRow 0 18, RotateColumn 13 1, RotateColumn 7 2, RotateColumn 2 3,
     RotateColumn 0 1, Rect 17 1, RotateRow 3 13, RotateRow 1 37,
     RotateRow 0 11, RotateColumn 7 1, RotateColumn 6 1, RotateColumn 4 1,
     RotateColumn 0 1, Rect 10 1, RotateRow 2 37, RotateColumn 19 2,
     RotateColumn 9 2, RotateRow 3 5, RotateRow 2 1, RotateRow 1 4,
     RotateRow 0 4, Rect 1 4, RotateColumn 25 3, RotateRow 3 5, RotateRow 2 2,
     RotateRow 1 1, RotateRow 0 1, Rect 1 5, RotateRow 2 10, RotateColumn 39 1,
     RotateColumn 35 1, RotateColumn 29 1, RotateColumn 19 1, RotateColumn 7 2,
     RotateRow 4 22, RotateRow 3 5, RotateRow 1 21, RotateRow 0 10,
     RotateColumn 2 2, RotateColumn 0 2, Rect 4 2, RotateColumn 46 2,
     RotateColumn 44 2, RotateColumn 42 1, RotateColumn 41 1, RotateColumn 40 2,
     RotateColumn 38 2, RotateColumn 37 3, RotateColumn 35 1, RotateColumn 33 2,
     RotateColumn 32 1, RotateColumn 31 2, RotateColumn 30 1, RotateColumn 28 1,
     RotateColumn 27 3, RotateColumn 26 1, RotateColumn 23 2, RotateColumn 22 1,
     RotateColumn 21 1, RotateColumn 20 1, RotateColumn 19 1, RotateColumn 18 2,
     RotateColumn 16 2, RotateColumn 15 1, RotateColumn 13 1, RotateColumn 12 1,
     RotateColumn 11 1, RotateColumn 10 1, RotateColumn 7 1, RotateColumn 6 1,
     RotateColumn 5 1, RotateColumn 3 2, RotateColumn 2 1, RotateColumn 1 1,
     RotateColumn 0 1, Rect 49 1, RotateRow 2 34, RotateColumn 44 1,
     RotateColumn 40 2, RotateColumn 39 1, RotateColumn 35 4, RotateColumn 34 1,
     RotateColumn 30 4, RotateColumn 29 1, RotateColumn 24 1, RotateColumn 15 4,
     RotateColumn 14 1, RotateColumn 13 3, RotateColumn 10 4, RotateColumn 9 1,
     RotateColumn 5 4, RotateColumn 4 3, RotateRow 5 20, RotateRow 4 20,
     RotateRow 3 48, RotateRow 2 20, RotateRow 1 41, RotateColumn 47 5,
     RotateColumn 46 5, RotateColumn 45 4, RotateColumn 43 5, RotateColumn 41 5,
     RotateColumn 33 1, RotateColumn 32 3, RotateColumn 23 5, RotateColumn 22 1,
     RotateColumn 21 2, RotateColumn 18 2, RotateColumn 17 3, RotateColumn 16 2,
     RotateColumn 13 5, RotateColumn 12 5, RotateColumn 11 5, RotateColumn 3 5,
     RotateColumn 2 5, RotateColumn 1 5]

-- | pixel of a screen that can be turned on or off by executing instructions
data Pixel = Off | On deriving Eq

-- | representation of a screen state: two-dimensional rectangular "array"
-- | (list of lists) of on/off pixels
type Screen = [[Pixel]]

-- | initial state of the 50x6 screen used in the puzzle
screen :: Screen
screen = replicate 6 $ replicate 50 Off

-- | returns the screen state obtained by executing the given instruction on
-- | the given initial screen state
executeInstruction :: Instruction -> Screen -> Screen
executeInstruction (Rect a b) rs =
    let
        -- rows that will/will not contain the rectangle
        (rs1, rs2) = splitAt b rs
        -- function that turns on pixels to create one row of the rectangle
        f rs = replicate a On ++ drop a rs
    in
        map f rs1 ++ rs2
executeInstruction (RotateRow a b) rs =
    let
        -- function that shifts a row right by b pixels
        f b ps = drop (length ps - b) ps ++ take (length ps - b) ps
    in
        take a rs ++ [f b (rs !! a)] ++ drop (a + 1) rs
executeInstruction (RotateColumn a b) rs =
    transpose $ executeInstruction (RotateRow a b) $ transpose rs

-- | returns the screen state obtained by executing the given instructions one
-- | after another, starting from the given initial screen state
executeInstructions :: [Instruction] -> Screen -> Screen
executeInstructions is = foldr1 (flip (.)) $ map executeInstruction is

-- | solution to part one of the puzzle
solution1 =
    let
        -- final screen state
        screen' = executeInstructions instructions screen
        -- number of pixels turned on
        n = sum $ map (length . filter (== On)) screen'
    in
        n

-- | returns a string displaying the given screen state
printScreen :: Screen -> String
printScreen rs =
    let
        -- function to print one pixel
        f1 p = case p of On -> '*'; Off -> ' ';
        -- function to print one row
        f2 ps = intersperse ' ' $ map f1 ps
    in
        intercalate "\n" $ map f2 rs

-- | solution to part two of the puzzle
solution2 =
    let
        -- final screen state
        screen' = executeInstructions instructions screen
    in
        putStrLn $ printScreen screen'
