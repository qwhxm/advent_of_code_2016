-- * adventofcode.com/2016/day/1
-- NOTE requires the package "mtl"
import Control.Monad.Writer

-- | instructions to reach Easter Bunny HQ: turn either left or right and walk
-- | the given number of blocks
data Instruction = L Int | R Int

-- | instructions given as puzzle input
instructions :: [Instruction]
instructions =
    [R 1, R 1, R 3, R 1, R 1, L 2, R 5, L 2, R 5, R 1, R 4, L 2, R 3, L 3, R 4,
     L 5, R 4, R 4, R 1, L 5, L 4, R 5, R 3, L 1, R 4, R 3, L 2, L 1, R 3, L 4,
     R 3, L 2, R 5, R 190, R 3, R 5, L 5, L 1, R 54, L 3, L 4, L 1, R 4, R 1,
     R 3, L 1, L 1, R 2, L 2, R 2, R 5, L 3, R 4, R 76, L 3, R 4, R 191, R 5,
     R 5, L 5, L 4, L 5, L 3, R 1, R 3, R 2, L 2, L 2, L 4, L 5, L 4, R 5, R 4,
     R 4, R 2, R 3, R 4, L 3, L 2, R 5, R 3, L 2, L 1, R 2, L 3, R 2, L 1, L 1,
     R 1, L 3, R 5, L 5, L 1, L 2, R 5, R 3, L 3, R 3, R 5, R 2, R 5, R 5, L 5,
     L 5, R 2, L 3, L 5, L 2, L 1, R 2, R 2, L 2, R 2, L 3, L 2, R 3, L 5, R 4,
     L 4, L 5, R 3, L 4, R 1, R 3, R 2, R 4, L 2, L 3, R 2, L 5, R 5, R 4, L 2,
     R 4, L 1, L 3, L 1, L 3, R 1, R 2, R 1, L 5, R 5, R 3, L 3, L 3, L 2, R 4,
     R 2, L 5, L 1, L 1, L 5, L 4, L 1, L 1, R 1]

-- | coordinates on street grid of the city
type Coordinates = (Int, Int)

-- | position on street grid of the city: coordinates + orientation (heading),
-- | possible orientations are 0° (east), 90° (north), 180° (west), 270° (south)
type Position = (Coordinates, Int)

-- | utility sine and cosine functions that take angle in degrees instead of
-- | radians, and also work with integers instead of floats
sin' :: Int -> Int
sin' x = round $ sin $ fromIntegral x * pi / 180

cos' :: Int -> Int
cos' x = round $ cos $ fromIntegral x * pi / 180

-- | returns the position reached by turning the given number of degrees and
-- | then walking the given number of blocks starting from the given position
turnAndMove :: Int -> Int -> Position -> Position
turnAndMove d b ((x, y), o) =
    let
        o' = (o + d) `mod` 360 -- new orientation
        x' = x + b * cos' o' -- new x coordinate
        y' = y + b * sin' o' -- new y coordinate
    in
        ((x', y'), o')

-- | returns the position reached by following the given instruction starting
-- | from the given position
followInstruction :: Instruction -> Position -> Position
followInstruction i p =
    case i of
        L b -> turnAndMove   90  b p
        R b -> turnAndMove (-90) b p

-- | returns the position reached by following the given sequence of
-- | instructions starting from the given position
followInstructions :: [Instruction] -> Position -> Position
followInstructions is = foldr1 (flip (.)) (map followInstruction is)

-- | returns distance between the given coordinates on the street grid
distance :: Coordinates -> Coordinates -> Int
distance (x, y) (x', y') = abs (x - x') + abs (y - y')

-- | solution to part one of the puzzle
solution1 =
    let
        -- initial position (zero coordinates, facing north)
        p = ((0, 0), 90)
        -- coordinates of Easter Bunny HQ
        c = fst $ followInstructions instructions p
    in
        distance (fst p) c

-- | returns a list of coordinates travelled when walking from the first given
-- | coordinates to the second ones, not including the final coordinates; the
-- | coordinates must lie on a straight line on the street grid
coordinatesBetween :: Coordinates -> Coordinates -> [Coordinates]
coordinatesBetween (x1, y1) (x2, y2) =
    if
        x1 == x2
    then
        init [(x1, y) | y <- [y1, y1 + if y1 < y2 then 1 else -1 .. y2]]
    else if
        y1 == y2
    then
        init [(x, y1) | x <- [x1, x1 + if x1 < x2 then 1 else -1 .. x2]]
    else
        error "Given coordinates are not on a straight line"

-- | produces a Writer that, when run, returns the position reached by following
-- | the given instruction starting from the given position, and writes list of
-- | coordinates travelled along the way (not including the final ones)
followInstruction' :: Instruction -> Position ->
                      Writer [Coordinates] Position
followInstruction' i p =
    let
        p' = followInstruction i p
    in
        writer (p', coordinatesBetween (fst p) (fst p'))

-- | produces a Writer that, when run, returns the position reached by following
-- | the given sequence of instructions starting from the given position, and
-- | writes list of coordinates travelled along the way (not including the final
-- | ones)
followInstructions' :: [Instruction] -> Position ->
                       Writer [Coordinates] Position
followInstructions' is = foldr1 (>=>) (map followInstruction' is)

-- | returns the first duplicate coordinates in the given list
firstDuplicate :: [Coordinates] -> Coordinates
firstDuplicate []     = error "No duplicate coordinates in given list"
firstDuplicate (x:xs) = if x `elem` xs then x else firstDuplicate xs

-- | solution to part two of the puzzle
solution2 =
    let
        -- initial position (zero coordinates, facing north)
        p = ((0, 0), 90)
        -- final position and list of travelled coordinates (except final ones)
        (p', cs) = runWriter $ followInstructions' instructions p
        -- all travelled coordinates
        cs' = cs ++ [fst p']
        -- coordinates of Easter Bunny HQ
        c = firstDuplicate cs'
    in
        distance (fst p) c
