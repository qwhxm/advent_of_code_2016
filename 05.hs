-- * adventofcode.com/2016/day/5
-- NOTE requires the packages "cryptohash" and "base16-bytestring"
import Crypto.Hash.MD5
import Data.ByteString.Base16
import Data.ByteString.Char8 (pack, unpack)
import Data.Char
import Data.Function
import Data.List

-- | door ID given as puzzle input
doorId :: String
doorId = "cxdnnyjw"

-- | calculates password for the given door ID
password :: String -> String
password id =
    let
        -- inputs to hash
        is = map (\n -> id ++ show n) [0 ..]
        -- hashes of the inputs, in hexadecimal
        hs = map (unpack . encode . hash . pack) is
        -- hashes that start with five zeros
        hs' = filter (isPrefixOf "00000") hs
    in
        take 8 $ map (!! 5) hs'

-- | solution to part one of the puzzle
solution1 = password doorId

-- | calculates password for the given door ID, according to the "slightly more
-- | inspired security mechanism"
password' :: String -> String
password' id =
    let
        -- inputs to hash
        is = map (\n -> id ++ show n) [0 ..]
        -- hashes of the inputs, in hexadecimal
        hs = map (unpack . encode . hash . pack) is
        -- hashes that start with five zeros ("interesting" hashes)
        hs' = filter (isPrefixOf "00000") hs
        -- predicate to check if an "interesting" hash specifies valid position
        valid h =
            let
                p = h !! 5
            in
                isDigit p && digitToInt p < 8
        -- "interesting" hashes that specify a valid position
        hs'' = filter valid hs'
        -- valid (position, character) pairs specified by "interesting" hashes
        cps = map (\h -> (digitToInt (h !! 5), h !! 6)) hs''
        -- list of only the first (position, character) pair for each position
        cps' = take 8 $ nubBy ((==) `on` fst) cps
    in
        map snd $ sort cps'

-- | solution to part two of the puzzle
solution2 = password' doorId
