-- VOORNAAM NAAM
-- R-NUMMER
-- RICHTING
module CountryCodes where

import Data.Bits (setBit, testBit)
import Data.Char (ord, chr)
import Data.Word (Word32)

encodedCountryCodes :: [Word32]
encodedCountryCodes
  = [48191864, 57637883, 66223597, 33576464, 917716, 153344, 23050747, 1717248,
     1013784, 53264, 54702544, 20841735, 67108093, 34785653, 4096, 21904625, 1,
     5521424, 61767647, 40533740, 50597953, 1057109, 262176, 0, 524304, 4198401]


-- Opdracht 1
decodeRow :: Word32 -> [Char]
decodeRow w = [l | (l,i) <- zip ['A'..'Z'] [0..25], testBit w i]

decode :: [Word32] -> [String]
decode list = [l : s : [] | (l,i) <- zip ['A'..'Z'] list, s <- decodeRow i]


-- Opdracht 2
fillInGaps :: Eq ref => [a] -> [ref] -> (a -> ref) -> (ref -> a) -> [a]
fillInGaps l [] _ _ = []
fillInGaps [] r _ rl = map rl r
fillInGaps (l:ls) (r:rs) lr rl
    | lr l == r = l : fillInGaps ls rs lr rl
    | otherwise = rl r : fillInGaps (l:ls) rs lr rl

encodeChars :: [Char] -> Word32
encodeChars [] = 0
encodeChars list = foldl setBit 0 [v | e <- list, (l,v) <- zip ['A'..'Z'] [0..], e == l]

encodeRow :: [String] -> Word32
encodeRow list = encodeChars [e | (_:e:[]) <- list] 

encode :: [String] -> [Word32]
encode list = map encodeRow [filter (\(e1:_:[]) -> e1 == l) list | l <- ['A'..'Z']]


-- Opdracht 3
printCodes :: [String] -> IO ()
printCodes list = mapM_ putStrLn $ map (\v -> concat $ map (\x -> x ++ " ") v) (map (\v -> fillInGaps v ['A'..'Z'] (\(_:e:[]) -> e) (\x -> "..")) [filter (\(e1:_:[]) -> e1 == l) list | l <- ['A'..'Z']])
