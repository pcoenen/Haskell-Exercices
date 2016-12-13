-- VOORNAAM NAAM
-- R-NUMMER
-- RICHTING
module CountryCodes where

import Data.Bits (setBit, testBit)
import Data.Char (ord, chr)
import Data.Word (Word32)
import Data.List

encodedCountryCodes :: [Word32]
encodedCountryCodes
  = [48191864, 57637883, 66223597, 33576464, 917716, 153344, 23050747, 1717248,
     1013784, 53264, 54702544, 20841735, 67108093, 34785653, 4096, 21904625, 1,
     5521424, 61767647, 40533740, 50597953, 1057109, 262176, 0, 524304, 4198401]


-- Opdracht 1
abc :: [Char]
abc = ['A'..'Z']
decodeRow :: Word32 -> [Char]
decodeRow x = check x 0

check :: Word32 -> Int -> [Char]
check _ 26 = []
check x n
    | testBit x n   = (abc !! n) : check x (n+1)
    | otherwise     = check x (n+1)
    

decode :: [Word32] -> [String]
decode = decodeHelp 0

decodeHelp :: Int -> [Word32] -> [String]
decodeHelp 26 _ = []
decodeHelp _ [] = []
decodeHelp i (x:xs) = (combine (abc !! i) (decodeRow x)) ++ decodeHelp (i+1) xs

combine :: Char -> String -> [String]
combine _ [] = []
combine c (x:xs) = ([c] ++ [x]) : (combine c xs)


-- Opdracht 2
fillInGaps :: Eq ref => [a] -> [ref] -> (a -> ref) -> (ref -> a) -> [a]
fillInGaps [] [] ar ra = []
fillInGaps (a:as) [] ar ra = []
fillInGaps [] (r:rs) ar ra = (ra r) : fillInGaps [] rs ar ra
fillInGaps (a:as) (r:rs) ar ra
    | ar a == r = a : fillInGaps as rs ar ra
    | otherwise = (ra r) : fillInGaps (a:as) rs ar ra

helpABC :: [(Char,Int)]
helpABC = [(abc !! i,i) | i <- [0..25]]

encodeChars :: [Char] -> Word32
encodeChars [] = 0
encodeChars xs = sum $ map (\x -> 2^x) $ map (\x -> getIndex x abc 0) xs

getIndex _ [] _ = -1
getIndex v (x:xs) i
    | v == x    = i
    | otherwise = getIndex v xs (i+1)
  

encodeRow :: [String] -> Word32
encodeRow = encodeChars . map (\ [x,y] -> y) 

encode :: [String] -> [Word32]
encode (i:is) = map encodeRow $ fillInGaps (split ((\[a,b] -> a)i) is [i]) ['A'..'Z'] (\([a,_]:_) -> a) (\x -> [])



split :: Char -> [String] -> [String] -> [[String]]
split c [] a = [a]
split c (x:xs) a
    |(\[a,_] -> a) x == c   = split c xs (x:a)
    | otherwise             = a : split ((\[a,_] -> a)x) xs [x]


-- Opdracht 3
printCodes :: [String] -> IO ()

printCodes x = printHelp1 (prepare x)

prepare (i:is) = map (\x -> fillInGaps (sort x) ['A'..'Z'] (\[_,a] -> a) (\ _ -> ".."))(split ((\[a,b] -> a)i) is [i])

printHelp1 (x:xs) = do
            printStart x
            printHelp1 xs

printStart (x:xs) = do
            putStrLn (x ++ " ")
            printHelp xs
            
printHelp :: [String] -> IO ()
printHelp [] = return ()
printHelp (x:xs) = do
                putStr (x ++ " ")
                printHelp xs
