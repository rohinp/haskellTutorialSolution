-- Informatics 1 - Functional Programming 
-- Tutorial 2

import Data.Char
import Data.List
import Test.QuickCheck

-- enconding a message

-- 1
rotate :: Int -> [Char] -> [Char]
rotate num xs | num > (length xs) = error "invalid data num > string length"
              | num < 0 = error "invalid data num less than 0"
              | num == 0 = xs
              | otherwise = rotate (num-1) ((tail xs) ++ [head xs])

-- 2
prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
                        where l = length str
                              m = if l == 0 then 0 else k `mod` l

-- 3
makeKey :: Int -> [(Char, Char)]
makeKey num = zip ['A'..'Z'] (rotate num ['A'..'Z'])

-- 4
lookUp :: Char -> [(Char, Char)] -> Char
lookUp c xs = if null v then c else head v
	where
		v = [ snd x | x <- xs, fst x == c]    

-- 5
lookUpRec :: Char -> [(Char, Char)] -> Char
lookUpRec c [] = c
lookUpRec c (x:xs) | fst x  == c = snd x
                   | otherwise   = lookUpRec c xs          

-- 5
prop_lookUp :: Char -> [(Char, Char)] -> Bool
prop_lookUp c xs = lookUp c xs == lookUpRec c xs

-- 6
encipher :: Int -> Char -> Char
encipher o c = lookUp c (makeKey o)

-- 7
normalize :: String -> String
normalize xs = [toUpper x | x <- xs, isDigit x || isAlpha x]

-- 8
encipherStr :: Int -> String -> String
encipherStr o msg = [encipherChar x | x <- normalize msg]
	where
		encipherChar = encipher o
 
 -- decoding a message
-- 9
reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey xs = [(snd x, fst x) | x <- xs]

-- 10
reverseKeyRec :: [(Char, Char)] -> [(Char, Char)]
reverseKeyRec [] = []
reverseKeyRec (x:xs) = (rev x):reverseKeyRec(xs)
	where
		rev y = (snd y, fst y)

-- 11
prop_reverseKey :: [(Char, Char)] -> Bool
prop_reverseKey xs = reverseKey xs == reverseKeyRec xs

-- 12
decipher :: Int -> Char -> Char
decipher o c = lookUp c (reverseKey (makeKey o))

-- 13
decipherStr :: Int -> String -> String
decipherStr o msg = [decipherChar c | c <- msg]
	where
		decipherChar = decipher o

-- breaking cryptography

-- 14
contains :: String -> String -> Bool
contains [] [] = False
contains str subStr | null str = False
			        | isZipEqSet str subStr  = True
        	        | otherwise = contains (tail str) subStr
	where
		isZipEqSet s1 s2 = length [x | x <- zip s1 s2, fst x == snd x] == length s2
		
-- 15
candidates :: String -> [(Int, String)]
candidates [] = []
candidates xs = [(i, decipherStr i xs) | i <- [1..26] , contains (decipherStr i xs) "THE" || contains (decipherStr i xs) "AND"]














