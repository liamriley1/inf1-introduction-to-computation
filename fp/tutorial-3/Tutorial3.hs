module Tutorial3 where

import Data.Char
import Data.List
import Test.QuickCheck

-- These are some helper functions for makeKey and makeKey itself.
-- Exercises continue below.

rotate :: Int -> [Char] -> [Char]
rotate k list | 0 <= k && k <= length list = drop k list ++ take k list
              | otherwise = error "Argument to rotate too large or too small"

--  prop_rotate rotates a list of lenght l first an arbitrary number m times,
--  and then rotates it l-m times; together (m + l - m = l) it rotates it all
--  the way round, back to the original list
--
--  from a random number k we use k `mod` l (but then l can't be 0,
--  since you can't divide by 0)

prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
                        where l = length str
                              m = if l == 0 then 0 else k `mod` l

alphabet = ['A'..'Z']

makeKey :: Int -> [(Char, Char)]
makeKey k = zip alphabet (rotate k alphabet)

-- ** Caesar Cipher Exercises

-- 1.
lookUp :: Char -> [(Char, Char)] -> Char
lookUp char cipherKey
           | length matchingPairs == 0 = char
           | otherwise = head matchingPairs
           where
                 matchingPairs = [b | (a,b) <- cipherKey, a == char]


-- Earlier, messy way that relied on an arbitrary argument of 0 to be passed at the start
{-
lookUpRec :: Int -> Char -> [(Char, Char)] -> Char
lookUpRec pos char cipherKey | firstOfTuple (cipherKey !! pos) == char  = lastOfTuple (cipherKey !! pos)
                             | otherwise = lookUpRec (pos+1) char cipherKey
                             where
                                firstOfTuple :: (a,b) -> a
                                firstOfTuple (x,_) = x

                                lastOfTuple :: (a,b) -> b
                                lastOfTuple (_,x) = x
-}

-- This way is cooler - can just use pattern patching to recurse through the list!
lookUpRec :: Char -> [(Char, Char)] -> Char

-- If the function is run on the empty list, we have not found any matches, therefore return the original char.
lookUpRec char [] = char
lookUpRec char ((key,value):rest)
        | char == key = value
        | otherwise = lookUpRec char rest

prop_lookUp :: Char -> [(Char, Char)] -> Bool
prop_lookUp char keys = lookUp char keys == lookUpRec char keys

-- 2.
encipher :: Int -> Char -> Char
encipher shift char = lookUpRec char (makeKey shift)

-- 3.
normalise :: String -> String

normalise input = [toUpper char | char <- input, isLetter char]

normaliseRec :: String -> String
normaliseRec "" = ""
normaliseRec (firstChar:rest)
                        | isLetter firstChar = toUpper firstChar : normaliseRec rest
                        | otherwise = normaliseRec rest

prop_normalise :: String -> Bool
prop_normalise input = normalise input == normaliseRec input

-- 4.
enciphers :: Int -> String -> String
enciphers shift cleartext= [encipher shift char | char <- (normalise cleartext)]
-- 5.
reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey keys= [(y,x) | (x,y) <- keys]

reverseKeyRec :: [(Char, Char)] -> [(Char, Char)]
reverseKeyRec [] = []
reverseKeyRec ((x,y):restKeys) = (y,x) : reverseKeyRec restKeys

prop_reverseKey :: [(Char, Char)] -> Bool
prop_reverseKey keys = reverseKey keys == reverseKeyRec keys

-- 6.
decipher :: Int -> Char -> Char
decipher shift char = head [y | (x,y) <- reverseKey (makeKey shift), x == char]

decipherStr :: Int -> String -> String
decipherStr shift cipherText = [decipher shift char | char <- cipherText, isAsciiUpper char]


-- ** Optional Material

-- 7.
candidates :: String -> [(Int, String)]
candidates cipherText = [(tryShift, (tryDecrypt tryShift))  | tryShift <- [1..26], "THE" `isInfixOf` (tryDecrypt tryShift) || "AND" `isInfixOf` (tryDecrypt tryShift)]
                    where
                        tryDecrypt :: Int -> String
                        tryDecrypt shift = decipherStr shift cipherText


splitEachFive :: String -> [String]
splitEachFive xs | length xs > 5 = take 5 xs : splitEachFive (drop 5 xs)
                 | otherwise     = [ fillToFive xs ]

fillToFive :: String -> String
fillToFive xs = xs ++ replicate (5 - length xs) 'X'

-- An alternative solution demonstrating 'repeat'
fillToFive' :: String -> String
fillToFive' xs = take 5 (xs ++ repeat 'X')

-- The following example shows why 'transpose' is not
--  invertible in general. The transpose function
--  takes the 'columns' of a list of lists, and makes
--  them the 'rows' of a new list of lists.
--
-- [[o n e],           [[o t t f f],       [[o n e e e],
--  [t w o],            [n w h o i],        [t w o r],
--  [t h r e e],   -->  [e o r u v],   -->  [t h r e],
--  [f o u r],          [e r e],            [f o u],
--  [f i v e]   ]       [e],        ]       [f i v]     ]

-- 8.
encrypt :: Int -> String -> String
encrypt shift cleartext = concat (transpose [fillToFive aFive | aFive <- splitEachFive (enciphers shift cleartext)])

-- 9.
decrypt :: Int -> String -> String
decrypt shift ciphertext = decipherStr shift (stripTrailingX (concat (transpose (splitEachN ciphertext ((length ciphertext) `div` 5)))))
                    where
                        splitEachN :: String ->  Int -> [String]
                        splitEachN [] _ = []
                        splitEachN string n = take n string : splitEachN (drop n string) n

                        -- Slightly hacky function. We revese the string, if we can pattern match an X to the beginning of the reversed string, remove the X, reverse, and call again
                        stripTrailingX :: String -> String
                        stripTrailingX string
                                            | 'X':restOfString <- reverse string = stripTrailingX (reverse restOfString)
                                            | otherwise = string