-- Informatics 1 - Functional Programming
-- Class Test 2025

module ClassExam where

import Data.Char
import Test.QuickCheck
import GHC.Real (FractionalExponentBase(Base10))

-- Problem 1

-- a

f :: String -> Int
f str = sum [ord char | char <- str, isAlpha char]

-- b

g :: String -> Int
g "" = 0
g (firstChar:restString) = getCode firstChar + g restString
                where
                    getCode :: Char -> Int
                    getCode char
                            | not (isAlpha char) = 0
                            | otherwise = ord char

-- c

h :: String -> Int
h str = sum (map getCode str)
    where
        getCode :: Char -> Int
        getCode char
                | not (isAlpha char) = 0
                | otherwise = ord char

-- d

prop_fgh :: String -> Bool
prop_fgh str = f str == g str && g str == h str

-- Problem 2

-- a

c :: String -> String -> Bool
c strA strB = and [checkEq a b | (a,b) <- transformToTuples strA strB]
    where
        transformToTuples :: String -> String -> [(Char,Char)]
        transformToTuples strA strB = take (min (length strA) (length strB)) (zip strA strB)

        checkEq :: Char -> Char -> Bool
        checkEq charA charB
                       | not (isAlpha charA && isAlpha charB) = True
                       | otherwise = charA == charB

-- b

d :: String -> String -> Bool
d "" _ = True
d _ "" = True
d (firstCharA:restStrA) (firstCharB:restStrB) = (checkEq firstCharA firstCharB) && (d restStrA restStrB)
    where
        checkEq :: Char -> Char -> Bool
        checkEq charA charB
                       | not (isAlpha charA && isAlpha charB) = True
                       | otherwise = charA == charB

-- c

prop_cd :: String -> String -> Bool
prop_cd strA strB = (c strA strB) == (d strA strB)
