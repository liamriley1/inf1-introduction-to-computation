module Tutorial2 where

import Data.Char
import Data.List
import Test.QuickCheck
import GHC.Real (FractionalExponentBase(Base10, Base2))

-- 1. inRange

inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = [ x | x <- xs , x > lo && x < hi ]

-- 2. multDigits

multDigits :: String -> Int
multDigits str = product [ getNum char | char <- str]
    where
        getNum char
            | isDigit char = digitToInt char
            | otherwise = 1

countDigits :: String -> Int
countDigits str = length [ num | num <- str, isDigit num]

prop_multDigits :: String -> Bool
prop_multDigits str = 9 ^ (countDigits str) >= multDigits str


-- 3. capitalise and title

capitalise :: String -> String
capitalise [] = []
capitalise (firstLetter:restString) = toUpper firstLetter : [toLower char | char <- restString]

title :: [String] -> [String]
title words = [capitalise word | word <- words]

-- 4. score and totalScore

score :: Char -> Int
score char
    | not (isLetter char) = 0
    | char `elem` "AEIOU" = 3
    | char `elem` "aeiou" = 2
    | isUpper char = 2
    | otherwise = 1

totalScore :: String -> Int
totalScore string = product [score char | char <- string, score char /= 0]

prop_totalScore :: String -> Bool
prop_totalScore string = totalScore string >= 1

prop_totalScore_pos :: String -> Bool
prop_totalScore_pos xs = undefined


-- ** Optional Material

-- 5. crosswordFind

crosswordFind :: Char -> Int -> Int -> [String] -> [String]
crosswordFind letter pos len words = [word | word <- words, length word == len && word !! pos == letter]


-- 6. search

search :: String -> Char -> [Int]
search str goal = [pos | (char, pos) <- indexedStr, char == goal]
    where
        indexedStr = zip str [0..]

-- Depending on the property you want to test, you might want to change the type signature
prop_search :: String -> Char -> Bool
prop_search str goal = and [(str !! index) == goal | index <- search str goal]

