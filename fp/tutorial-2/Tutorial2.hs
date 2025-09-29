module Tutorial2 where

import Data.Char
import Data.List
import Test.QuickCheck

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
score x  = undefined

totalScore :: String -> Int
totalScore xs = undefined

prop_totalScore_pos :: String -> Bool
prop_totalScore_pos xs = undefined


-- ** Optional Material

-- 5. crosswordFind

crosswordFind :: Char -> Int -> Int -> [String] -> [String]
crosswordFind letter pos len words = undefined


-- 6. search

search :: String -> Char -> [Int]
search str goal = undefined

-- Depending on the property you want to test, you might want to change the type signature
prop_search :: String -> Char -> Bool
prop_search str goal = undefined

