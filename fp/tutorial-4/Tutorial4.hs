module Tutorial4 where

import Data.Char
import Data.List
import Test.QuickCheck


-- 1. doubles
-- a.
doublesComp :: [Int] -> [Int]
doublesComp xs =  [x*2 | x <- xs]

-- b.
doublesRec :: [Int] -> [Int]
doublesRec [] = []
doublesRec (firstx:xs) = firstx * 2 : doublesRec xs
-- c.
doublesHO :: [Int] -> [Int]
doublesHO xs = map double xs
    where
        double n = n * 2

-- d.
prop_doubles :: [Int] -> Bool
prop_doubles xs =  (doublesComp xs == doublesRec xs) && (doublesRec xs == doublesHO xs)

-- 2. aboves
-- a.
abovesComp :: Int -> [Int] -> [Int]
abovesComp n xs =  [x | x <- xs, x > n]

-- b.
abovesRec :: Int -> [Int] -> [Int]
abovesRec _ [] = []
abovesRec n (headx:restxs)
        | headx > n = headx:abovesRec n restxs
        | otherwise = abovesRec n restxs

-- c.
abovesHO :: Int -> [Int] -> [Int]
abovesHO n xs =  filter (aboven n) xs
        where
            aboven limit x = x > limit

-- d.
prop_aboves :: Int -> [Int] -> Bool
prop_aboves n xs = (abovesComp n xs == abovesRec n xs) && (abovesRec n xs == abovesHO n xs)

-- 3. parity
-- a.
xor :: Bool -> Bool -> Bool
xor a b = a && not b || not a && b

-- b.
parityRec :: [Bool] -> Bool
parityRec [] = True
parityRec (firstBool:restBool) = firstBool `xor` parityRec restBool

-- c.
parityHO :: [Bool] -> Bool
-- In general we should use foldr rather than fold. This is because it works better with Haskell's lazy evalution and can work with infinite lists
parityHO bools =  foldr xor True bools

-- d.
prop_parity :: [Bool] -> Bool
prop_parity bools = parityRec bools == parityHO bools

-- 4. allcaps
-- a.
allcapsComp :: String -> Bool
allcapsComp inputStr =  and [isAsciiUpper x | x <- inputStr, isLetter x]

-- b.
allcapsRec :: String -> Bool
allcapsRec "" = True
allcapsRec (firstChar:restInput)
                                | not (isLetter firstChar) = True && allcapsRec restInput
                                | otherwise = isAsciiUpper firstChar && allcapsRec restInput

-- c.
allcapsHO :: String -> Bool
allcapsHO inputStr =  foldr (&&) True (map isAsciiUpper (filter isLetter inputStr))

-- d.
prop_allcaps :: String -> Bool
prop_allcaps inputStr =  allcapsComp inputStr == allcapsRec inputStr && allcapsRec inputStr == allcapsHO inputStr


-- ** Optional material
-- Matrix manipulation

type Matrix = [[Rational]]

-- 5
-- a.
uniform :: [Int] -> Bool
uniform [] = True
uniform nums = all (== head nums) nums

-- b.
valid :: Matrix -> Bool
valid rows = uniform [length row | row <- rows] && length rows > 0 && and [length row > 0 | row <- rows]


-- 6.
width :: Matrix -> Int
width matrix = length (head matrix)

height :: Matrix -> Int
height matrix = length matrix

plusM :: Matrix -> Matrix -> Matrix
plusM matA matB
            | not (valid matA) || not (valid matB) = error "Matrix is not valid"
            | height matA /= height matB || width matA /= width matB = error "Matrix is not valid"
            | otherwise = zipWith plusRow matA matB
    where
        plusRow :: [Rational] -> [Rational] -> [Rational]
        plusRow [] [] = []
        plusRow (firstElemA:restElemA) (firstElemB:restElemB) = firstElemA + firstElemB : plusRow restElemA restElemB

-- 7.
-- If matA has width m and height n, and matB has width o and height p then the dot product will be a matrix with width p and height m BUT only if n=o
timesM :: Matrix -> Matrix -> Matrix
timesM matA matB
            | not (valid matA) || not (valid matB) = error "Matrix is not valid"
            | width matA /= height matB = error "Not conformable under multiplication"
            | otherwise = [map (dot row) (transpose matB)| row <- matA]
        where
            dot :: [Rational] -> [Rational] -> Rational
            dot vecA vecB = sum(zipWith (*) vecA vecB)

-- ** Challenge

-- 8.
-- Mapping functions
mapMatrix :: (a -> b) -> [[a]] -> [[b]]
mapMatrix f = undefined

zipMatrix :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
zipMatrix f = undefined

-- All ways of deleting a single element from a list
removes :: [a] -> [[a]]
removes = undefined

-- Produce a matrix of minors from a given matrix
minors :: Matrix -> [[Matrix]]
minors m = undefined

-- A matrix where element a_ij = (-1)^(i + j)
signMatrix :: Int -> Int -> Matrix
signMatrix w h = undefined

determinant :: Matrix -> Rational
determinant = undefined

cofactors :: Matrix -> Matrix
cofactors m = undefined

scaleMatrix :: Rational -> Matrix -> Matrix
scaleMatrix k = undefined

inverse :: Matrix -> Matrix
inverse m = undefined

-- Tests
identity :: Int -> Matrix
identity n = undefined

prop_inverse1 :: Rational -> Property
prop_inverse1 a = undefined

prop_inverse2 :: Rational -> Rational -> Rational
                -> Rational -> Property
prop_inverse2 a b c d = undefined

type Triple a = (a,a,a)

prop_inverse3 :: Triple Rational ->
                 Triple Rational ->
                 Triple Rational ->
                 Property
prop_inverse3 r1 r2 r3 = undefined