module Tutorial10 where

import Test.QuickCheck
import Control.Monad
import Data.Char
-- Question 1

-- 1a

ok :: String -> Bool
ok string = (length string < 6 ) && all isAsciiLower string

-- 1b

f :: [String] -> String
f strings = snd (minimum okStringsSafe)
        where
            okStrings = [(length string,string) | string <- strings, ok string]
            okStringsSafe
                    | okStrings == [] = [(5,"zzzzz")]
                    | otherwise = okStrings

-- 1c
g :: [String] -> String
g [] = "zzzzz"
g (x:xs)
    | not (ok x) = g xs
    | otherwise = shorter x (g xs)
    where
        shorter :: String -> String -> String
        shorter s1 s2
            | s2 == "zzzzz" = s1
            | length s1 <= length s2 = s1
            | otherwise = s2
-- 1d

h :: [String] -> String
h string
    | length (filter ok string) == 0 = "zzzzz"
    | otherwise = minimum $ filter ok string

checkThem ::  [String] -> Bool
checkThem strings = f strings == g strings && g strings == h strings
-- Question 2

-- 2a

i :: [a] -> [a] -> [a]
i = undefined

-- 2b

j :: [[a]] -> [[a]]
j = undefined

-- 2c

k :: [[a]] -> [[a]]
k = undefined

-- Question 3

data Prop = X
          | Y
          | T
          | F
          | Not Prop
          | Prop :&&: Prop
          | Prop :||: Prop
          | Prop :->: Prop
  deriving (Eq, Show)

instance Arbitrary Prop where
  arbitrary = sized gen
    where
    gen 0 =
      oneof [ return X,
              return Y,
              return T,
              return F ]
    gen n | n>0 =
      oneof [ return X,
              return Y,
              return T,
              return F,
              liftM Not prop,
              liftM2 (:&&:) prop prop,
              liftM2 (:||:) prop prop,
              liftM2 (:->:) prop prop]
      where
      prop = gen (n `div` 2)

-- 3a

eval :: Bool -> Bool -> Prop -> Bool
eval = undefined

-- 3b

simple :: Prop -> Bool
simple = undefined

-- 3c

simplify :: Prop -> Prop
simplify = undefined
