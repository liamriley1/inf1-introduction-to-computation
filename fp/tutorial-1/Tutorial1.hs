module Tutorial1 where

import PicturesSVG -- needed for the optional chess part
import Test.QuickCheck
import GHC.Float (Floating(sqrt))
import GHC.Base (Float)
import Data.List (repeat)


-- 2.
double :: Int -> Int
double x = x + x

square :: Int -> Int
square x = x ^ 2

-- 3.
isTriple :: Int -> Int -> Int -> Bool
isTriple a b c = (a^2 + b^2) == c^2

-- 4.
leg1 :: Int -> Int -> Int
leg1 x y = x^2 - y^2

leg2 :: Int -> Int -> Int
leg2 x y = 2 * x * y

hyp :: Int -> Int -> Int
hyp x y = x^2 + y^2

-- 5.
prop_triple :: Int -> Int -> Bool
prop_triple x y = isTriple (leg1 x y) (leg2 x y) (hyp x y)

-- 8.
pic1 :: Picture
pic1 = above (beside knight (invert knight)) (beside (invert knight) knight)

pic2 :: Picture
pic2 = above (beside knight (invert knight)) (flipV (beside knight (invert knight)))

-- ** Functions

twoBeside :: Picture -> Picture
twoBeside x = beside x (invert x)

-- 9.
twoAbove :: Picture -> Picture
twoAbove x = above x (invert x)

fourPictures :: Picture -> Picture
fourPictures x = twoBeside (twoAbove x)

-- 10.
-- a)
otherEmptyRow :: Picture
otherEmptyRow = repeatH 4 (twoBeside blackSquare)

-- b)
emptyRow :: Picture
emptyRow = invert otherEmptyRow

-- c)
middleBoard :: Picture
middleBoard = repeatV 2 (above emptyRow otherEmptyRow)

-- d)
whiteTopLine :: Picture
whiteTopLine = beside (beside (beside (beside (beside (beside (beside rook knight) bishop) queen) king) bishop) knight) rook
whiteRow :: Picture
whiteRow = over whiteTopLine otherEmptyRow
whitePawns :: Picture
whitePawns = over (repeatH 8 pawn) emptyRow

blackTopLine :: Picture
blackTopLine = invert whiteTopLine
blackRow :: Picture
blackRow = over blackTopLine emptyRow
blackPawns = over (repeatH 8 (invert pawn)) otherEmptyRow

-- e)
populatedBoard :: Picture
populatedBoard = above blackRow (above blackPawns (above middleBoard (above whitePawns whiteRow)))
