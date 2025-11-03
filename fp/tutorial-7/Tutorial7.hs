module Tutorial7 where

import LSystem
import Test.QuickCheck
import Data.List
import Data.Foldable (Foldable(fold))

pathExample = Go 30 :#: Turn 120 :#: Go 30 :#: Turn 120 :#: Go 30

-- 1a. copy
copy :: Int -> Command -> Command
copy 0 command = Sit
copy n command = command :#: copy (n-1) command

-- 1b. polygon
polygon :: Distance -> Int -> Command
polygon d n = copy n (Go d :#: Turn extAngle)
            where
                extAngle :: Float
                extAngle = 360 / fromIntegral(n)

-- 2. snowflake
snowflake :: Int -> Command
snowflake x = f x :#: n :#: n :#: f x :#: n :#: n :#: f x :#: n :#: n
        where
            f 0 = GrabPen red :#: Go 10
            f x = f (x-1) :#: p :#: f (x-1) :#: n :#: n :#: f (x-1) :#: p :#: f (x-1)
            p = Turn (60)
            n = Turn (-60)

-- 3. sierpinski
{-
angle: 60
start: f
rewrite: f → g+f+g
g → f-g-f
-}
sierpinski :: Int -> Command
sierpinski x = f x
            where
                f 0 = Go 10
                f x = g (x-1) :#: p :#: f (x-1) :#: p :#: g (x-1)
                g 0 = Go 10
                g x = f (x-1) :#: n :#: g (x-1) :#: n :#: f (x-1)
                p = Turn 60
                n = Turn (-60)
     
-- 4. hilbert
hilbert :: Int -> Command
hilbert =  undefined

-- 5. dragon
dragon :: Int -> Command
dragon =  undefined

-- ** Optional Material

-- 6a. split
split :: Command -> [Command]
split (firstCommand :#: restCommand) = split firstCommand ++ split restCommand
split Sit = []
split command = [command]

-- 6b. join
join :: [Command] -> Command
join = foldr1 (:#:)

-- 6c. equivalent
equivalent :: Command -> Command -> Bool
equivalent command1 command2 = split command1 == split command2

-- 6d. testing join and split
prop_split_join :: Command -> Bool
prop_split_join c = join (split c) == c

prop_split :: Command -> Bool
prop_split command = not (":#:" `isInfixOf` show (split command) || "Sit" `isInfixOf` show  (split command))

-- 7. optimise
optimise :: Command -> Command
optimise command = join (simplifyCommands [c | c <- (split command), c /= (Go 0), c /= (Turn 0)])

simplifyCommands :: [Command] -> [Command]
simplifyCommands [] = []
simplifyCommands ((Go n):(Go m):rest) = simplifyCommands (Go (n+m) : rest)
simplifyCommands ((Turn n):(Turn m):rest) = simplifyCommands (Turn (n+m) : rest)
simplifyCommands ((Go 0):rest) = rest
simplifyCommands ((Turn 0):rest) = rest
simplifyCommands (command:rest) = command : simplifyCommands rest

testCommandLong :: Command
testCommandLong =
    Go 0 :#: Turn 0 :#: Go 5 :#: Go 10 :#: Turn 90 :#: Turn 0 :#: Sit :#:
    Go 0 :#: Turn 45 :#: Go 0 :#: Go 7 :#: Turn 15 :#: Turn 15 :#: Sit :#:
    Go 3 :#: Go 2 :#: Go 0 :#: Turn 0 :#: Turn 30 :#: Turn 30 :#: Sit :#:
    Go 0 :#: Turn 0 :#: Go 8 :#: Turn 60 :#: Go 0 :#: Sit :#:
    Go 1 :#: Go 2 :#: Turn 0 :#: Turn 0 :#: Go 0 :#: Turn 0 :#: Sit