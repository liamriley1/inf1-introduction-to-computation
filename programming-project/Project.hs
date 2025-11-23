module Project ( prove, proveCount ) where

import Sequent

-- Added imports
import Data.List
import Data.Foldable (concatMap)
import GHC.Base (undefined)
import Data.Bool (Bool(True))

prove :: Sequent -> [Sequent]
prove = undefined


-- Defining the rules for the simplification below here

-- First, define a type for the 7 rules
-- If the rule does not simplify the sequent, we return Nothing rather than a trivial return, hopefully useful for later!
type SequentRule = Sequent -> Maybe [Sequent]

-- Then, the 7 rules are defined below

immediate :: SequentRule
immediate (ants :|=: sucs)
                    | or [ant `elem` sucs | ant <- ants] = Just []
                    | otherwise = Nothing

andLeft :: SequentRule
andLeft (ants :|=: sucs)
            | length simplifiableAnts == 0 = Nothing
            | otherwise = Just [((concatMap simplifyAnt simplifiableAnts) ++ (ants \\ simplifiableAnts) :|=: sucs)]
        where
            simplifiableAnts :: [Prop]
            simplifiableAnts = [pickedAnt | pickedAnt <- ants, simplifiableAnt pickedAnt]
            simplifiableAnt :: Prop -> Bool
            simplifiableAnt (_ :&&: _) = True
            simplifiableAnt _ = False
            simplifyAnt :: Prop -> [Prop]
            simplifyAnt (prop :&&: props) = [prop,props]


orRight :: SequentRule
orRight (ants :|=: sucs)
            | length simplifiableSucs == 0 = Nothing
            | otherwise = Just [(ants :|=: (concatMap simplifySuc simplifiableSucs) ++ (sucs \\ simplifiableSucs))]
        where
            simplifiableSucs :: [Prop]
            simplifiableSucs = [pickedSuc | pickedSuc <- sucs, simplifiableSuc pickedSuc]
            simplifiableSuc :: Prop -> Bool
            simplifiableSuc (_ :||: _) = True
            simplifiableSuc _ = False
            simplifySuc :: Prop -> [Prop]
            simplifySuc (prop :||: props) = [prop,props]











-- for challenge part

proveCount :: Sequent -> ([Sequent],Int)
proveCount = undefined
