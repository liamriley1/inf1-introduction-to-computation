module Project ( prove, proveCount ) where

import Sequent

-- Added imports
import Data.List
import Data.Foldable (concatMap)
import Data.Functor.Contravariant (Predicate(Predicate))
--import Data.Maybe (fromJust)


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



notLeft :: SequentRule
notLeft (ants :|=: sucs)
            | length removedAnts == 0 = Nothing
            | otherwise = Just [(ants \\ removedAnts) :|=: sucs ++ addedSucs]
        where
            removedAnts :: [Prop]
            removedAnts = [pickedAnt | pickedAnt <- ants, removableAnt pickedAnt]
            removableAnt :: Prop -> Bool
            removableAnt (Not _) = True
            removableAnt _ = False
            addedSucs :: [Prop]
            addedSucs = map (\(Not pred) -> pred) removedAnts


notRight :: SequentRule
notRight (ants :|=: sucs)
            | length removedSucs == 0 = Nothing
            | otherwise = Just [(ants ++ addedAnts)  :|=: (sucs \\ removedSucs)]
        where
            removedSucs :: [Prop]
            removedSucs = [pickedSuc | pickedSuc <- sucs, removableSuc pickedSuc]
            removableSuc :: Prop -> Bool
            removableSuc (Not _) = True
            removableSuc _ = False
            addedAnts :: [Prop]
            addedAnts = map (\(Not pred) -> pred) removedSucs


orLeft :: SequentRule
orLeft (ants :|=: sucs)
            | length simplifiableAnts == 0 = Nothing
            | otherwise = Just [(orAnt:remainingAnts :|=: sucs) | orAnt <- orAnts ]
        where
            simplifiableAnts :: [Prop]
            simplifiableAnts = [pickedAnt | pickedAnt <- ants, simplifiableAnt pickedAnt]
            simplifiableAnt :: Prop -> Bool
            simplifiableAnt (_ :||: _) = True
            simplifiableAnt _ = False
            separateAnt :: Prop -> [Prop]
            separateAnt (before :||: after) = [before,after]
            orAnts = concatMap separateAnt simplifiableAnts
            remainingAnts = ants \\ simplifiableAnts

andRight :: SequentRule
andRight (ants :|=: sucs)
            | length simplifiableSucs == 0 = Nothing
            | otherwise = Just [(ants :|=: andSuc:remainingSucs) | andSuc <- andSucs]
        where
            simplifiableSucs :: [Prop]
            simplifiableSucs = [pickedSuc | pickedSuc <- sucs, simplifiableSuc pickedSuc]
            simplifiableSuc :: Prop -> Bool
            simplifiableSuc (_ :&&: _) = True
            simplifiableSuc _ = False
            separateSuc :: Prop -> [Prop]
            separateSuc (before :&&: after) = [before,after]
            andSucs = concatMap separateSuc simplifiableSucs
            remainingSucs = sucs \\ simplifiableSucs

-- for challenge part

proveCount :: Sequent -> ([Sequent],Int)
proveCount = undefined
