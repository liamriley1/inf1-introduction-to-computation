module Project ( prove, proveCount ) where

import Sequent

-- Added imports
import Data.List
import Data.Foldable (concatMap)


prove :: Sequent -> [Sequent]
prove sequent = proveRec [sequent]

-- This function applies all of the "non-branching" rules until the sequent no longer changes (max simplified for now)
simplify :: Sequent -> Sequent
simplify sequent =
        case andLeft sequent of
            Just [seq] -> simplify seq
            Nothing -> case orRight sequent of
                Just [seq] -> simplify seq
                Nothing -> case notLeft sequent of
                    Just [seq] -> simplify seq
                    Nothing -> case notRight sequent of
                        Just [seq] -> simplify seq
                        -- This is the only exit point, therefore we only need to dedupe here...
                        Nothing -> dedupe sequent

-- This function de-duplicates the lists of antecedents and succedents in a sequent
-- Note: we also need to remove duplicate sequents themselves, this will be handled elsewhere...
-- Therefore the lists of antecedents and succedents need to be ordered in order for this other step to work
dedupe (ants :|=: sucs) = (sort $ nub ants) :|=: (sort $ nub sucs)

-- This function tells us if a seqent follows the "immediate" rule
immediate :: Sequent -> Bool
immediate (ants :|=: sucs)
                    | or [ant `elem` sucs | ant <- ants] = True
                    | otherwise = False

proveRec :: [Sequent] -> [Sequent]
proveRec [] = []
proveRec (firstSequent:restSequents)
                    | immediate (simplify firstSequent) = proveRec restSequents
                    | otherwise =
                    case flow (simplify firstSequent) of
                        Nothing -> nub((simplify firstSequent):(proveRec restSequents))
                        Just newSequents -> nub(proveRec (newSequents ++ restSequents))

flow :: SequentRule
flow sequent =
        case orLeft sequent of
            Just sequents -> Just sequents
            Nothing -> case andRight sequent of
                Just sequents -> Just sequents
                Nothing -> Nothing


-- Defining the rules for the simplification below here

-- First, define a type for the sequent rules
-- If a rule does not simplify the sequent, we return Nothing rather than a trivial return, hopefully useful for later!
type SequentRule = Sequent -> Maybe [Sequent]

-- Then, the sequent rules are defined below

andLeft :: SequentRule
andLeft (ants :|=: sucs)
            | null simplifiableAnts = Nothing
            | otherwise = Just [(concatMap simplifyAnt simplifiableAnts) ++ (ants \\ simplifiableAnts) :|=: sucs]
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
            | null simplifiableSucs = Nothing
            | otherwise = Just [ants :|=: (concatMap simplifySuc simplifiableSucs) ++ (sucs \\ simplifiableSucs)]
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
            | null removedAnts = Nothing
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
            | null removedSucs = Nothing
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
            | null simplifiableAnts = Nothing
            | otherwise = Just [before:remainingAnts :|=: sucs, after:remainingAnts :|=: sucs]
        where
            simplifiableAnts :: [Prop]
            simplifiableAnts = [pickedAnt | pickedAnt <- ants, simplifiableAnt pickedAnt]
            (prop:rest) = simplifiableAnts
            simplifiableAnt :: Prop -> Bool
            simplifiableAnt (_ :||: _) = True
            simplifiableAnt _ = False
            (before :||: after) = prop
            remainingAnts = ants \\ [prop]

andRight :: SequentRule
andRight (ants :|=: sucs)
            | null simplifiableSucs = Nothing
            | otherwise = Just [ants :|=: before:remainingSucs, ants :|=: after:remainingSucs]
        where
            simplifiableSucs :: [Prop]
            simplifiableSucs = [pickedSuc | pickedSuc <- sucs, simplifiableSuc pickedSuc]
            (prop:rest) = simplifiableSucs
            simplifiableSuc :: Prop -> Bool
            simplifiableSuc (_ :&&: _) = True
            simplifiableSuc _ = False
            (before :&&: after)  = prop
            remainingSucs = sucs \\ [prop]

-- for challenge part

proveCount :: Sequent -> ([Sequent],Int)
proveCount = undefined

problem = [ (Var "f" :||: Var "a") :&&: Not (Var "f")
          , (Var "a" :||: Var "a") :&&: (Var "c" :||: Var "b")
          , Not (Var "a") :||: Not (Var "c")
          , Not (Var "d") :||: (Var "a" :||: Var "d")
          ]
          :|=:
          [ (Var "f" :&&: Var "d") :||: (Var "d" :||: Var "d")
          , (Var "c" :||: Var "e") :||: (Var "d" :||: Var "e")
          , (Var "b" :||: Var "a") :&&: (Var "f" :||: Var "e")
          ]

