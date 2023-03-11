{-# OPTIONS_GHC -Wall #-}

module EL (
    Prop,
    Agent,
    World,
    EpiState,
    EpiFormula (..),
    epiSat,
    testEpiSat,
    update,
    testUpdate,
    testAll
    ) where

{-Prop – chaîne de caractères
Représente une proposition. Vous pouvez les écrire en toutes les lettres ou pas (par exemple :
"Le visage d’Alice est sale", "Alice-sale", "a-sale", "as").-}
type Prop = String

{-Agent – chaîne de caractères
Représente un agent (ex. : "alice", "A", "a")-}
type Agent = String

{-World – entier (Int)
Représente un monde possible (ex. : 00, 01).
Note : Différemment de ce que nous avons fait dans l’épisode 1, ici les mondes possibles seront
représentés par des entiers-}
type World = Integer

{-Définissez le type des états épistémiques EpiState. Ceci est un tuple de trois éléments :
— Une fonction d’interprétation de type Prop -> [World].
— Une fonction d’indiscernabilité de type Agent -> World -> [World].
— Un monde possible qui représente le monde réel.-}
type EpiState = (Prop -> [World], Agent -> World -> [World], World)


data EpiFormula = 
        T 
        | F 
        | Var Prop 
        | Not EpiFormula
        | And EpiFormula EpiFormula 
        | Or EpiFormula EpiFormula 
        | Imp EpiFormula EpiFormula
        | Eqv EpiFormula EpiFormula
        | Knows Agent EpiFormula       -- connaissance : agent sait que formule
        | After EpiFormula EpiFormula  -- dynamique : apres formule1, on a formule2
        deriving (Eq, Show)

--(épistémique s, une formule phi) -> True si s satisfait phi, False sinon.
epiSat :: EpiState -> EpiFormula -> Bool
epiSat _ T = True
epiSat _ F = False
epiSat (i, _, w) (Var x) = w `elem` i x 
epiSat s (Not phi) = not (epiSat s phi)
epiSat s (And phi psi) = epiSat s phi && epiSat s psi
epiSat s (Or phi psi) = epiSat s phi || epiSat s psi
epiSat s (Imp phi psi) = epiSat s (Not phi) || epiSat s psi  -- non phi ou psi
epiSat s (Eqv phi psi) = epiSat s phi == epiSat s psi -- phi implique psi et psi implique phi
epiSat (interp, ind, w) (Knows a phi) = all (\w' -> epiSat (interp, ind, w') phi) (ind a w)
epiSat s (After phi psi) = epiSat s phi && epiSat (update s phi) psi

--(épistémique s, une formule phi) -> un nouvel état épistémique correspondant à la mise à jour de s par phi
update :: EpiState -> EpiFormula -> EpiState
update (interp, ind, w) phi = (interp', ind', w)
    where
    interp' x = [w' | w' <- interp x, epiSat (interp, ind, w') phi] -- on filtre les mondes qui ne satisfont pas phi
    ind' a w' = [w'' | w'' <- ind a w', epiSat (interp, ind, w'') phi] 


testEpiSat :: [Bool]
testEpiSat = [True, True]
    
testUpdate :: [Bool]
testUpdate = [True, True]


test :: [Bool] -> Bool
test [] = True
test (x : xs) = x && test xs

testAll :: [Char]
testAll
    | test [ 
        test testEpiSat, 
        test testUpdate
        ] == True = "Success!"
    | otherwise = "Fail!"