{-# OPTIONS_GHC -Wall #-}

module Cha4 (challenge4) where

    import CPL

    -- Choisis n’importe quelle cellule, ça n’a pas d’importance !
    -- ie: 2 tigres ou 2 peluches
    door1 :: Formula
    door1 = Or (And (Var "t1") (Var "t2")) (And (Var "p1") (Var "p2"))

    -- Il y a une peluche dans l’autre cellule.
    door2 :: Formula
    door2 = Var "p1"

    -- Ne peut y avoir les deux en même temps
    constraint :: Formula
    constraint = And 
                (Not (And (Var "p1") (Var "t1"))) 
                (Not (And (Var "p2") (Var "t2")))

    {-
    Cellule1 -> affiche vrai si peluche, faux sinon
    Cellule2 -> affiche faux si peluche, vrai sinon
    -}
    reglement :: Formula
    reglement = And
                (And (Imp (Var "p1") door1) (Imp (Var "t1") (Not door1)))
                (And (Imp (Var "p2") (Not door2)) (Imp (Var "t2") door2))

    -- La formule finale
    challenge4 :: Formula
    challenge4 = And constraint reglement
