{-# OPTIONS_GHC -Wall #-}

module Cha4 (challenge5) where

    import CPL

    -- Choisis bien ta cellule ça’a a de l’importance !
    door1 :: Formula
    door1 = Or (And (Var "p1") (Var "t2")) (And (Var "t1") (Var "p2"))

    -- Tu ferais mieux de choisir l’autre cellule !
    door2 :: Formula
    door2 = And (Var "p1") (Var "t2")

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
    challenge5 :: Formula
    challenge5 = And constraint reglement
