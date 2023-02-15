{-# OPTIONS_GHC -Wall #-}

module Cha3 (challenge3) where

    import CPL


    -- Il y a un tigre dans cette cellule ou il y a une peluche dans l’autre.
    door1 :: Formula
    door1 = Or (Var "t1") (Var "p2")

    -- Il y a une peluche dans l’autre cellule.
    door2 :: Formula
    door2 = Var "p1"

    -- Ne peut y avoir les deux en même temps
    constraint :: Formula
    constraint = And 
                (Not (And (Var "p1") (Var "t1"))) 
                (Not (And (Var "p2") (Var "t2")))

    -- Les 2 sont vraies ou les 2 sont fausses 
    reglement :: Formula
    reglement = Or (And door1 door2) (And (Not door1) (Not door2))

    -- La formule finale
    challenge3 :: Formula
    challenge3 = And constraint reglement
