{-# OPTIONS_GHC -Wall #-}

module Cha2 (challenge2) where

    import CPL

    -- Une au moins de deux cellules contient une peluche.
    door1 :: Formula
    door1 = Or (Var "p1") (Var "p2")

    -- Il y a un tigre dans l’autre cellule. ie : un tigre sûr
    door2 :: Formula
    door2 = Var "t1"

    -- Ne peut y avoir les deux en même temps
    constraint :: Formula
    constraint = And 
                (Not (And (Var "p1") (Var "t1"))) 
                (Not (And (Var "p2") (Var "t2")))

    -- Les 2 sont vraies ou les 2 sont fausses 
    reglement :: Formula
    reglement = Or (And door1 door2) (And (Not door1) (Not door2))

    -- La formule finale
    challenge2 :: Formula
    challenge2 = And constraint reglement
