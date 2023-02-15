{-# OPTIONS_GHC -Wall #-}

module Cha4 (challenge6) where

    import CPL

    -- Il y a un tigre ici.
    door1 :: Formula
    door1 = Var "t1"

    -- Cette cellule contient une peluche.
    door2 :: Formula
    door2 = Var "p2"

    -- Il y a un tigre dans la cellule 2.
    door3 :: Formula
    door3 = Var "t2"

    -- tigre ou peluche
    constraint :: Formula
    constraint = And (And
      (Eqv (Var "p1") (Not (Var "t1")))
      (Eqv (Var "p2") (Not (Var "t2"))))
      (Eqv (Var "p3") (Not (Var "t3")))

    -- Une seule  renferme une peluche, les autres un tigre && une seule ne ment pas
    reglement :: Formula
    reglement = And 
            (Or (
                Or (
                    And 
                        (And door1 (Not door2)) 
                        (Not door3))
                    (And
                        (And (Not door1) door2 )
                        (Not door3)))
                (And (And (Not door1) (Not door2)) door3 ))
            (And (And
                (Imp (Var "p1") (And (Not (Var "p2")) (Not (Var "p3"))))
                (Imp (Var "p2") (And (Not (Var "p1")) (Not (Var "p3")))))
                (Imp (Var "p3") (And (Not (Var "p1")) (Not (Var "p2")))))

    -- La formule finale
    challenge6 :: Formula
    challenge6 = And constraint reglement
