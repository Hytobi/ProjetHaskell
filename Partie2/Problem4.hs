{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Problem4 (problem4) where

    import EL

    interp :: Prop -> [World]
    interp "a0" = [01]
    interp "a1" = [10, 12]
    interp "a2" = [21, 23]
    interp "a3" = [32, 34]
    interp "a4" = [43]
    interp "b0" = [10]
    interp "b1" = [01, 21]
    interp "b2" = [12, 32]
    interp "b3" = [23, 43]
    interp "b4" = [34]
    interp _ = []

    indis :: Agent -> World -> [World]
    indis "a" 10 = [10,12]
    indis "a" 12 = [12,10]
    indis "a" 21 = [21,23]
    indis "a" 23 = [21,23]
    indis "a" 32 = [32,34]
    indis "a" 34 = [32,34]
    indis "b" 01 = [01,21]
    indis "b" 21 = [01,21]
    indis "b" 12 = [12,32]
    indis "b" 32 = [12,32]
    indis "b" 23 = [23,43]
    indis "b" 43 = [23,43]
    indis _ _  = []

    s0 :: EpiState
    s0 = (interp, indis, 12)

    anneIgn :: EpiFormula
    anneIgn =
        And 
            (Not (Knows "a" (Var "b0")))
            (And 
                (Not (Knows "a" (Var "b1")))
                (And 
                    (Not (Knows "a" (Var "b2")))
                    (And 
                        (Not (Knows "a" (Var "b3")))
                        (Not (Knows "a" (Var "b4"))))))

    billIgn :: EpiFormula
    billIgn = 
        And 
            (Not (Knows "b" (Var "a0")))
            (And 
                (Not (Knows "b" (Var "a1")))
                (And 
                    (Not (Knows "b" (Var "a2")))
                    (And 
                        (Not (Knows "b" (Var "a3")))
                        (Not (Knows "b" (Var "a4"))))))

    problem4 :: EpiFormula
    problem4 =
        And
            (And anneIgn billIgn)
            (After anneIgn 
                (After 
                    billIgn 
                    (Not anneIgn)))
    
    testEpisat :: [Bool]
    testEpisat =
        [ epiSat s0 anneIgn,
            epiSat s0 billIgn,
            epiSat s0 problem4
        ]

