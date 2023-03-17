{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Problem2 (problem2) where

    import EL

    interp :: Prop -> [World]
    interp "as" = [10 ,11]  -- Alice est sale dans les mondes 10 et 11.
    interp "bs" = [01 ,11]  -- Bob est sale dans les mondes 01 et 11.
    interp _ = []           -- Toutes les autres propositions sont fausses.

    indis :: Agent -> World -> [World]
    indis "a" 00 = [00 ,10] -- Alice ne peut pas distinguer 00 de 10.
    indis "b" 00 = [00 ,01] -- Bob ne peut pas distinguer 00 de 01.
    indis "a" 01 = [01 ,11] -- Alice ne peut pas distinguer 01 de 11.
    indis "b" 01 = [00 ,01] -- Bob ne peut pas distinguer 01 de 00.
    indis "a" 10 = [00 ,10] -- Alice ne peut pas distinguer 10 de 00.
    indis "b" 10 = [10 ,11] -- Bob ne peut pas distinguer 10 de 11.
    indis "a" 11 = [01,11]  -- Alice ne peut pas distinguer 11 de 01.
    indis "b" 11 = [10,11]  -- Bob ne peut pas distinguer 11 de 10.
    indis _ _ = []          -- Tous les autres agents ne peuvent pas distinguer les autres mondes.

    -- Les deux ont leurs visages sales
    s0 :: EpiState
    s0 = (interp, indis, 11)

    -- Leur père les informe qu’au moins un des deux a le visage sale
    fatherAnn :: EpiFormula
    fatherAnn = Or (Var "as") (Var "bs")

    {-Exprime l’ignorance d’Alice sur son état, c.-à-d., « Alice ne sait pas si son propre visage est sale. »
    Indice : Ceci est équivalent à : « Alice ne sait pas que le visage d’Alice est sale, et
    Alice ne sait pas que le visage d’Alice n’est pas sale. »-}
    aliceIgn :: EpiFormula
    aliceIgn = And (Not (Knows "a" (Var "as"))) (Not (Knows "a" (Not (Var "as"))))

    --Exprime l’ignorance de Bob sur son état, c.-à-d., « Bob ne sait pas si son propre visage est sale. »
    bobIgn :: EpiFormula
    bobIgn = And (Not (Knows "b" (Var "bs"))) (Not (Knows "b" (Not (Var "bs"))))


    {- Celui qui sait si son propre visage est sale, fait un pas en avant. », mais ni Alice ni Bob bougent.
    Il répète sa réquisition : « Celui qui sait si son propre visage est sale, fait un pas en avant. » Cette
    fois, Alice et Bob font un pas en avant en même temps.-}
    problem2 :: EpiFormula
    problem2 =   And
                    ( And aliceIgn bobIgn )
                    ( After
                        ( After
                            fatherAnn
                            ( And aliceIgn bobIgn )
                        )
                        ( And
                            (Knows "a" (Var "as"))
                            (Knows "b" (Var "bs"))
                        )
                    )

    testEpisat :: [Bool]
    testEpisat =
        [ epiSat s0 fatherAnn,
            epiSat s0 aliceIgn,
            epiSat s0 bobIgn,
            epiSat s0 problem2
        ]