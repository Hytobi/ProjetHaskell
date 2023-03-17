{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Problem2 (problem2) where

    import EL



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
    problem2 = And 
                (And aliceIgn bobIgn) 
                (And 
                    (After fatherAnn (And aliceIgn bobIgn) ) 
                    (After 
                        fatherAnn 
                        (Not (And aliceIgn bobIgn))
                    )
                )