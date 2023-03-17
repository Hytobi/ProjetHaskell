{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Problem1 (problem1) where

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

    s0 :: EpiState
    s0 = (interp, indis, 01)

    --Exprime l’annonce du père, c.-à-d., « Un parmi vous a le visage sale. »
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

    {-Exprime le problème 1 dans sa totalité, c.-à-d. :
    « Les deux agents sont ignorants, et
    Après l’annonce du père, Alice est ignorante, et
    après l’annonce que Bob sait que son propre visage est sale, Alice et Bob ne sont pas ignorants. »
    Note : L’annonce que Bob sait que son propre visage n’est pas sale est fait implicitement le moment où
    il part laver son visage sans même pas regarder le miroir.-}
    problem1 :: EpiFormula
    problem1 = And 
                (And aliceIgn bobIgn) 
                (And 
                    (After fatherAnn aliceIgn) 
                    (After 
                        (Knows "b" (Var "bs")) 
                        (Not (And aliceIgn bobIgn))
                    )
                )



