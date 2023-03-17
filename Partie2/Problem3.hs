{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Problem3 (problem3) where

    import EL

    interp :: Prop -> [World]
    interp "as" = [100, 110, 101, 111]  -- Alice est sale dans les mondes 100, 110, 101 et 111.
    interp "bs" = [010, 011, 110, 111]  -- Bob est sale dans les mondes 010, 011, 110 et 111.
    interp "cs" = [001, 101, 011, 111]  -- Caroline est sale dans les mondes 01 et 11.
    interp _ = []           -- Toutes les autres propositions sont fausses.

    indis :: Agent -> World -> [World]
    indis "a" 000 = [000, 100]
    indis "b" 000 = [000, 010]
    indis "c" 000 = [000, 001]
    indis "a" 100 = [000, 100]
    indis "b" 100 = [100, 110]
    indis "c" 100 = [100, 101]
    indis "a" 001 = [001, 101]
    indis "b" 001 = [001, 011]
    indis "c" 001 = [000, 001]
    indis "a" 101 = [001, 101]
    indis "b" 101 = [101, 111]
    indis "c" 101 = [100, 101]
    indis "a" 010 = [010, 110]
    indis "b" 010 = [000, 010]
    indis "c" 010 = [010, 011]
    indis "a" 110 = [010, 110]
    indis "b" 110 = [100, 110]
    indis "c" 110 = [110, 111]
    indis "a" 011 = [011, 111]
    indis "b" 011 = [001, 011]
    indis "c" 011 = [010, 011]
    indis "a" 111 = [011, 111]
    indis "b" 111 = [101, 111]
    indis "c" 111 = [110, 111]
    indis _ _ = []

    s0 :: EpiState
    s0 = (interp, indis, 111)