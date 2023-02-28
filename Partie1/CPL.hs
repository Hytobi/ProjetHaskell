{-# OPTIONS_GHC -Wall #-}

module CPL (
    Formula(..),
    World,
    genAllWorlds,
    sat,
    findWorlds,
    testGenAllWorlds,
    testSat,
    testFindWorlds,
    testAll
    ) where


    data Formula = 
        T 
        | F 
        | Var String 
        | Not Formula
        | And Formula Formula 
        | Or Formula Formula 
        | Imp Formula Formula
        | Eqv Formula Formula
        deriving (Eq, Show)

    type World = [String] 

    {-
    Pour une liste de noms de variables propositionnels,
    génère la liste de tous les mondes possibles pour ces variables
    -}
    genAllWorlds :: [String] -> [World]
    genAllWorlds [] = [[]]
    genAllWorlds (x:xs) = [x:ws | ws <- genAllWorlds xs] ++ [ws | ws <- genAllWorlds xs]


    -- Vérifie si un monde w satisfait une formule phi 
    sat :: World -> Formula -> Bool
    sat _ T = True
    sat _ F = False
    sat w (Var x) = x `elem` w
    sat w (Not phi) = not (sat w phi)
    sat w (And phi psy) = sat w phi && sat w psy
    sat w (Or phi psy) = sat w phi || sat w psy
    sat w (Imp phi psy) = sat w (Or (Not phi) psy)
    sat w (Eqv phi psy) = sat w (And (Imp phi psy) (Imp psy phi))

    -- Choix de faire plusieur fonctions plutot qu'un where de 2km
    -- On supprime les doublons d'une liste avec un filtre
    deleteDouble :: [String] -> [String]
    deleteDouble [] = []
    deleteDouble (x:xs) = x : deleteDouble (filter (/= x) xs)

    -- On récupère les variables d'une formule
    getVars :: Formula -> [String]
    getVars f = deleteDouble (getVars' f)
        where
        getVars' :: Formula -> [String]
        getVars' T = []
        getVars' F = []
        getVars' (Var x) = [x]
        getVars' (Not phi) = getVars' phi
        getVars' (And phi psy) = getVars' phi ++ getVars' psy
        getVars' (Or phi psy) = getVars' phi ++ getVars' psy
        getVars' (Imp phi psy) = getVars' phi ++ getVars' psy
        getVars' (Eqv phi psy) = getVars' phi ++ getVars' psy

    -- On vérifie si une formule est satisfiable
    findWorlds :: Formula -> [World]
    findWorlds phi = [w | w <- genAllWorlds (getVars phi), sat w phi]
        
        
    -- Tests
    testGenAllWorlds :: [Bool]
    testGenAllWorlds = [ 
        genAllWorlds ["p1", "p2", "t1", "t2"] == 
            [["p1", "p2", "t1", "t2"], ["p1", "p2", "t1"], ["p1", "p2", "t2"], 
            ["p1", "p2"], ["p1", "t1", "t2"], ["p1", "t1"], ["p1", "t2"], ["p1"], 
            ["p2", "t1", "t2"], ["p2", "t1"], ["p2", "t2"], ["p2"], ["t1", "t2"], 
            ["t1"], ["t2"], []],
        genAllWorlds ["p1","t1"] == [["p1", "t1"], ["p1"], ["t1"], []],
        genAllWorlds [] == [[]]
        ]

    testSat :: [Bool]
    testSat = [
        tmpSat (Var "p1") == True,
        tmpSat (Var "p3") == False,
        tmpSat (Not (Var "p1")) == False,
        tmpSat (Not (Var "p3")) == True,
        tmpSat (And (Var "p1") (Var "t1")) == True,
        tmpSat (And (Var "p1") (Var "t2")) == True,
        tmpSat (Or (Var "p1") (Var "t1")) == True,
        tmpSat (Imp (Var "p1") (Var "t2")) == True,
        tmpSat (Eqv (Var "p1") (Var "t1")) == True,
        not (sat [] (Eqv T F)),
        sat [] (Eqv T T),
        sat [] (Or T F),
        not (sat [] (And T F))
        ]
        where
            tmpSat = sat ["p1", "p2", "t1", "t2"]

    testDeleteDouble :: [Bool]
    testDeleteDouble = [
        deleteDouble ["p1", "p2", "t1", "t2"] == ["p1", "p2", "t1", "t2"],
        deleteDouble ["p1", "p2", "t1", "t2", "p1", "p2", "t1", "t2"] == ["p1", "p2", "t1", "t2"],
        deleteDouble ["p1", "p2", "t1", "t2", "p1", "p2", "t1", "t2", "p1", "p2", "t1", "t2"] == ["p1", "p2", "t1", "t2"]
        ]

    testGetVars :: [Bool]
    testGetVars = [
        getVars (Var "p1") == ["p1"],
        getVars (Not (Var "p1")) == ["p1"],
        getVars (And (Var "p1") (Var "t1")) == ["p1", "t1"],
        getVars (And (Var "p1") (Var "t2")) == ["p1", "t2"],
        getVars (Or (Var "p1") (Var "t1")) == ["p1", "t1"],
        getVars (Imp (Var "p1") (Var "t2")) == ["p1", "t2"],
        getVars (Eqv (Var "p1") (Var "t1")) == ["p1", "t1"],
        not (getVars (Or (Var "p1") (Not (Var "p1"))) == ["p1", "p1"])
        ]

    testFindWorlds :: [Bool]
    testFindWorlds = [
        null (findWorlds F),
        findWorlds T == [[]],
        findWorlds (Var "p1") == [["p1"]],
        not (["p1"] `elem` findWorlds (And (Var "p1") (Var "t1"))),
        ["p1", "t1"] `elem` findWorlds (And (Var "p1") (Var "t1")),
        ["p1", "t1"] `elem` findWorlds (Or (Var "p1") (Var "t1")),
        ["p1", "t1"] `elem` findWorlds (Imp (Var "p1") (Var "t1")),
        ["p1", "t1"] `elem` findWorlds (Eqv (Var "p1") (Var "t1")),
        ["p1", "t1"] `elem` findWorlds (And (Var "p1") (Var "t1")),
        ["p1", "t1"] `elem` findWorlds (And (Var "p1") (Var "t1")),
        ["p1", "t1"] `elem` findWorlds (And (Var "p1") (Var "t1")),
        findWorlds (Var "p1") == findWorlds (Not (Not (Var "p1")))
        ]

    test :: [Bool] -> Bool
    test [] = True
    test (x : xs) = x && test xs

    testAll :: [Char]
    testAll
        | test [ 
            test testGenAllWorlds, 
            test testSat,
            test testDeleteDouble,
            test testGetVars,
            test testFindWorlds
            ] == True = "Success!"
        | otherwise = "Fail!"




