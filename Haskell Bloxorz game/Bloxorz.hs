-- Copyright Cezar Craciunoiu 324CA
-- Comentarii -> 80 caractere / Cod -> 100 caractere
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}


module Bloxorz where

import ProblemState

import qualified Data.Array as A

{-
    Caracterele ce vor fi printate pentru fiecare tip de obiect din joc 
    Puteți înlocui aceste caractere cu orice, în afară de '\n'.
-}
hardTile :: Char
hardTile = '#'

softTile :: Char
softTile = '='

block :: Char
block = 'B'

switch :: Char
switch = '+'

emptySpace :: Char
emptySpace = ' '

winningTile :: Char
winningTile = '*'

{-
    Sinonim de tip de date pentru reprezetarea unei perechi (int, int)
    care va reține coordonatele de pe tabla jocului
-}
type Position = (Int, Int)

{-
    Direcțiile în care se poate mișcă blocul de pe tablă
-}
data Directions = North | South | West | East
    deriving (Show, Eq, Ord)

{-
    In celule se salveaza doar caracterul ce reprezinta un tile
-}
data Cell = Cell { chr     :: Char
                 } deriving (Eq, Ord)

instance Show Cell where
    show = show . chr

{-
    Un nivel retine urmatoarele informatii:
    - matrix -> Array de array-uri ce contine celulele hartii
    - size -> Dimensiunea matricei
    - trig -> O lista cu switch-urile ce activeaza o lista de tile-uri
    - won -> True - Jocul a fost castigat / False - Jocul a fost pierdut
    - finished -> True - Jocul s-a terminat / False - Jocul e in desfasurare
    - blockPos -> Pereche cu cele 2 pozitii ale blocului (daca sunt egale
                blocul este in picioare)
-}
data Level = Level { matrix   :: A.Array Position Cell
                   , size     :: Int
                   , trig     :: [(Position, [Position])]
                   , won      :: Bool
                   , finished :: Bool
                   , blockPos :: (Position, Position)
                   }  deriving (Eq, Ord)

{-
    Functii pentru inlocuitul campurilor din level
-}
updateBlockPos :: Level -> (Position, Position) -> Level
updateBlockPos lvl newPos = Level (matrix lvl) (size lvl) (trig lvl) 
                                  (won lvl) (finished lvl) newPos

updateFinished :: Level -> Bool -> Level
updateFinished lvl newFinished = Level (matrix lvl) (size lvl) (trig lvl) 
                                  (won lvl) newFinished (blockPos lvl)

updateWon :: Level -> Bool -> Level
updateWon lvl newWon = Level (matrix lvl) (size lvl) (trig lvl) 
                                  newWon (finished lvl) (blockPos lvl)

updateTrig :: Level -> [(Position, [Position])] -> Level
updateTrig lvl newTrig = Level (matrix lvl) (size lvl) newTrig 
                                  (won lvl) (finished lvl) (blockPos lvl)

updateSize :: Level -> Int -> Level
updateSize lvl newSize = Level (matrix lvl) newSize (trig lvl) 
                                  (won lvl) (finished lvl) (blockPos lvl)

updateMatrix :: Level -> A.Array Position Cell -> Level
updateMatrix lvl newMatrix = Level newMatrix (size lvl) (trig lvl) 
                                  (won lvl) (finished lvl) (blockPos lvl)

{-
    Afiseaza mai intai matricea pe care s-au aplicat cele 2 functii de mai jos.
    Inainte de asta adauga si blocul retinut in blockPos. La final in functie
    daca s-a terminat sau nu meciul afiseaza sau nu mesaj la final.
-}
instance Show Level where
    show lvl = ("\n" ++ (removePunc (concat $ printMatrix 
                     ((matrix lvl) A.// [(fst (blockPos lvl), (Cell block)), 
                                        (snd (blockPos lvl), (Cell block))]))))                      
                    ++ (if (not (finished lvl)) 
                        then ""
                        else (if (won lvl) 
                              then "Congrats! You won!\n"
                              else "Game Over\n"))

{-
    Parcurge tot string-ul obtinut cu matricea si elimina caracterele ' ce
    apar cand se apeleaza show pe un char (show H devine 'H')
-}                          
removePunc :: String -> String
removePunc msg = [char | char <- msg, not (char `elem` "\'")]

{-
    Parcurge dupa coloane si dupa linii array-ul si afiseaza in parte fiecare
    element si il adauga la lista. La final se obtine o lista de stringuri cu
    fiecare linie.
-}
printMatrix :: Show a => A.Array (Int, Int) a -> [String]
printMatrix matr = concat [([(show (matr A.! (row, col))) 
                        | col <- [0..(snd $ snd $ A.bounds matr)] ] ++ ["\n"]) 
                        | row <- [0..(fst $ snd $ A.bounds matr)] ]

{-
    Construieste un level cu matricea initializata cu emptySpace si restul
    datelor de inceput
-}
emptyLevel :: Position -> Position -> Level
emptyLevel pos cube = Level (A.array ((0, 0), ((fst pos) , (snd pos)))
                            [((x, y), (Cell emptySpace)) |  x <- [0..(fst(pos))],
                            y <- [0..(snd(pos))]])
                            (fst pos) [] False False (cube, cube)

{-
    Transforma un char in Cell si il adauga la pozitia data in level
-}
addTile :: Char -> Position -> Level -> Level
addTile char pos lvl = updateMatrix lvl ((matrix lvl) A.// [(pos, (Cell (convertChar char)))])

{-
    Primeste mai multe pozitii pe care sa le schimbe si un char pe
    care sa il punca acolo
-}
changeTiles :: Char -> [Position] -> Level -> Level
changeTiles char pos lvl = updateMatrix lvl ((matrix lvl) A.// 
                            (zip pos (replicate  (length pos) (Cell char))))

{-
    Face traducerea din input in tile-ul corespunzator
-}
convertChar :: Char -> Char
convertChar c
    | c == 'H'  = hardTile
    | c == 'S'  = softTile
    | c == 'W'  = winningTile
    | c == '+'  = switch
    | otherwise = 'E'

{-
    Pune un switch nou pe harta respectiva in lista de perechi din trig
-}
addSwitch :: Position -> [Position] -> Level -> Level
addSwitch swpos poss lvl = Level (matrix (addTile switch swpos lvl)) (size lvl)
                                ((trig lvl) ++ [(swpos, poss)]) 
                                (won lvl) (finished lvl) (blockPos lvl)

{-
    Cauta in lista de perechi din trig switch-ul si apoi returneaza tile-urile
    care urmeaza sa se dez/activeze
-}
findSwitch :: Position -> [(Position, [Position])] -> [Position]
findSwitch toFind list = snd $ head $
                         filter (\(index, _) -> (index == toFind)) list


{-
    Verifica in functie de tile-ul pe care s-a calcat, ce umreaza sa se faca:
    - daca e emptySpace jocul s-a pierdut
    - daca e softTile si e in picioare jocul e pierdut, atlfel e ok
    - daca e hardTile nu se face nimic
    - daca e winningTile si e in picioare a castigat, atlfel se continua
    - daca e switch verifica ce tip ce tile e acolo unde vrea sa se activeze:
    daca e emptySpace pune hardTile, respectiv invers
    Toate cazurile apeleaza la final activate2 care verifica acelasi lucru
    pentru a doua jumatate de bloc.
-}
activate :: (Cell, Cell) -> Level -> Level
activate cells lvl
    | (chr $ fst cells) == emptySpace = updateFinished (updateWon lvl False) True
    | (chr $ fst cells) == switch = activate2 cells $ 
                if ((matrix lvl) A.! (head (findSwitch (fst $ blockPos lvl) (trig lvl))) == 
                    (Cell emptySpace))
                then (changeTiles hardTile (findSwitch (fst $ blockPos lvl) (trig lvl)) lvl) 
                else (changeTiles emptySpace (findSwitch (fst $ blockPos lvl) (trig lvl)) lvl)
    | (chr $ fst cells) == softTile = if ((fst (blockPos lvl)) == (snd (blockPos lvl)))
                                      then updateFinished (updateWon lvl False) True
                                      else (activate2 cells lvl)
    | (chr $ fst cells) == winningTile = if ((fst (blockPos lvl)) == (snd (blockPos lvl)))
                                         then updateFinished (updateWon lvl True) True
                                         else (activate2 cells lvl)
    | (chr $ fst cells) == hardTile = (activate2 cells lvl)
    | otherwise = updateBlockPos lvl ((-1, -1), (-1, -1))

{-
    La fel ca cea de sus dar se verifica cazurile pentru a doua jumatate
    de bloc. Functia este apelata de primul activate.
-}
activate2 :: (Cell, Cell) -> Level -> Level
activate2 cells lvl
    | (chr $ snd cells) == emptySpace = updateFinished (updateWon lvl False) True
    | (chr $ snd cells) == switch = 
                if ((matrix lvl) A.! (head (findSwitch (snd $ blockPos lvl) (trig lvl))) == 
                    (Cell emptySpace)) 
                then (changeTiles hardTile (findSwitch (snd $ blockPos lvl) (trig lvl)) lvl) 
                else (changeTiles emptySpace (findSwitch (snd $ blockPos lvl) (trig lvl)) lvl)
    | (chr $ snd cells) == softTile = if ((fst (blockPos lvl)) == (snd (blockPos lvl)))
                                      then updateFinished (updateWon lvl False) True
                                      else lvl
    | (chr $ snd cells) == winningTile = if ((fst (blockPos lvl)) == (snd (blockPos lvl)))
                                         then updateFinished (updateWon lvl True) True
                                         else lvl
    | (chr $ snd cells) == hardTile = lvl
    | otherwise = updateBlockPos lvl ((-1, -1), (-1, -1))

{-
    Verifica mai intai daca s-a terminat jocul, caz in care nu face nimic.
    Daca inca e in desfasurare atunci verifica daca blocul e in picioare sau
    vrea sa se dea "undo" la mutare. Altfel verifica daca blocul e culcat
    si vrea doar sa se rasuceasca. In ultimul caz inseamna ca urmeaza sa se
    repuna in picioare, avand a doua jumatate in jos. In fiecare caz se
    apeleaza activate pentru a se verifica interactiunea cu blocurile si apoi
    se schimba pozitia blocului pe harta.
-}
move :: Directions -> Level -> Level
move dir lvl
    | finished lvl =  lvl
    | otherwise = (if (fst(blockPos lvl) == snd(blockPos lvl) || (goBack dir (blockPos lvl)))
        then (activate (((matrix lvl) A.! (fst (dirToPos dir (blockPos lvl) 0 1))),
             ((matrix lvl) A.! (snd (dirToPos dir (blockPos lvl) 0 1)))) 
             (updateBlockPos lvl (dirToPos dir (blockPos lvl) 0 1)))
        else (if (stay2Block dir (blockPos lvl))
              then (activate (((matrix lvl) A.! (fst (dirToPos dir (blockPos lvl) 0 0))),
                   ((matrix lvl) A.! (snd (dirToPos dir (blockPos lvl) 0 0)))) 
                   (updateBlockPos lvl (dirToPos dir (blockPos lvl) 0 0)))
              else (activate (((matrix lvl) A.! (fst (dirToPos dir (blockPos lvl) 1 0))),
                   ((matrix lvl) A.! (snd (dirToPos dir (blockPos lvl) 1 0)))) 
                   (updateBlockPos lvl (dirToPos dir (blockPos lvl) 1 0)))))
{-
    Verifica daca urmatoarea mutare va lasa blocul tot culcat
-}
stay2Block :: Directions -> (Position, Position) -> Bool
stay2Block dir pos
    | ((dir == North || dir == South) && 
        ((snd $ snd pos) - (snd $ fst pos) /= 0)) = True
    | ((dir == West || dir == East) && 
        ((fst $ snd pos) - (fst $ fst pos) /= 0)) = True
    | otherwise = False

{- 
    Verifica daca urmatoarea mutare va pune piesa in picioare inapoi de unde
    a venit
-}
goBack :: Directions -> (Position, Position) -> Bool
goBack dir pos
    | dir == North && (((fst $ fst pos) - (fst $ snd pos)) < 0) = True
    | dir == South && (((fst $ fst pos) - (fst $ snd pos)) > 0) = True
    | dir == East  && (((snd $ fst pos) - (snd $ snd pos)) > 0) = True
    | dir == West  && (((snd $ fst pos) - (snd $ snd pos)) < 0) = True
    | otherwise = False

{-
    Translateaza din directie in pozitie:
    - roll poate fi 0 (daca se rasuceste) si 1 (daca se pune in picioare)
    - fall poate fi 0 (daca vrea sa cada) si 1 (daca se pune in picioare)
-}
dirToPos :: Directions -> (Position, Position) -> Int -> Int -> (Position, Position)
dirToPos dir pos roll fall
    | dir == North =  (((fst $ fst pos) - (1 + roll), (snd $ fst pos)), 
                        ((fst $ snd pos) - (1 + fall), (snd $ snd pos)))
    | dir == South =  (((fst $ fst pos) + (1 + roll), (snd $ fst pos)), 
                        ((fst $ snd pos) + (1 + fall), (snd $ snd pos)))
    | dir == East  =  (((fst $ fst pos), (snd $ fst pos) + (1 + roll)), 
                        ((fst $ snd pos), (snd $ snd pos) + (1 + fall)))
    | dir == West  =  (((fst $ fst pos), (snd $ fst pos) - (1 + roll)), 
                        ((fst $ snd pos), (snd $ snd pos) - (1 + fall)))
    | otherwise    =  ((-1, -1), (-1, -1))

{-
    Returneaza True daca jocul nu s-a terminat, adica interogheaza finished
-}
continueGame :: Level -> Bool
continueGame lvl = not (finished lvl)

{-
    isGoal -> verifica daca blocul blocul este pe o pozitie de win si daca
    este in picioare
    succesors -> construieste mai intai o lista cu toate mutarila care nu duc
    la defeat. In asta verifica daca exista un win. Daca exista il retureaza
    doar pe acela, altfel returneaza toate lista.
-}
instance ProblemState Level Directions where
    successors lvl = let succs = ((if (finished $ move West lvl) && (not $ won $ move West lvl)
                                then [] else [(West, move West lvl)]) ++ 
                                (if (finished $ move South lvl) && (not $ won $ move South lvl) 
                                then [] else [(South, move South lvl)]) ++ 
                                (if (finished $ move East lvl) && (not $ won $ move East lvl)
                                then [] else [(East, move East lvl)]) ++ 
                                (if (finished $ move North lvl) && (not $ won $ move North lvl)
                                then [] else [(North, move North lvl)]) ++ [])
                    in if (length [x | x <- succs, ((finished $ snd x) && (won $ snd x))] == 0) 
                        then succs else [x | x <- succs, ((finished $ snd x) && (won $ snd x))]

    isGoal lvl = (fst $ blockPos lvl) == (snd $ blockPos lvl) && 
                 (chr ((matrix lvl) A.! (fst $ blockPos lvl)) == winningTile)
    -- Doar petru BONUS
    -- heuristic = undefined
