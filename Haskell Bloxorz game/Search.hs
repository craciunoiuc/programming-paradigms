-- Copyright Cezar Craciunoiu 324CA
-- Comentarii -> 80 caractere / Cod -> 100 caractere
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import ProblemState

import qualified Data.Set as S
import qualified Data.List as L(nub)

{-
    In reprezentare sunt 2 tipuri de noduri:
    - Nil -> parintele nodului radacina, ce are dimensiunea -1
    - Node -> ce reprezinta un nod oarecare ce pe langa stare si actiunea care
    a dus la aceasta mai retine si nodul precedent respectiv nodurile urmatoare
    cat si adancimea la care se afla
-}
data Node s a = Nil {depth :: Int} | Node  { state    :: s
                                           , parent   :: Node s a
                                           , depth    :: Int
                                           , action   :: a
                                           , children :: [Node s a]
                                           }

{-
    Implementari ale Ord, Eq, Show ce se leaga de state adica singura
    informatie ce se poate afisa (restul nu se pot afisa (action) sau sunt
    metadate)
-}
instance (Ord s) => Ord (Node s a) where
   x < y = ((state x) < (state y))
   x <= y = ((state x) <= (state y))
   x >= y = ((state x) >= (state y))
   x > y = ((state x) > (state y))

instance (Eq s) => Eq (Node s a) where
    x == y = ((state x) == (state y))
                      
instance Show s => Show (Node s a) where
    show = show . state

nodeState :: Node s a -> s
nodeState = state

{-
    Functia apeleaza o functie recursiva helper care sa poata trimite de la un
    pas la altul informatie in legatura cu nodul actual si nodul parinte. S-a
    ales sa se foloseasca buildStateSpace deoarece prin createStateSpace nu se
    putea trimite informatie in legatura cu actiunea ce a generat starea
-}
createStateSpace :: (ProblemState s a) => s -> Node s a
createStateSpace st = buildStateSpace st (fst $ head $ successors st) (Nil 0) 0

{-
    Construieste spatiul starilor folosindu-se de evaluarea lazy. Nodurile care
    reprezinta win-ul nu au copii deoarece reprezinta o stare finala.
-}
buildStateSpace :: (ProblemState s a) => s -- stare
                    -> a -- actiune
                    -> Node s a -- parinte
                    -> Int -- depth
                    -> Node s a -- nod radacina returnat
buildStateSpace st act prnt dep = if isGoal st
                                    then Node st prnt dep act [] 
                                    else Node st prnt dep act 
                                    [(buildStateSpace s a (buildStateSpace st act prnt dep)
                                    (dep + 1)) | (a, s) <- (successors st)]

orderStateSpace :: (ProblemState s a) => Node s a -> Node s a
orderStateSpace = undefined


{-
    Se foloseste de o functie recursiva helper pentru a parcurge spatiul
    starilor avand si un set ca parametru
-}
limitedDfs :: (ProblemState s a, Ord s)
           => Node s a    -- Nodul stării inițiale
           -> Int         -- Adâncimea maximă de explorare
           -> [Node s a]  -- Lista de noduri
limitedDfs st dep = if dep > 0 then (L.nub $ limitedDfsWithSet st dep S.empty) else [st]

{-
    Parcurge spatiul starilor in adancime pana ajunge la adancimea maxima si
    apoi continua pe urmatorul copil. Fiecare nod gasit il adauga in Set.
-}
limitedDfsWithSet :: (ProblemState s a, Ord s) 
                    => Node s a
                    -> Int
                    -> S.Set s
                    -> [Node s a]
limitedDfsWithSet st depMax set = [] ++ (if (depMax == (depth st))
                      then [st]
                      else [st] ++ concat [(if (S.notMember (state toExplore) set)
                                  then limitedDfsWithSet toExplore depMax (S.insert (state toExplore) set)
                                  else []) | toExplore <- (children st)])
{-
limitedDfsWithSet st depMax set = [] ++ (if (depMax == (depth st))
                      then (S.toList (S.insert st set))
                      else concat [(if (S.notMember toExplore set)
                                  then limitedDfsWithSet toExplore depMax (S.insert toExplore set)
                                  else []) | toExplore <- (children st)])
-}
{-
    Functia pune in done mai intai nodul gasit ca fiind goal si adancimea la care a fost gasit
    apoi construieste rezultatul: pune nodul si calculeaza lungimea apelurilor pana in acel punct,
    la acestea el aduna apoi numarul de noduri cercetate pana sa se dea de goal si 
    scade 1 ca este fara acesta
-}
iterativeDeepening :: (ProblemState s a, Ord s)
    => Node s a         -- Nodul stării inițiale
    -> (Node s a, Int)  -- (Nod cu prima stare finală,
                        --  număr de stări nefinale vizitate)
iterativeDeepening nod = let done = (head $ concat [[(x, dep)
                                    | x <- (limitedDfs nod dep), ((isGoal $ state x) == True)] 
                                    | dep <- [0..100]])
                        in (fst done, (length $ concat 
                        [limitedDfs nod dep | dep <- [0..((snd done) - 1)]]) - 1 +
                        (length [ceva | ceva <- (limitedDfs nod (snd done)),
                                                (isGoal $ state ceva) /= True]))

{-
    Functia mai intai creeaza o lista infinita cu toti parintii nodului dat si
    apoi ii ia pe primii "cat e adancimea nodului". Ea creeaza o lista cu
    perechile cerute si apoi o inverseaza ca sa fie de la radacina incolo.   
-}
extractPath :: Node s a -> [(a, s)]
extractPath nod  = reverse [(action x, state x) | x <- (take (depth nod) (iterate parent nod))]

{-
    Verifica mai intai daca se doreste euristica sau nu si apoi apeleaza toate
    functiile create pentru a gasi drumul minim
-}
solve :: (ProblemState s a, Ord s, Ord a)
      => s          -- Starea inițială de la care se pornește 
      -> Bool       -- Dacă să folosească sau nu euristica dată 
      -> [(a, s)]   -- Lista perechilor
solve initial heur = if heur 
                     then extractPath $ fst $ iterativeDeepening
                          $ orderStateSpace $ createStateSpace initial
                     else extractPath $ fst $ iterativeDeepening
                          $ createStateSpace initial

printSpacedList :: Show a => [a] -> IO ()
printSpacedList = mapM_ (\a -> print a >> putStrLn (replicate 20 '*'))
