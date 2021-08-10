{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import ProblemState
import qualified Data.PSQueue as PQ
import Data.Maybe
import Prelude
import qualified Data.Set as S

{-
    *** TODO ***
    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:
    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime;
    * estimarea costului până la starea finală;
    * copiii, ce vor desemna stările învecinate;
-}

data Node s a = Node 
 { state :: s
 , action :: Maybe a
 , parent :: Maybe (Node s a)
 , depth :: Int
 , heuristic :: Float
 , children :: [Node s a]
} deriving (Show)

{-
    *** TODO ***
    Instanțiați Eq și Ord pe baza stării.
-}

instance Eq s => Eq (Node s a) where
    (Node st1 ac1 p1 d1 h1 c1) == (Node st2 ac2 p2 d2 h2 c2) = st1 == st2

instance Ord s => Ord (Node s a) where
    (Node st1 ac1 p1 d1 h1 c1) <= (Node st2 ac2 p2 d2 h2 c2) = st1 <= st2

{-
    *** TODO ***
    Gettere folosite pentru accesul la câmpurile nodului
-}

nodeState :: Node s a -> s
nodeState (Node st ac p d h c) = st

nodeParent :: Node s a -> Maybe (Node s a)
nodeParent (Node st ac p d h c) = p

nodeDepth :: Node s a -> Int
nodeDepth (Node st ac p d h c) = d

nodeChildren :: Node s a -> [Node s a]
nodeChildren (Node st ac p d h c) = c

nodeHeuristic :: Node s a -> Float
nodeHeuristic (Node st ac p d h c) = h

nodeAction :: Node s a -> Maybe a
nodeAction (Node st ac p d h c) = ac

{-
    *** TODO ***
    Generarea întregului spațiu al stărilor.
    Primește starea inițială și creează nodul corespunzător acestei stări,
    având drept copii nodurile succesorilor stării curente, și așa mai
    departe, recursiv.
-}
createStateSpaceAux :: (ProblemState s a, Eq s) => s -> Node s a -> Int -> a -> Node s a
createStateSpaceAux state parent depth action = Node state (Just action) (Just parent) (depth + 1) (h state) (map (\(dire, st) -> (createStateSpaceAux st (createStateSpaceAux state parent depth action) (depth + 1) dire)) (successors state))

createStateSpace :: (ProblemState s a, Eq s) => s -> Node s a
createStateSpace initialState = Node initialState Nothing Nothing 0 (h initialState) (map (\(dire, st) -> (createStateSpaceAux st (createStateSpace initialState) 0 dire)) (successors initialState))

{-
    Funcție ce primește o coadă de priorități și întoarce o pereche
    formată din cheia cu prioritatea minimă și coada din care a fost ștearsă
    aceasta.
    Hint: O puteți folosi pentru a extrage și a șterge un nod din frontieră.
-}

deleteFindMin :: (Ord k, Ord p) => (PQ.PSQ k p) -> (k, (PQ.PSQ k p))
deleteFindMin pq = (minK, pq')
    where minK = PQ.key $ fromJust $ PQ.findMin pq
          pq' = PQ.deleteMin pq

{-
    *** TODO ***
    Primește nodul curent și mulțimea stărilor vizitate și întoarce
    o listă cu nodurile succesor nevizitate, care ar putea fi introduse
    în frontieră.
-}

suitableSuccs :: (ProblemState s a, Ord s) => Node s a -> (S.Set s) -> [Node s a]
suitableSuccs node visited = filter (\nod -> not (S.member (state nod) visited)) (children node)

{-
    *** TODO ***
    Primește o frontieră (o coadă de priorități) și un nod ce trebuie inserat în aceasta,
    întorcând o nouă frontieră.
    ATENȚIE: Dacă la introducerea unui nod există deja în frontieră un alt nod cu aceeași
    stare, dar cu cost mai mare, nodul nou, cu cost mai mic îl va înlocui pe cel vechi.
    
    Hints:
    1. Vedeți funcția insertWith din pachetul PSQueue.
        (https://hackage.haskell.org/package/PSQueue-1.1.0.1/docs/Data-PSQueue.html#v:insertWith)
    2. Costul se calculează ca suma dintre adâncime și euristică.
-}

insertSucc :: (ProblemState s a, Ord s) => (PQ.PSQ (Node s a) Float) -> Node s a -> PQ.PSQ (Node s a) Float
insertSucc frontier node -- newFrontier
 | PQ.lookup node frontier == Nothing = PQ.insert node ((nodeHeuristic node) + fromIntegral (nodeDepth node)) frontier
 | otherwise = if (((nodeHeuristic node) + fromIntegral (nodeDepth node)) < (fromJust (PQ.lookup node frontier))) then (PQ.insert node ((nodeHeuristic node) + fromIntegral (nodeDepth node)) (PQ.delete node frontier)) else frontier
{-
    *** TODO ***
    Primește nodul curent, frontiera și mulțimea stărilor vizitate, întorcând noua
    frontieră (coadă de priorități) în care au fost adăugate nodurile succesor validate
    de suitableSuccs.
-}

insertSuccs :: (ProblemState s a, Ord s) => (Node s a) -> (PQ.PSQ (Node s a) Float) -> (S.Set s) -> (PQ.PSQ (Node s a) Float)
insertSuccs node frontier visited = foldl (\f n -> insertSucc f n) frontier (suitableSuccs node visited) --newFrontier

{-
    *** TODO ***
    Funcție helper care implementează A-star.
    Primește o mulțime de noduri vizitate și o coadă de priorități (aka frontiera) și
    întoarce starea finală.
    Se procedează astfel până la întâlnirea unei stări scop:
        - se extrage un nod adecvat din frontireră
        - se marchează starea acestuia ca fiind vizitată
        - se introduc succesorii în frontieră
-}

astar' :: (ProblemState s a, Ord s) => (S.Set s) -> (PQ.PSQ (Node s a) Float) -> Node s a
astar' visited frontier -- goalNode
 | bind == Nothing = undefined
 | otherwise = if (isGoal (state (PQ.key (fromJust bind)))) then (PQ.key (fromJust bind)) else astar' (S.insert (state (PQ.key (fromJust bind))) visited) (insertSuccs (PQ.key (fromJust bind)) (PQ.deleteMin frontier) visited)
 where bind = PQ.findMin frontier
{-
    *** TODO ***
  
    Primește starea inițială și întoarce starea finală pentru o singură aplicare
    a algoritmului.
    Asigură parametrii inițiali corecți pentru aplicarea funcției astar'.
-}

astar :: (ProblemState s a, Ord s) => Node s a -> Node s a
astar initialNode = astar' (S.empty) (PQ.insert initialNode ((nodeHeuristic initialNode) + fromIntegral (nodeDepth initialNode)) (PQ.empty)) -- goalNode

{-
    *** TODO ***
    Pornind de la un nod, reface parțial calea către nodul inițial, urmând legăturile
    către părinți.
    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea următoare
    stării inițiale și se încheie la starea finală.
    ATENȚIE: Nodul inițial este singurul exclus!
-}
generateParents :: (ProblemState s a, Ord s) => Node s a -> [Node s a]
generateParents goalNode
 | (nodeParent goalNode) == Nothing = []
 | otherwise = generateParents (fromJust (nodeParent goalNode)) ++ [goalNode]

extractPath :: (ProblemState s a, Ord s) => Node s a -> [(a, s)]
extractPath goalNode = foldl (\acc e -> acc ++ [((fromJust (nodeAction e)), (nodeState e))]) [] (generateParents goalNode)
