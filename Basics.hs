{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}

module Basics where
{-
    Expune funcțiile necesare reprezentării jocului.
-}

import ProblemState
import Data.List
import Data.Maybe

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc.
    Colțul stânga-sus este (0, 0).
-}
type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea Target-urilor.
    Acestea conțin informații atât despre poziția curentă a
    Target-ului cât și despre comportamentul acestuia.
    Tipul Behavior este definit mai jos.
-}
data Target = Target {
    position :: Position,
    behavior :: Behavior
}

targetAsString :: Target -> String
targetAsString (Target (x, y) behav) = show (x, y)

instance Show Target where
    show = targetAsString

instance Eq Target where
    Target p1 _ == Target p2 _ = p1 == p2

instance Ord Target where
    Target p1 _ <= Target p2 _ = p1 <= p2

{-
    Tip de date pentru reprezentarea comportamentului unui Target.
    Tipul Behavior este utilizat pentru a modela tranziția Target-urilor
    din starea curentă în starea următoare. Primul parametru este poziția
    actuală a target-ului, iar al doilea, starea curentă a jocului.
    Tipul Game este definit mai jos.
    
    Observați că, din moment ce un Behavior produce un Target nou,
    acesta din urmă ar putea fi caracterizat de un alt Behavior
    decât cel anterior.
-}
type Behavior = Position -> Game -> Target

{-
    Direcțiile de deplasare pe tablă
-}
data Direction = North | South | West | East
    deriving (Eq, Show)

{-
    *** TODO ***
    
    Tip de date pentru reprezentarea stării jocului, la un anumit
    moment. Completați-l cu orice informație aveți nevoie pentru
    stocarea stării jocului (hunter, target, obstacole, gateways).
-}
data Gateway = Gateway {
    position1 :: Position,
    position2 :: Position
} deriving (Eq, Ord)

gateString :: Gateway -> String
gateString (Gateway (x1, y1) (x2, y2)) = show (x1, y1) ++ "-" ++ show (x2, y2)

instance Show Gateway where
    show = gateString
-- instance Eq Gateway where
--     Gateway p1 _ == Gateway p2 _ = p1 == p2

-- instance Ord Gateway where
--     Gateway p1 _ <= Gateway p2 _ = p1 <= p2

data Game = Game 
 { hunter :: Position
 , target :: [Target]
 , obstacles :: [Position]
 , gateways :: [Gateway]
 , rows :: Int
 , cols :: Int
 } deriving (Eq, Ord)
{-
    *** Optional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Game.
    În cazul acesta, eliminați deriving (Eq, Ord) din Game.
-}

{-
    *** TODO ***

    Reprezentați starea jocului ca șir de caractere, pentru afișarea
    la consolă.
    
    Atenție! Fiecare linie, mai puțin ultima, este urmată de \n.
    Celule goale vor fi reprezentate ca ' '.
    Hunter-ul va fi reprezentat ca '!'.
    Target-urile vor fi reprezentate ca '*'
    Gateways-urile vor fi reprezentate ca '#'.
    Obstacolele vor fi reprezentate de '@'.

    Hint: S-ar putea să vă fie utile list comprehensions,
    precum și funcțiile elem, any și intercalate din Data.List.
-}
gameAsStringAux :: String -> Game -> (Int, Int) -> String
gameAsStringAux acc (Game hun t o g r c) (currentX, currentY)
 | currentX == (r - 1) && currentY == (c - 1) = acc ++ "@"
 | currentY == c = acc ++ "\n"
 | hun == (currentX, currentY) = acc ++ "!"
 | elem (currentX, currentY) o = acc ++ "@"
 | any (\(Target pos behav) -> pos == (currentX, currentY)) t = acc ++ "*"
 | any (\(Gateway pos1 pos2) -> pos1 == (currentX, currentY) || pos2 == (currentX, currentY)) g = acc ++ "#"
 | otherwise = acc ++ " "

gameAsString :: Game -> String
gameAsString (Game hun t o g r c) = foldl (\acc e -> gameAsStringAux acc (Game hun t o g r c) e) "" (take (r * (c + 1) - 1) [(x, y) | x<-[0..(r - 1)], y<-[0..c]])

instance Show Game where
    show = gameAsString

{-
    *** TODO ***
    
    Primește numărul de linii și numărul de coloane ale tablei de joc.
    Intoarce un obiect de tip Game în care tabla conține spații goale în interior, fiind
    împrejmuită de obstacole pe toate laturile. Implicit, colțul din stânga sus este (0,0),
    iar Hunterul se găsește pe poziția (1, 1).
-}
emptyGameAux :: Game -> Int -> Int -> Game
emptyGameAux (Game hun t o g r c) currentX currentY
 | currentX == (r - 1) && currentY == (c - 1) = (Game hun t (o ++ [(currentX, currentY)]) g r c)
 | currentY == (c - 1) = emptyGameAux (Game hun t (o ++ [(currentX, currentY)]) g r c) (currentX + 1) 0
 | currentY == 0 = emptyGameAux (Game hun t (o ++ [(currentX, currentY)]) g r c) currentX (currentY + 1)
 | currentX == 0 = emptyGameAux (Game hun t (o ++ [(currentX, currentY)]) g r c) currentX (currentY + 1)
 | currentX == (r - 1) = emptyGameAux (Game hun t (o ++ [(currentX, currentY)]) g r c) currentX (currentY + 1)
 | otherwise = emptyGameAux (Game hun t o g r c) currentX (currentY + 1)

emptyGame :: Int -> Int -> Game
emptyGame x y = emptyGameAux (Game (1,1) [] [] [] x y) 0 0

{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, cu Hunter-ul pus
    pe poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugat Hunter-ul
    Daca poziția este invalidă (ocupată sau în afara tablei de joc) se va întoarce
    același joc.
-}
addHunter :: Position -> Game -> Game
addHunter (x, y) (Game hun t o g r c)
 | x >= 1 && y >= 1 && x < (r - 1) && y < (c - 1) = (Game (x, y) t o g r c)
 | otherwise = (Game hun t o g r c)

{-
    *** TODO ***

    Primește un comportament, o poziție și un joc și întoarce un nou joc, în care a fost
    adăugat Target-ul descris de comportament și poziție.
    Parametrul Behavior reprezintă comportamentul Hunter-ului care va fi adăugat.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat Target-ul.
-}
addTarget :: Behavior -> Position -> Game -> Game
addTarget behav (x, y) (Game hun t o g r c) = (Game hun (t ++ [(Target (x,y) behav)]) o g r c)

{-
    *** TODO ***

    Primește o pereche de poziții și un joc și întoarce un nou joc, în care au fost adăugate
    cele două gateway-uri interconectate.
    Parametrul (Position, Position) reprezintă pozițiile de pe hartă la care vor fi adăugate 
    cele două gateway-uri interconectate printr-un canal bidirecțional.
-}
addGateway :: (Position, Position) -> Game -> Game
addGateway ((x1,y1),(x2,y2)) (Game hun t o g r c) = (Game hun t o (g ++ [(Gateway (x1, y1) (x2, y2))]) r c)

{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, în care a fost adăugat un obstacol
    la poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat obstacolul.
-}
addObstacle :: Position -> Game -> Game
addObstacle (x, y) (Game hun t o g r c) = (Game hun t (o ++ [(x, y)]) g r c)

{-
    *** TODO ***
    
    Primește o poziție destinație înspre care vrea să se deplaseze o entitate (Hunter sau Target)
    și verifică daca deplasarea este posibilă, întorcând noua poziție, luând în considerare
    și Gateway-urile.
    Avem următoarele cazuri:
    - dacă poziția corespunde unui spațiu gol, se întoarce acea poziție;
    - dacă poziția corespunde unui gateway, se întoarce poziția gateway-ului pereche;
    - dacă poziția corespunde unui obstacol, se întoarce Nothing.
    Parametrul Position reprezintă poziția destinație.
-}
attemptMove :: Position -> Game -> Maybe Position
attemptMove (x, y) (Game hun t o g r c)
 | any (\(Gateway pos1 pos2) -> pos1 == (x, y)) g = Just (head (map (\(Gateway pos1 pos2) -> pos2) (filter (\(Gateway pos1 pos2) -> pos1 == (x, y)) g)))
 | any (\(Gateway pos1 pos2) -> pos2 == (x, y)) g = Just (head (map (\(Gateway pos1 pos2) -> pos1) (filter (\(Gateway pos1 pos2) -> pos2 == (x, y)) g)))
 | elem (x, y) o = Nothing
 | otherwise = Just (x, y)

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre est. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
    
    Conform definiției, tipul Behavior corespunde tipului funcție
    Position -> Game -> Target.
    
    Având în vedere că cele patru funcții definite în continuare (goEast, goWest,
    goNorth, goSouth) sunt foarte similare, încercați să implementați o funcție
    mai generală, pe baza căreia să le definiți apoi pe acestea patru.
-}
goEast :: Behavior
goEast (x, y) (Game hun t o g r c)
 | attemptMove (x, y + 1) (Game hun t o g r c) == Nothing = (Target (fromJust (attemptMove (x, y) (Game hun t o g r c))) goEast)
 | otherwise = (Target (fromJust (attemptMove (x, y + 1) (Game hun t o g r c))) goEast)

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre vest. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goWest :: Behavior
goWest (x, y) (Game hun t o g r c)
 | attemptMove (x, y - 1) (Game hun t o g r c) == Nothing = (Target (fromJust (attemptMove (x, y) (Game hun t o g r c))) goWest)
 | otherwise = (Target (fromJust (attemptMove (x, y - 1) (Game hun t o g r c))) goWest)

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre nord. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goNorth :: Behavior
goNorth (x, y) (Game hun t o g r c)
 | attemptMove (x - 1, y) (Game hun t o g r c) == Nothing = (Target (fromJust (attemptMove (x, y) (Game hun t o g r c))) goNorth)
 | otherwise = (Target (fromJust (attemptMove (x - 1, y) (Game hun t o g r c))) goNorth)

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre sud. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goSouth :: Behavior
goSouth (x, y) (Game hun t o g r c)
 | attemptMove (x + 1, y) (Game hun t o g r c) == Nothing = (Target (fromJust (attemptMove (x, y) (Game hun t o g r c))) goSouth)
 | otherwise = (Target (fromJust (attemptMove (x + 1, y) (Game hun t o g r c))) goSouth)

{-
    *** TODO ***

    Comportamentul unui Target de a-și oscila mișcarea, când înspre nord, când înspre sud. 
    Mișcarea se poate face doar dacă poziția este validă (se află pe tablă de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul iși va schimba
    direcția de mers astfel:
    - daca mergea inspre nord, își va modifica direcția miscării înspre sud;
    - daca mergea inspre sud, își va continua mișcarea înspre nord.
    Daca Target-ul întâlneste un Gateway pe traseul său, va trece prin acesta,
    către Gateway-ul pereche conectat și își va continua mișcarea în același sens la ieșire
    din acesta.
    Puteți folosit parametrul Int pentru a surprinde deplasamentul Target-ului (de exemplu,
    1 pentru sud, -1 pentru nord).
-}
bounce :: Int -> Behavior
bounce direction (x, y) (Game hun t o g r c)
 | direction == 1 = if attemptMove (x + 1, y) (Game hun t o g r c) == Nothing then (if attemptMove (x - 1, y) (Game hun t o g r c) == Nothing then (Target (fromJust (attemptMove (x, y) (Game hun t o g r c))) $ bounce (-1)) else (Target (fromJust (attemptMove (x - 1, y) (Game hun t o g r c))) $ bounce (-1))) else (Target (fromJust (attemptMove (x + 1, y) (Game hun t o g r c))) $ bounce 1)
 | direction == -1 = if attemptMove (x - 1, y) (Game hun t o g r c) == Nothing then (if attemptMove (x + 1, y) (Game hun t o g r c) == Nothing then (Target (fromJust (attemptMove (x, y) (Game hun t o g r c))) $ bounce 1) else (Target (fromJust (attemptMove (x + 1, y) (Game hun t o g r c))) $ bounce 1)) else (Target (fromJust (attemptMove (x - 1, y) (Game hun t o g r c))) $ bounce (-1))

{-
    *** TODO ***
    Funcție care mută toate Target-urile din Game-ul dat o poziție, în functie
    de behavior-ul fiecăreia și întoarce noul Game în care pozițiile Target-urilor
    sunt actualizate.
    
-}

moveTargets :: Game -> Game
moveTargets (Game hun t o g r c) = (Game hun (map (\(Target pos behav) -> (behav pos (Game hun t o g r c))) t) o g r c)

{-
    *** TODO ***

    Verifică dacă Targetul va fi eliminat de Hunter.
    Un Target este eliminat de Hunter daca se află pe o poziție adiacentă
    cu acesta.
    Parametrul Position reprezintă poziția Hunterului pe tabla
    de joc.
    Parametrul Target reprezintă Targetul pentru care se face verificarea.
-}
isTargetKilled :: Position -> Target -> Bool
isTargetKilled (x1, y1) (Target (x2, y2) behav) = ((x2 - x1)*(x2 - x1) + (y2 - y1)*(y2 - y1)) <= 1


{-
    *** TODO ***

    Avansează starea jocului curent, rezultând starea următoare a jocului.
    Parametrul Direction reprezintă direcția în care se va deplasa Hunter-ul.
    Parametrul Bool specifică dacă, după mutarea Hunter-ului, vor fi
    mutate și Target-urile sau nu, și dacă vor fi eliminate din joc sau nu.
    Este folosit pentru a distinge între desfășurarea reală a jocului (True)
    și planificarea „imaginată” de hunter (False) în partea a doua a temei.

    Avansarea stării jocului respectă următoarea ordine:
    1. Se deplasează Hunter-ul.
    2. În funcție de parametrul Bool, se elimină Target-urile omorâte de către Hunter.
    3. In funcție de parametrul Bool, se deplasează Target-urile rămase pe tablă.
    4. Se elimină Targeturile omorâte de către Hunter și după deplasarea acestora.
    
    Dubla verificare a anihilării Target-urilor, în pașii 2 și 4, îi oferă Hunter-ului
    un avantaj în prinderea lor.
-}

eliminateTargets :: Game -> Game
eliminateTargets (Game (x, y) t o g r c) = (Game (x, y) (filter (\target -> not (isTargetKilled (x, y) target)) t) o g r c)

advanceGameState :: Direction -> Bool -> Game -> Game
advanceGameState direction decision (Game (x, y) t o g r c)
 | decision == False = case direction of
     East -> if attemptMove (x, y + 1) (Game (x, y) t o g r c) == Nothing then (Game (fromJust (attemptMove (x, y) (Game (x, y) t o g r c))) t o g r c) else (Game (fromJust (attemptMove (x, y + 1) (Game (x, y) t o g r c))) t o g r c)
     West -> if attemptMove (x, y - 1) (Game (x, y) t o g r c) == Nothing then (Game (fromJust (attemptMove (x, y) (Game (x, y) t o g r c))) t o g r c) else (Game (fromJust (attemptMove (x, y - 1) (Game (x, y) t o g r c))) t o g r c)
     North -> if attemptMove (x - 1, y) (Game (x, y) t o g r c) == Nothing then (Game (fromJust (attemptMove (x, y) (Game (x, y) t o g r c))) t o g r c) else (Game (fromJust (attemptMove (x - 1, y) (Game (x, y) t o g r c))) t o g r c)
     South -> if attemptMove (x + 1, y) (Game (x, y) t o g r c) == Nothing then (Game (fromJust (attemptMove (x, y) (Game (x, y) t o g r c))) t o g r c) else (Game (fromJust (attemptMove (x + 1, y) (Game (x, y) t o g r c))) t o g r c)
 | decision == True = case direction of
     East -> if attemptMove (x, y + 1) (Game (x, y) t o g r c) == Nothing then (eliminateTargets (moveTargets (Game (fromJust (attemptMove (x, y) (Game (x, y) t o g r c))) (filter (\target -> not (isTargetKilled (x, y) target)) t) o g r c))) else (eliminateTargets (moveTargets (Game (fromJust (attemptMove (x, y + 1) (Game (x, y) t o g r c))) (filter (\target -> not (isTargetKilled (x, y + 1) target)) t) o g r c)))
     West -> if attemptMove (x, y - 1) (Game (x, y) t o g r c) == Nothing then (eliminateTargets (moveTargets (Game (fromJust (attemptMove (x, y) (Game (x, y) t o g r c))) (filter (\target -> not (isTargetKilled (x, y) target)) t) o g r c))) else (eliminateTargets (moveTargets (Game (fromJust (attemptMove (x, y - 1) (Game (x, y) t o g r c))) (filter (\target -> not (isTargetKilled (x, y - 1) target)) t) o g r c)))
     North -> if attemptMove (x - 1, y) (Game (x, y) t o g r c) == Nothing then (eliminateTargets (moveTargets (Game (fromJust (attemptMove (x, y) (Game (x, y) t o g r c))) (filter (\target -> not (isTargetKilled (x, y) target)) t) o g r c))) else (eliminateTargets (moveTargets (Game (fromJust (attemptMove (x - 1, y) (Game (x, y) t o g r c))) (filter (\target -> not (isTargetKilled (x - 1, y) target)) t) o g r c)))
     South -> if attemptMove (x + 1, y) (Game (x, y) t o g r c) == Nothing then (eliminateTargets (moveTargets (Game (fromJust (attemptMove (x, y) (Game (x, y) t o g r c))) (filter (\target -> not (isTargetKilled (x, y) target)) t) o g r c))) else (eliminateTargets (moveTargets (Game (fromJust (attemptMove (x + 1, y) (Game (x, y) t o g r c))) (filter (\target -> not (isTargetKilled (x + 1, y) target)) t) o g r c)))

{-
    ***  TODO ***

    Verifică dacă mai există Target-uri pe table de joc.
-}
areTargetsLeft :: Game -> Bool
areTargetsLeft (Game hun t o g r c) = length t /= 0

{-
    *** BONUS TODO ***

    Comportamentul unui Target de a se deplasa în cerc, în jurul unui Position, având
    o rază fixată.
    Primul parametru, Position, reprezintă centrul cercului.
    Parametrul Int reprezintă raza cercului.
    Puteți testa utilizând terenul circle.txt din directorul terrains, în conjuncție
    cu funcția interactive.
-}
circle :: Position -> Int -> Behavior
circle (centerX, centerY) radius (x, y) (Game hun t o g r c)
 | x < centerX && y <= centerY = if hEuclidean (centerX, centerY) (x, y) <= fromIntegral (radius * radius) then Target (x, y - 1) $ circle (centerX, centerY) radius else Target (x + 1, y) $ circle (centerX, centerY) radius
 | x >= centerX && y < centerY = if hEuclidean (centerX, centerY) (x, y) <= fromIntegral (radius * radius) then Target (x + 1, y) $ circle (centerX, centerY) radius else Target (x, y + 1) $ circle (centerX, centerY) radius
 | x > centerX && y >= centerY = if hEuclidean (centerX, centerY) (x, y) <= fromIntegral (radius * radius) then Target (x, y + 1) $ circle (centerX, centerY) radius else Target (x - 1, y) $ circle (centerX, centerY) radius
 | x <= centerX && y > centerY = if hEuclidean (centerX, centerY) (x, y) <= fromIntegral (radius * radius) then Target (x - 1, y) $ circle (centerX, centerY) radius else Target (x, y - 1) $ circle (centerX, centerY) radius

instance ProblemState Game Direction where
    {-
        *** TODO ***
        
        Generează succesorii stării curente a jocului.
        Utilizați advanceGameState, cu parametrul Bool ales corespunzător.
    -}
    successors (Game hun t o g r c) = [(North, (advanceGameState North False (Game hun t o g r c)))] ++ [(South, (advanceGameState South False (Game hun t o g r c)))] ++ [(East, (advanceGameState East False (Game hun t o g r c)))] ++ [(West, (advanceGameState West False (Game hun t o g r c)))]

    {-
        *** TODO ***
        
        Verifică dacă starea curentă este un în care Hunter-ul poate anihila
        un Target. Puteți alege Target-ul cum doriți, în prezența mai multora.
    -}
    isGoal (Game hun t o g r c) = length (filter (\target -> isTargetKilled hun target) t) /= 0

    {-
        *** TODO ***
        
        Euristica euclidiană (vezi hEuclidian mai jos) până la Target-ul ales
        de isGoal.
    -}
    h (Game hun t o g r c) = hEuclidean hun (position (head (t)))
{-
     ** NU MODIFICATI **
-}
hEuclidean :: Position -> Position -> Float
hEuclidean (x1, y1) (x2, y2) = fromIntegral $ ((x1 - x2) ^ pow) + ((y1 - y2) ^ pow)
  where
    pow = 2 :: Int

{-
    *** BONUS ***

    Acesta reprezintă un artificiu necesar pentru testarea bonusului,
    deoarece nu pot exista două instanțe diferite ale aceleiași clase
    pentru același tip.

    OBSERVAȚIE: Testarea bonusului pentru Seach este făcută separat.
-}

newtype BonusGame = BonusGame Game
    deriving (Eq, Ord, Show)

{-
    *** BONUS TODO ***

    Folosind wrapper-ul peste tipul Game de mai sus instanțiați
    ProblemState astfel încât să fie folosită noua euristică. 
-}
instance ProblemState BonusGame Direction where
    {-
        *** BONUS TODO ***

        Pentru a ne asigura că toțî succesorii unei stări sunt de tipul
        BonusGame și folosesc noua euristică trebuie să aplicăm wrapper-ul
        definit mai sus peste toți succesorii unei stări.

        Hint: Puteți să folosiți funcția fmap pe perechi pentru acest lucru.
        https://wiki.haskell.org/Functor
    -}
    successors (BonusGame (Game hun t o g r c)) = [(North, BonusGame (advanceGameState North False (Game hun t o g r c)))] ++ [(South, BonusGame (advanceGameState South False (Game hun t o g r c)))] ++ [(East, BonusGame (advanceGameState East False (Game hun t o g r c)))] ++ [(West, BonusGame (advanceGameState West False (Game hun t o g r c)))]

    {-
        *** BONUS TODO ***

        Definiți funcția isGoal pentru BonusGame.

        Hint: Folosiți funcția isGoal deja implementată pentru tipul Game.
    -}
    isGoal (BonusGame (Game hun t o g r c)) = length (filter (\target -> isTargetKilled hun target) t) /= 0

    {-
        *** BONUS TODO ***

        Definiți o funcție euristică care este capabilă să găsească un drum mai scurt
        comparativ cu cel găsit de euristica implementată pentru Game.

        ATENȚIE: Noua euristică NU trebuie să fie una trivială.

        OBSERVAȚIE: Pentru testare se va folosi fișierul terrains/game-6.txt.
    -}
    h (BonusGame (Game hun t o g r c)) = if length (filter (\(Target pos behav) -> isTargetKilled hun (behav pos (Game hun t o g r c))) t) == 0 then 1000 else hEuclidean hun (position (head (filter (\(Target pos behav) -> isTargetKilled hun (behav pos (Game hun t o g r c))) t)))
    -- sau mai putin destept (fara sa considere o miscare in fata a targetului)
    -- h (BonusGame (Game hun t o g r c)) = if length (filter (\target -> isTargetKilled hun target) t) == 0 then 1000 else hEuclidean hun (position (head (filter (\target -> isTargetKilled hun target) t)))