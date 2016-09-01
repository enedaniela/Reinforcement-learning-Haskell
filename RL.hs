module RL where

import Data.Array 
import Data.Maybe
import Data.List
import System.Random
import Text.Printf
import qualified Data.List.Split as Chunks
import Data.Function

{-
    O stare este reprezentată ca un număr întreg.
    O cale este o listă de stări, eventual infinită.

    O estimare reprezintă o mulțime de asocieri (stare, informație aferentă).
    Cu toate că mediul este bidimensional, utilizăm o reprezentare liniară
    a acestuia, bazată pe un `Array`, cu stările indexate ca în figura din 
    enunț.
-}
type State      = Int
type Path       = [State]
type Estimation = Array State StateInfo

{-
    Lățimea și înălțimea mediului, precum și numărul de stări.
-}
width, height, nStates :: Int
width   = 4
height  = 3
nStates = width * height

{-
    Perechile de stări vecine.
-}
neighbors :: [(State, State)]
neighbors = [ (1, 2), (1, 5)
            , (2, 1), (2, 3)
            , (3, 2), (3, 4)
            , (3, 7)
            , (4, 3), (4, 8)
            , (5, 1), (5, 9)
            , (7, 3), (7, 8), (7, 11)
            , (8, 4), (8, 7), (8, 12)
            , (9, 5), (9, 10)
            , (10, 9), (10, 11)
            , (11, 7), (11, 10), (11, 12)
            , (12, 8), (12, 11)
            ]

{-
    Starea de pornire.
-}
startState :: State
startState = 1

{-
     Stările terminale.
-}
terminalStates :: [State]
terminalStates = [8, 12]

{-
    Rata de învățare alfa și factorul de scalare a acesteia.
-}
learningRate, scaleFactor :: Float
learningRate = 0.1
scaleFactor  = 0.999

-------------------------------------------------------------------------------
-- Completați sub această linie.


--  === 1. Generarea căilor ===

{-
    *** TODO ***

    Întoarce toate stările vecine ale unei stări.
-}
neighborsOf :: State -> [State]
neighborsOf = \state -> map snd (filter (\x -> (fst x) == state) neighbors)

{-
    *** TODO ***

    Construiește o cale aleatoare infinită, pe baza unui generator.

    Hint: `Data.List.iterate`, `System.Random.split`.
-}
 
randomIndex len g = randomR (0, len - 1) g

randomPathMatch g = iterate (\(state, gen) -> (((neighborsOf state) !! fst (randomIndex (length (neighborsOf state)) gen)), (snd (next gen)))) (startState, g)

randomPath :: RandomGen g => g -> (Path, g)
randomPath g = (map fst (randomPathMatch (fst (split g))), snd (split g))

{-
    *** TODO ***

    Trunchiază o cale, eventual infinită, la prima stare terminală.
-}

terminatePath :: Path -> Path
terminatePath path = take (length (takeWhile (\x -> (x/=12) && (x/=8)) path) + 1) path

{-
    *** TODO ***

    Construiește o infinitate de căi infinite.
-}

randomPathsMatch g = iterate (\(path, gen) -> ((fst (randomPath gen)), (snd (next gen)))) (randomPath g)

randomPaths :: RandomGen g => g -> [Path]
randomPaths g = map fst (randomPathsMatch g)


--  === 2. Estimarea utilităților fără diminuarea ratei de învățare ===

{-
    *** TODO ***

    Array cu conscințele specifice fiecărei stări.
-}

reinforcements :: Array State Float
reinforcements = array (1,12) [(1,0.0),(2,0.0),(3,0.0),(4,0.0),(5,0.0),(6,0.0),(7,0.0),(8,-1.0),(9,0.0),(10,0.0),(11,0.0),(12,1.0)]

{-
    *** TODO ***

    Valorile inițiale ale stărilor, înaintea rulării algoritmului.
    Se construiesc pe baza array-ului de consecințe.
-}

initialEstimation :: Estimation
initialEstimation = array (1,12) [(1,(StateInfo 0.0 0)),(2,(StateInfo 0.0 0)),(3,(StateInfo 0.0 0)),(4,(StateInfo 0.0 0)),(5,(StateInfo 0.0 0)),(6,(StateInfo 0.0 0)),(7,(StateInfo 0.0 0)),(8,(StateInfo (-1.0) 0)),(9,(StateInfo 0.0 0)),(10,(StateInfo 0.0 0)),(11,(StateInfo 0.0 0)),(12,(StateInfo 1.0 0))]

{-
    *** TODO ***

    Lista de utilități provenite dintr-o estimare.
-}

values :: Estimation -> [Float]
values estimare = [(est (estimare ! (fromIntegral i))) | i <- [1..nStates]]

{-
    *** TODO ***

    Reprezentarea sub formă de șir de caractere a unei estimări.
    Se va întrebuința forma bidimensională, ca în imaginile din enunț.
    De asemenea, utilitățile vor fi rotunjite la 2 zecimale, și vor
    avea semnul inclus.

    Hint: `Text.Printf`.

    Exemplu de rezultat:

    -0.07 +0.06 +0.20 +1.00
    -0.20 +0.00 -0.43 -1.00
    -0.32 -0.45 -0.56 -0.78

    Pentru a vizualiza corect caracterele de linie nouă, aplicați
    în interpretor funcția `putStrLn` asupra șirului obținut.
-}

partialList e = (Chunks.chunksOf width (values e))

str :: [Float] -> String
str l = printf "%+.2f %+.2f %+.2f %+.2f" (l!!0) (l!!1) (l!!2) (l!!3)

listOfRows e = map (\l -> (str l)) (partialList e)

showEstimation :: Estimation -> String
showEstimation e = intercalate "\n" (reverse (listOfRows e))

{-
    *** TODO ***

    Actualizează o estimare în urmare parcurgerii unei căi.

    Hint: `Data.Array.accum`.
-}

transitionList path = zipWith (\x -> \y -> (x,y)) path (tail path)

compute state e = e // (((fst state), (StateInfo (vs + ((tail scaledLearningRates)!!viz) * (rs + vsnext - vs)) (viz + 1))):[])
                    where vs = est (e!(fst state))
                          vsnext = est (e!(snd state))
                          rs = reinforcements!(fromIntegral (fst state))
                          viz = nr_viz (e!(fst state))

updateEstimation :: Estimation -> Path -> Estimation
updateEstimation e path = foldl (\est state -> compute state est) e (transitionList path)

{-
    *** TODO ***

    Obține un flux infinit de estimări rafinate succesiv, pe baza unui flux
    infinit de căi finite, încheiate în stări terminale.

    Hint: `Data.List.mapAccumL`.
-}

estimations :: [Path] -> [Estimation]
estimations pathList = snd (mapAccumL (\e path -> ((updateEstimation e path),(updateEstimation e path))) initialEstimation pathList)

{-
    *** TODO ***

    Determină estimarea de rang dat ca parametru, pe baza unui generator.
-}

estimate :: RandomGen g => Int -> g -> Estimation
estimate which g = (estimations (map terminatePath (randomPaths g)))!!which

{-
    *** TODO ***

    Pentru o stare, determină vecinul cu cea mai mare valoare estimată.

    Hint: `Data.Function.on`.
-}

assign e = [(i,e!i) | i <-[1..12]]

makeList e state = map (\x -> ((fst ((assign e)!!(x - 1))),(est (snd ((assign e)!!(x - 1)))))) (neighborsOf state)

bestNeighborOf :: State -> Estimation -> State
bestNeighborOf state e = (fst (head (sortBy ((flip compare) `on` snd) (makeList e state))))


{-
    *** TODO ***

    Contruiește o cale începută în starea inițială, pe principiul alegerii 
    vecinului cu utilitata maximă.
-}

bestPath :: Estimation -> Path
bestPath e = (iterate (\x -> (bestNeighborOf x e)) startState)


--  === 3. Estimarea utilităților cu diminuarea ratei de învățare ===

{-
    *** TODO ***

    Fluxul infinit al ratelor de învățare scalate:

    [ 1
    , learningRate
    , learningRate * scaleFactor
    , learningRate * scaleFactor^2
    , ...
    ]
-}

scaledLearningRates :: [Float]
scaledLearningRates = 1:(iterate (\x -> x * scaleFactor) learningRate)

{-
    *** TODO ***

    Tip de date pentru reținerea atât a valorii estimate a unei stări,
    cât și a numărului de vizitări ale acesteia.
-}
data StateInfo = StateInfo { est :: Float
                , nr_viz :: Int
            }deriving (Show)