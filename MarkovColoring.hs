module MarkovColoring where

import System.Random (randomRIO)
import Data.HashMap (Map, (!))
import Data.List (delete, union, (\\))
import qualified Data.HashMap as Map

type Vertex = Int

-- Graph defined using a map adjacency-list structure.
type Graph = Map Vertex [Vertex]

type Color = Int

type Coloring = Map Vertex Color

g :: Graph
g = Map.fromList [(1, [2]), (2, [1]), (3, [4]), (4, [3])]

cascade      :: (Monad m) => (a -> m a) -> a -> Int -> m a
cascade _ x 0 =  return x
cascade f x n =  f x >>= \x' -> cascade f x' (n - 1)

allColorings :: [Color] -> Graph -> [Coloring]
allColorings cs g = map Map.fromList $ listPower (Map.keys g) cs
  where
    -- Named listPower because the resulting list has q^n elements.
    listPower :: [Vertex] -> [Color] -> [[(Vertex, Color)]]
    listPower []     _  = [[]]
    listPower vs     [] = undefined
    listPower (v:vs) cs = concatMap (\c -> map ((v, c) :) (listPower vs cs)) cs

isValidColoring :: Graph -> Coloring -> Bool
isValidColoring g c = foldr (&&) True $ map (validVertex) (Map.keys g)
  where
    validVertex v = let vc = (c ! v) in
        foldr (&&) True [vc /= c ! u | u <- g ! v]

validColorings :: [Color] -> Graph -> [Coloring]
validColorings cs g = filter (isValidColoring g) (allColorings cs g)

generateColoring :: [Color] -> Graph -> Coloring
generateColoring cs g = head $ validColorings cs g

removeEdges :: Graph -> Vertex -> Graph
removeEdges g v = fmap (delete v) (Map.insert v [] g)

changeColoring :: [Color] -> Graph -> Coloring -> IO Coloring
changeColoring cs g coloring = do
    let vertices = Map.keys g
    idxV <- randomRIO (0, length vertices - 1)
    let v = vertices !! idxV
    let valid = cs \\ neighborColors v g coloring
    idxC <- randomRIO (0, length valid - 1)
    let c = valid !! idxC
    let coloring' = Map.insert v c coloring
    if isValidColoring g coloring' then return coloring' else return coloring

neighborColors :: Vertex -> Graph -> Coloring -> [Color]
neighborColors v g coloring = map (coloring !) (g ! v)

randomColoring :: [Color] -> Graph -> IO Coloring
randomColoring cs g = do
    let init = generateColoring cs g
    cascade (changeColoring cs g) init 100

countColorings :: [Color] -> Graph -> IO Float
countColorings cs g = do
    let vs = (Map.keys g)
    r <- aux vs g
    return (fromIntegral ((length cs)^(length vs)) / r)
  where
    aux :: [Vertex] -> Graph -> IO Float
    aux [] _ = return 1.0
    aux (v:vs) g = do
        let g' = removeEdges g v
        x <- rho cs g g' 100
        let y = 1 / ((fromIntegral x) / 100)
        z <- aux vs g'
        return (y * z)
        
rho :: [Color] -> Graph -> Graph -> Int -> IO Int
rho _ _ _ 0 = return 0 
rho cs g g' n = do
    coloring <- randomColoring cs g'
    let valid = isValidColoring g coloring
    if valid
       then (rho cs g g' (n - 1)) >>= (\x -> return (x + 1))
       else rho cs g g' (n - 1)

constructGraph :: [(Vertex, Vertex)] -> Graph
constructGraph [] = Map.empty
constructGraph ((a,b):edges) = Map.insertWith union a [b] g where
    g = Map.insertWith union b [a] $ constructGraph edges

readGraph :: String -> IO (Int, Int, Int, Graph)
readGraph fileName = do
    fileContents <- readFile fileName
    let (firstLine:ls) = lines fileContents
    let [n, m, q] = parseArgs firstLine
    return (n, m, q, constructGraph (map parseEdge ls))
  where
    parseArgs s = map read $ words s
    parseEdge s = case parseArgs s of
        [a, b] -> (a, b)
        _      -> error "Bad edge."

main = do
    let colors = [6, 9]
    let colorings = allColorings colors g
    let valid = filter (isValidColoring g) colorings
    print valid
    return ()
