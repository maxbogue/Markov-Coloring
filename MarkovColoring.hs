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

validColoring :: Graph -> Coloring -> Bool
validColoring g c = foldr (&&) True $ map (validVertex) (Map.keys g)
  where
    validVertex v = let vc = (c ! v) in
        foldr (&&) True [vc /= c ! u | u <- g ! v]

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
    if validColoring g coloring' then return coloring' else return coloring

neighborColors :: Vertex -> Graph -> Coloring -> [Color]
neighborColors v g coloring = map (coloring !) (g ! v)

randomColoring :: [Color] -> Graph -> Float -> IO Coloring
randomColoring cs g e = do
    let init = genColoring cs g
    cascade (changeColoring cs g) init (ceiling t)
  where
    q = fromIntegral $ length cs
    n = fromIntegral $ length (Map.keys g)
    d = fromIntegral $ maximum (map length (Map.elems g))
    t = (q * n) / ((q - 2 * d)) * log (n / e)


genColoring :: [Color] -> Graph -> Coloring
genColoring cs g = head $ filter (validColoring g) (allColorings cs g)

countColorings :: [Color] -> Graph -> Float -> IO Float
countColorings cs g e = do
    let vs = (Map.keys g)
    r <- aux vs g
    return (fromIntegral ((length cs)^(length vs)) * r)
  where
    n = length (Map.keys g)
    s = (n * 75 * (ceiling (1 / e)))
    aux :: [Vertex] -> Graph -> IO Float
    aux [] _ = return 1.0
    aux (v:vs) g = do
        let g' = removeEdges g v
        x <- rho cs g g' e s
        let y = (fromIntegral x) / fromIntegral s
        z <- aux vs g'
        return (y * z)
        
rho :: [Color] -> Graph -> Graph -> Float -> Int -> IO Int
rho _ _ _ _ 0 = return 0 
rho cs g g' e n = do
    coloring <- randomColoring cs g' e
    let valid = validColoring g coloring
    if valid
       then (rho cs g g' e (n - 1)) >>= (\x -> return (x + 1))
       else rho cs g g' e (n - 1)

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
    let valid = filter (validColoring g) colorings
    print valid
    return ()
