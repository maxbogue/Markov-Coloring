module MarkovColoring where

import System.Random (randomRIO)
import Data.HashMap (Map, (!))
import Data.List ((\\))
import Control.Monad (foldM)
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

randomColoring :: [Color] -> Graph -> Coloring -> IO Coloring
randomColoring cs g coloring = do
    cascade (changeColoring cs g) coloring 100

main = do
    let colors = [6, 9]
    let colorings = allColorings colors g
    let valid = filter (validColoring g) colorings
    print valid
    return ()
