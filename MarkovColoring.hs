module MarkovColoring where

import Data.HashMap (Map, (!))
import qualified Data.HashMap as Map

type Vertex = Int

-- Graph defined using a map adjacency-list structure.
type Graph = Map Vertex [Vertex]

type Color = Int

type Coloring = Map Vertex Color

g :: Graph
g = Map.fromList [(1, [2]), (2, [1]), (3, [4]), (4, [3])]

allColorings :: [Color] -> Graph -> [Coloring]
allColorings cs g = map Map.fromList $ listPower (Map.keys g) cs
  where
    listPower []     _  = [[]]
    listPower vs     [] = undefined
    listPower (v:vs) cs = concat [map ((v, c) :) (listPower vs cs) | c <- cs]

validColoring :: Graph -> Coloring -> Bool
validColoring g c = foldr (&&) True $ map (validVertex) (Map.keys g)
  where
    validVertex v = let vc = (c ! v) in
        foldr (&&) True [vc /= c ! u | u <- g ! v]

main = do
    let colors = [6, 9]
    let colorings = allColorings colors g
    print $ colorings
    let valid = filter (validColoring g) colorings
    print valid
    return ()
