import Data.Array.IO
import Data.HashMap (Map, (!))
import Data.List (delete, sort, union, (\\))
import System.Environment (getArgs)
import System.Random (randomRIO)
import qualified Data.HashMap as Map

-- A vertex is simply represented as an int.
type Vertex = Int

-- Graph defined using a map adjacency-list structure.
type Graph = Map Vertex [Vertex]

-- A color is also simply represented as an int.
type Color = Int

-- Colorings are decoupled from their graphs and simply map vertices to colors.
type Coloring = Map Vertex Color

-- A mutable coloring.
type MColoring = IOUArray Vertex Color

-- Utility function to repeatedly apply a monadic computation.
cascade :: (Monad m) => (a -> m a) -> a -> Int -> m a
cascade _ x 0 =  return x
cascade f x n =  f x >>= \x' -> cascade f x' (n - 1)

makeBounds :: [a] -> (Int, Int)
makeBounds ls = (0, length ls - 1)

-- Remove all the edges adjacent to a vertex from the graph.
removeEdges :: Graph -> Vertex -> Graph
removeEdges g v = fmap (delete v) (Map.insert v [] g)

mColoring :: Coloring -> IO MColoring
mColoring c = let pairs = Map.toList c in
    newListArray (makeBounds pairs) $ map snd $ sort $ pairs

-- Returns a list of colors used by neighbors of a given vertex.
neighborColors :: Vertex -> Graph -> Coloring -> [Color]
neighborColors v g coloring = maybeMap (\n -> Map.lookup n coloring) (g ! v)
  where
    maybeMap :: (a -> Maybe b) -> [a] -> [b]
    maybeMap _ []     = []
    maybeMap f (a:as) = case f a of
        Just b  -> b : maybeMap f as
        Nothing -> maybeMap f as

-- Test a graph coloring for validity.
isValidColoring :: Graph -> MColoring -> IO Bool
isValidColoring g c = do
    return . foldr (&&) True =<< mapM (validVertex) (Map.keys g)
  where
    validVertex v = do
        vc <- readArray c v
        cs <- mapM (readArray c) (g ! v)
        return $ foldr (&&) True $ map (vc /=) cs

-- Generate all valid colorings of a graph.
validColorings :: [Color] -> Graph -> [Coloring]
validColorings cs g = validColorings' (Map.empty) (Map.keys g)
  where
    -- Takes a coloring so far and a list of remaining vertices to color.
    validColorings' :: Coloring -> [Vertex] -> [Coloring]
    validColorings' cg []     = [cg]
    validColorings' cg (v:vs) = concatMap
        (\c -> validColorings' (Map.insert v c cg) vs)
        (cs \\ neighborColors v g cg)

-- Generate a single (non-random) coloring of a graph.
generateColoring :: [Color] -> Graph -> Coloring
generateColoring cs g = head $ validColorings cs g

mNeighborColors :: Vertex -> Graph -> MColoring -> IO [Color]
mNeighborColors v g coloring = mapM (readArray coloring) (g ! v)

-- Performs one step of the coloring Markov chain.
changeColoring :: [Color] -> Graph -> MColoring -> IO MColoring
changeColoring cs g coloring = do
    let vertices = Map.keys g
    iv <- randomRIO (makeBounds vertices)
    let v = vertices !! iv
    validColors <- return . (cs \\) =<< mNeighborColors v g coloring
    c <- readArray coloring v
    c' <- return . (validColors !!) =<< randomRIO (makeBounds validColors)
    writeArray coloring v c'
    isValidColoring g coloring >>= \b -> if b
        then return coloring
        else writeArray coloring v c >> return coloring

-- Generate a random coloring of the map using the Markov chain.
randomColoring :: [Color] -> MColoring -> Graph -> Double -> IO MColoring
randomColoring cs init g e = cascade (changeColoring cs g) init (ceiling t)
  where
    q = fromIntegral $ length cs
    n = fromIntegral $ length (Map.keys g)
    d = fromIntegral $ maximum (map length (Map.elems g))
    t = if q - (2 * d) > 0
        then (q * n) / ((q - 2 * d)) * log (n / e)
        else n * log ( n / e)

-- Estimate a count of the number of colorings of a graph.
countColorings :: [Color] -> Graph -> Double -> IO Double
countColorings cs g e = do
    let vs = (Map.keys g)
    r <- aux vs g
    return $ ((fromIntegral (length cs))^(fromIntegral (length vs))) * r
  where
    n = length (Map.keys g)
    s = (n * 75 * (ceiling (1 / e)))
    aux :: [Vertex] -> Graph -> IO Double
    aux [] _ = return 1.0
    aux (v:vs) g = do
        putStrLn $ "vertices left:" ++ show ( (length vs) + 1)
        let g' = removeEdges g v
        init <- mColoring $ generateColoring cs g'
        x <- rho cs init g g' e s
        let y = (fromIntegral x) / fromIntegral s
        z <- aux vs g'
        return (y * z)

-- Calculates the rho value used in the countColoring estimation.
rho :: [Color] -> MColoring -> Graph -> Graph -> Double -> Int -> IO Int
rho _ _ _ _ _ 0 = return 0 
rho cs init g g' e n = do
    coloring <- randomColoring cs init g' e
    valid <- isValidColoring g coloring
    if valid
       then (rho cs init g g' e (n - 1)) >>= (\x -> return (x + 1))
       else rho cs init g g' e (n - 1)

-- Constructs a graph from a list of edges.
constructGraph :: [(Vertex, Vertex)] -> Graph
constructGraph [] = Map.empty
constructGraph ((a,b):edges) = Map.insertWith union a [b] $
                               Map.insertWith union b [a] $
                               constructGraph edges

-- Reads a graph from a file.  Returns (n, m, q, graph).
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

-- If reads can get a valid parse of the string, return just that.
maybeRead :: (Read a) => String -> Maybe a
maybeRead s = case reads s of
                   [(x,_)] -> Just x
                   _       -> Nothing

main :: IO ()
main = do
    args <- getArgs
    case args of 
        [fn, es, algo] | Just e <- maybeRead es -> do
            (n, m, q, g) <- readGraph fn
            let colors = [1..q]
            case algo of
                 "markov"     -> print =<< countColorings colors g e
                 "bruteforce" -> print $ length (validColorings colors g)
            return ()
        _ -> putStrLn "Usage: MarkovColoring <filename> <epsilon> <markov|bruteforce>"
