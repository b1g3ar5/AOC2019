{-# language DeriveFunctor #-}
{-# language TupleSections #-}

module Graph(

  )where


import qualified Data.Map as M hiding (findIndex)
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Array
import Data.STRef
import Control.Monad.ST


type Vertex = Int
type Table a = Array Vertex a
type Graph e = Table [(e, Vertex)]
type Bounds  = (Vertex, Vertex)
type Edge e = (Vertex, e, Vertex)

type Labeling a = Vertex -> a
data LabGraph n e = LabGraph (Graph e) (Labeling n)


vertices :: LabGraph n e -> [Vertex]
vertices (LabGraph gr _) = indices gr


labels :: LabGraph n e -> [n]
labels (LabGraph gr l) = map l (indices gr)


unfoldG :: (Ord s) => (s -> (n, [(e, s)])) -> s -> (Vertex, LabGraph n e)
unfoldG f r = (r', res)
  where
    ([r'], res) = unfoldGMany f [r]


unfoldGMany :: (Ord s) => (s -> (n, [(e, s)])) -> [s] -> ([Vertex], LabGraph n e)
unfoldGMany f roots = runST ( unfoldGManyST f roots ) -- detailed later


foldG :: (Eq r) => r -> (Vertex -> [(e, r)] -> r) -> Graph e -> Vertex -> r
foldG i f g v = foldGAll i f g ! v


foldGAll :: (Eq r) => r -> (Vertex -> [(e, r)] -> r) -> Graph e -> Array Vertex r
foldGAll bot f gr = finalTbl
  where
    finalTbl = fixedPoint updateTbl initialTbl
    initialTbl = listArray bnds (replicate (rangeSize bnds) bot)

    fixedPoint f x = fp x
      where
        fp z = if z == z'
               then z
               else fp z'
          where z' = f z
    updateTbl tbl = listArray bnds $ map recompute $ indices gr
          where
            recompute v = f v [(b, tbl!k) | (b, k) <- gr!v]
    bnds = bounds gr



-- | Build a graph from a list of edges.
buildG :: Bounds -> [Edge e] -> Graph e
buildG bounds0 edges0 = accumArray (flip (:)) [] bounds0 [(v, (l,w)) | (v,l,w) <- edges0]


-- | The graph obtained by reversing all edges.
transposeG  :: Graph e -> Graph e
transposeG g = buildG (bounds g) (reverseE g)


reverseE    :: Graph e -> [Edge e]
reverseE g   = [ (w, l, v) | (v, l, w) <- edges g ]


showGraphViz (LabGraph gr lab)  =
  "digraph name {\n" ++
  "rankdir=LR;\n" ++
  (concatMap showNode $ indices gr) ++
  (concatMap showEdge $ edges gr) ++
  "}\n"
  where
    showEdge (from, t, to) = show from ++ " -> " ++ show to ++
                           " [label = \"" ++ show t ++ "\"];\n"
    showNode v = show v ++ " [label = " ++ (show $ lab v) ++ "];\n"


edges :: Graph e -> [Edge e]
edges g = [ (v, l, w) | v <- indices g, (l, w) <- g!v ]


closure :: Ord a => (a -> [a]) -> (a -> [a])
closure f i = labels $ snd $ unfoldG f' i
    where f' x = (x, [((), fx) | fx <- f x])


interleave (x1:x2:xs) = (x1:x2:xs) : (map (x2:) (interleave (x1:xs)))
interleave xs = [xs]


-- | Compute the distance to v for every vertex of gr.
distsTo :: Vertex -> Graph Float -> Table Float
distsTo v gr = foldGAll infinite distance gr
    where infinite = 10000000 -- well, you get the idea
          distance v' neighbours
              | v == v' = 0
              | otherwise = minimum [distV+arcWeight | (distV, arcWeight) <- neighbours]


grDist = buildG (1,5) [(1,5.0,2), (2,5.0,3), (2,7.0,4), (3,5.0,4), (4,5.0,5), (4,3.0,1)]


dists = distsTo 5 grDist


unfoldGManyST :: (Ord a) => (a -> (c, [(b, a)]))
             -> [a] -> ST s ([Vertex], LabGraph c b)
unfoldGManyST gen seeds =
     do mtab <- newSTRef (M.empty)
        allNodes <- newSTRef []
        vertexRef <- newSTRef firstId
        let allocVertex =
                do vertex <- readSTRef vertexRef
                   writeSTRef vertexRef (vertex + 1)
                   return vertex
        let cyc src =
             do probe <- memTabFind mtab src
                case probe of
                   Just result -> return result
                   Nothing -> do
                     v <- allocVertex
                     memTabBind src v mtab
                     let (lab, deps) = gen src
                     ws <- mapM (cyc . snd) deps
                     let res = (v, lab, [(fst d, w) | d <- deps, w <- ws])
                     modifySTRef allNodes (res:)
                     return v
        mapM_ cyc seeds
        list <- readSTRef allNodes
        seedsResult <- (return . map fromJust) =<< mapM (memTabFind mtab) seeds
        lastId <- readSTRef vertexRef
        let cycamore = array (firstId, lastId-1) [(i, k) | (i, a, k) <- list]
        let labels = array (firstId, lastId-1) [(i, a) | (i, a, k) <- list]
        return (seedsResult, LabGraph cycamore (labels !))
   where firstId = 0::Vertex
         memTabFind mt key = return . M.lookup key =<< readSTRef mt
         memTabBind key val mt = modifySTRef mt (M.insert key val)


foldGAllImplementation bot f gr = finalTbl
    where finalTbl = fixedPoint updateTbl initialTbl
          initialTbl = listArray bnds (replicate (rangeSize bnds) bot)

          fixedPoint f x = fp x
              where fp z = if z == z' then z else fp z'
                        where z' = f z
          updateTbl tbl = listArray bnds $ map recompute $ indices gr
              where recompute v = f v [(b, tbl!k) | (b, k) <- gr!v]
          bnds = bounds gr
