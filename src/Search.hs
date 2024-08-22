module Search (
    bfs, dfs, Path, Solution
) where

import Data.Function ((&))
import Data.Maybe (catMaybes, listToMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Sequence (Seq(..), (><))
import Data.Sequence qualified as Seq

type Path world = [world]
type Solution world = Path world

type Visited world = Set world
type PartialSolution world = (Path world, Visited world)
type Queue world = Seq (PartialSolution world)

search :: Ord world
       => (Queue world -> [PartialSolution world] -> Queue world)
       -> (world -> Bool)
       -> (world -> [world])
       -> world
       -> Maybe (Solution world)
search enqueue isGoal neighbors start = go (Seq.singleton ([start], Set.empty))
  where
    go ((s@(w:_), v) :<| queue) =
      if isGoal w then Just (reverse s)
      else extend v w s & enqueue queue & go
    go _ = Nothing

    extend v w s = neighbors w
                 & filter (flip Set.notMember v)
                 & map (\x -> ((x:s), Set.insert x v))

bfs :: Ord world
    => (world -> Bool)
    -> (world -> [world])
    -> world
    -> Maybe (Solution world)
bfs = search $ \queue path -> queue >< Seq.fromList path  -- add new paths to end of queue

dfs :: Ord world
    => (world -> Bool)
    -> (world -> [world])
    -> world
    -> Maybe (Solution world)
dfs = search $ \queue path -> Seq.fromList path >< queue -- add new paths to front of queue

