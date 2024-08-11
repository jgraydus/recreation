module FindPath ( problem ) where

import Control.Monad (filterM, forM_)
import Control.Monad.ST
import Data.Foldable (foldl')
import Data.Function ((&))
import Data.List (intercalate)
import Data.PQueue.Prio.Min (MinPQueue(..))
import Data.PQueue.Prio.Min qualified as PQ
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed.Mutable (MVector)
import Data.Vector.Unboxed.Mutable qualified as MV
import Data.Vector.Generic ((!))
import Data.Vector.Generic qualified as V
import Problem

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = take n xs : chunks n (drop n xs)

data Dimensions = Dimensions { rows :: !Int, cols :: !Int }
  deriving (Eq, Ord, Show)

data Position = Position { row :: !Int , col :: !Int }
  deriving (Eq, Ord, Show)

data WorldMap = WorldMap
  { dat :: !(Vector Char)
  , dims :: !Dimensions
  , start :: !Position
  , goal :: !Position
  , costMap :: Char -> Int
  }

instance Show WorldMap where
  show WorldMap { dat, dims, start, goal } =
    "start: " <> show start <> ", goal: " <> show goal <> "\n" <>
    intercalate "\n" (chunks dims.cols $ V.toList dat)

readWorldMapPosition :: WorldMap -> Position -> Int
readWorldMapPosition m pos = m.costMap $ m.dat ! (pos.row * m.dims.cols + pos.col)

mkWorldMap :: Position -> Position -> String -> (Char -> Int) -> WorldMap
mkWorldMap start goal s costMap =
  let ls = lines s
   in WorldMap { dat = V.fromList . concat $ ls
               , dims = Dimensions { rows = length ls, cols = length . head $ ls }
               , start, goal, costMap }

neighbors :: WorldMap -> Position -> [Position]
neighbors m p =
  let diffs = [Position { row, col } | row <- [(-1)..1], col <- [(-1)..1], (col,row) /= (0,0)]
      applyDiff Position { row, col } = Position { row = row + p.row, col = col + p.col }
      inBounds Position { row, col } = row >= 0 && row < m.dims.rows && col >= 0 && col < m.dims.cols
   in map applyDiff diffs & filter inBounds

data Costs s = Costs
  { v :: !(MVector s Int)
  , dims :: !Dimensions
  }

mkCosts :: WorldMap -> ST s (Costs s)
mkCosts m = do
  v <- MV.replicate (m.dims.rows * m.dims.cols) 10000000
  MV.write v (m.start.row * m.dims.cols + m.start.col) 0
  pure $ Costs { v, dims = m.dims }

readCost :: Costs s -> Position -> ST s Int
readCost c p = MV.read c.v (p.row * c.dims.cols + p.col)

writeCost :: Costs s -> Position -> Int -> ST s ()
writeCost c p val = MV.write c.v (p.row * c.dims.cols + p.col) val 

data Path = Path
  { cost :: !Int
  , path :: ![Position]
  } deriving (Eq, Ord, Show)

addToPath :: WorldMap -> Path -> Position -> Path
addToPath m p pos =
  Path
  { cost = p.cost + readWorldMapPosition m pos
  , path = (pos:p.path)
  }

isImprovement :: Costs s -> Path -> ST s Bool
isImprovement cs p = (p.cost <) <$> readCost cs (head p.path)

extendPath :: WorldMap -> Costs s -> Path -> ST s [Path]
extendPath m cs p = do
  -- construct all possible paths extending this path
  let tmp = fmap (addToPath m p) (neighbors m $ head p.path)
  -- remove paths that aren't better than the current best
  ps <- filterM (isImprovement cs) tmp
  -- update the costs because these paths are better
  forM_ ps $ \q -> writeCost cs (head q.path) q.cost
  pure ps

newtype Queue = Queue { pq :: MinPQueue Int [Position] }
  deriving Show

newQueue :: [Path] -> Queue
newQueue ps = Queue { pq = foldl' (\q p -> PQ.insert p.cost p.path q) PQ.empty ps }

insertAll :: [Path] -> Queue -> Queue
insertAll ps queue = Queue { pq = foldl' (\q p -> PQ.insert p.cost p.path q) queue.pq ps }

popQueue :: Queue -> Maybe (Path, Queue)
popQueue q =
  case q.pq of
    (cost,path) :< pq -> Just (Path { cost, path }, Queue { pq })
    Empty -> Nothing

isGoal :: WorldMap -> Path -> Bool
isGoal m p = m.goal == head p.path

findPath :: WorldMap -> Maybe Path
findPath m = runST $ do
  cs <- mkCosts m
  let go queue = case popQueue queue of
        Nothing -> pure Nothing
        Just (p,q) ->
          if isGoal m p then pure (Just p)
          else do
            ps <- extendPath m cs p
            go (insertAll ps q)
  go $ newQueue [Path { cost = 0, path = [m.start] }]

worldMap01 :: WorldMap
worldMap01 = mkWorldMap
  Position { row = 0, col = 0 }
  Position { row = 22, col = 39 }
  "     ....   ...     ..OOOO.     ....OOOO\n\
  \   ...OO..  .O.  ....OOO...     .O...OOO\n\
  \   .OOOOO.  ......OOOOO..    ...... ...O\n\
  \   ..OOO..     .OOOOOOOO..  ..O...    ..\n\
  \    ..O..  ..............  ...OOO...    \n\
  \     .......OO..  ..OO..  ...OOOOOO.    \n\
  \       ...OOOOO....OOOO.  .OOOOOO.....  \n\
  \....   .OOOOOOOO....O........OO....OO.  \n\
  \OOO... ..OOOOO...  ... ..OO...O.  .OO.  \n\
  \OOOOO.  ..OO...      ...OOOO....  ....  \n\
  \OOOO..   ........ ....OOOOOOO......     \n\
  \OO...  ...OOOOOO. .OOOOOOOOOOOOOOO.  ...\n\
  \...    .OOOOOO... ..OOOO..OOOOOO......OO\n\
  \       ..OOO...... ..OO....OOOO.....OOOO\n\
  \...     ..O....O......... ..OO.. .OOOOOO\n\
  \OO....   ...  ....OOOOOO.. ..........OOO\n\
  \OOOOO.....     .OOOOOOOOO.  ..OO... ...O\n\
  \OOOOOOOOO... ....OOOOOOO.....OOOOO..  ..\n\
  \OOOOOOOOOOO. .O....OO.... .OOOOOOOO.   .\n\
  \OOOOOOOOOO............    ..OOOOO...    \n\
  \OOOOOOO....O. ...OO..      ...OOOOO..   \n\
  \OOOOOO..  .....OOOOO..       ...OOO0... \n\
  \OOO....     .OOOOOOOO.      .OOOOOOOOO. "
  (\case ' ' -> 1; '.' -> 5; 'O' -> 100; _ -> 100000)

drawWithPath :: WorldMap -> Path -> IO ()
drawWithPath m Path { path } = do
  dat <- V.thaw m.dat
  forM_ path $ \pos -> MV.write dat (pos.row * m.dims.cols + pos.col) '*'
  MV.write dat (m.start.row * m.dims.cols + m.start.col) 'S'
  MV.write dat (m.goal.row * m.dims.cols + m.goal.col) 'G'
  v <- V.freeze dat
  V.toList v & chunks m.dims.cols & intercalate "\n" & putStrLn

problem :: Problem
problem = Problem
  { label = "path finding"
  , description =
      "Find the lowest cost path between two indicated points on a \
      \map. Each point on the map has an associated cost."
  , run = do
      case findPath worldMap01 of
        Nothing -> putStrLn "no path found"
        Just solution -> drawWithPath worldMap01 solution
  }
