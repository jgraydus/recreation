module NPuzzle ( problem ) where

import Data.Function ((&))
import Data.Functor.Identity (runIdentity)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Vector.Unboxed (Vector)
import Data.Vector.Generic ((!),(//))
import Data.Vector.Generic qualified as V
import Data.Word (Word8)
import Problem
import Search qualified

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = let (chunk, rest) = (take n xs, drop n xs)
               in (chunk : chunks n rest)

data World = World 
  { dat :: !(Vector Word8)
  , width :: !Int
  , height :: !Int
  } deriving (Eq, Ord)

-- TODO verify validity
mkWorld :: [[Word8]] -> World
mkWorld d = let (width, height) = ( length . head $ d
                                  , length d )
             in World { dat = V.fromList . concat $ d, width, height }

instance Show World where
  show World { dat, width, height } =
    let hole = width * height - 1
        tileToString n = if fromIntegral n == hole then " " else (show $ n + 1)
     in   V.toList dat
        & fmap tileToString
        & chunks width
        & fmap (intercalate " ")
        & intercalate "\n"

diffs :: [(Int,Int)]
diffs = [((-1),0), (1,0), (0,(-1)), (0,1)]

neighbors :: World -> [World]
neighbors w@World { dat, width, height } = runIdentity $ do
  let toIdx (x, y) = y * width + x
      fromIdx i = quotRem i width
      hole = fromIntegral $ width * height - 1
      holeIdx = fromMaybe (error "hole is missing") $ V.findIndex (== hole) dat
      (holeX, holeY) = fromIdx holeIdx
      applyDiff (x,y) = (holeX+x,holeY+y)
      isValid (x,y) = x >= 0 && x < width && y >= 0 && y < height
      neighborTiles = map applyDiff diffs & filter isValid
      move (x,y) = let i = toIdx (x,y); n = dat ! i
                    in w { dat = dat // [(holeIdx, n), (i, hole)] }
  pure $ map move neighborTiles

isGoal :: World -> Bool
isGoal World { dat } = V.ifoldl' (\acc i x -> i == fromIntegral x && acc) True dat

type Solution = [World]

solve :: World -> Maybe Solution
solve = Search.bfs isGoal neighbors

test01 :: World
test01 = mkWorld
  [ [1, 3, 2]
  , [0, 4, 5]
  , [6, 8, 7] ]

problem :: Problem
problem = Problem
  { label = "n-puzzle"
  , description =
      "A puzzle consisting of a W x H grid of tiles labeled 1 to W x H - 1. The\
      \ remaining tile is the 'hole'. A valid move consists of sliding a tile\
      \ into the hole. i.e. exchanging the hole with a tile above, below, to\
      \ the left, or to the right of it. The goal is to put the tiles in numeric\
      \ order from top left to bottom right with the hole in the last position."
  , run =
      case solve test01 of
        Nothing -> putStrLn "no solution!"
        Just s -> mapM_ (\w -> print w >> putStrLn "") s
  }

