module RowAndColumnExchanges where

import Control.Monad (forM_)
import Data.Function ((&))
import Data.List (intercalate)
import Data.Vector.Unboxed ((!), (//), Vector)
import Data.Vector.Unboxed qualified as VU
import Problem
import Search qualified

newtype State = State (Vector Int)
  deriving (Eq, Ord)

instance Show State where
  show (State v) =
    intercalate "\n" rows
      where
        row i = VU.slice i 4 v & VU.toList & map (pad . show) & intercalate ", "
        rows = map row [0,4,8,12]
        pad s = if length s == 1 then ' ' : s else s

mkState :: [[Int]] -> State
mkState rows = mconcat rows & VU.fromList & State

start :: State
start = mkState
 [ [ 1,  2,  3,  4]
 , [ 5,  6,  7,  8]
 , [ 9, 10, 11, 12]
 , [13, 14, 15, 16] ]

goal :: State
goal = mkState
  [ [12, 10, 11,  9]
  , [16, 14, 15, 13]
  , [ 8,  6,  7,  5]
  , [ 4,  2,  3,  1] ]

exchangeRows :: Int -> Int -> State -> State
exchangeRows x y (State v) = map select [0..3] & VU.concat & State
  where
    rows = map (\i -> VU.slice i 4 v & VU.force) [0,4,8,12]
    select n = if n == x then rows !! y
               else if n == y then rows !! x
               else rows !! n

exchangeCols :: Int -> Int -> State -> State
exchangeCols x y (State v) = switch x y v & switch y x & State
  where
    switch a b = writeCol a (readCol b)
    readCol n = map (v !) [n,n+4..15]
    writeCol n d v' = v' // (zip [n,n+4..] d)

neighbors :: State -> [State]
neighbors s = 
  [exchangeRows x y s | x <- [0..3], y <- [x+1..3]]
  <>
  [exchangeCols x y s | x <- [0..3], y <- [x+1..3]]

solution :: Maybe [State]
solution = Search.bfs (== goal) neighbors start

problem :: Problem
problem = Problem
  { label = "row and column exchanges"
  , description = "TODO"
  , run = do
      case solution of
        Just s ->
          forM_ s $ \x -> do
            putStrLn (show x)
            putStrLn ""
        Nothing -> putStrLn "NO SOLUTION!"
  }

