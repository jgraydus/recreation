{-# OPTIONS_GHC -Wno-missing-methods #-}
module MissionariesCannibals ( problem ) where

import Data.Function ((&))
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Problem

data Boat = North | South
  deriving (Eq, Ord, Show)

data World = World
  { north :: Population
  , south :: Population
  , boat :: Boat
  } deriving (Eq, Ord, Show)

data Population = Population
  { m :: Int -- missionaries
  , c :: Int -- cannibals
  } deriving (Eq, Ord, Show)

instance Num Population where
  left + right = Population { m = left.m + right.m, c = left.c + right.c }
  negate p = Population { m = negate p.m, c = negate p.c }

start :: World
start = World
  { north = Population { m = 3, c = 3 }
  , south = Population { m = 0, c = 0 }
  , boat = North
  }

goal :: World
goal = World
  { north = Population { m = 0, c = 0 }
  , south = Population { m = 3, c = 3 }
  , boat = South
  }

type Solution = [World]

solve' :: Set World -> World -> Maybe Solution
solve' visited w =
  if w == goal then Just [w] else
  if Set.member w visited then Nothing
  else map (solve' $ Set.insert w visited) (neighbors w)
       & catMaybes
       & listToMaybe
       & fmap (w:)

solve :: World -> Maybe Solution
solve = solve' Set.empty

neighbors :: World -> [World]
neighbors w = worldDiffs w & map (applyDiff w) & filter isValid

worldDiffs :: World -> [Population]
worldDiffs w =
  case w.boat of
    North -> populationDiffs w.north
    South -> populationDiffs w.south

populationDiffs :: Population -> [Population]
populationDiffs p =
  [ Population { m, c }
  | m <- [0..p.m]
  , c <- [0..p.c]
  , m + c > 0
  , m + c <= 2
  ]

applyDiff :: World -> Population -> World
applyDiff w p =
  case w.boat of
    North -> World { north = w.north - p, south = w.south + p, boat = South }
    South -> World { north = w.north + p, south = w.south - p, boat = North }

isValid :: World -> Bool
isValid w = (w.north.m == 0 || w.north.m >= w.north.c)
            && (w.south.m == 0 || w.south.m >= w.south.c)

problem :: Problem
problem = Problem
  { label = "Missionaries and Cannibals"
  , description =
      "Three missionaries and three cannibals need to cross a river. They have a boat\
      \ that can carry at most two people. If there are ever more cannibals than missionaries\
      \ on either side of the river, the cannibals will eat the missionaries. How do they\
      \ safely cross the river?"
  , run = do
      case solve start of
        Nothing -> putStrLn "no solution found"
        Just solution -> mapM_ (putStrLn . show) solution
  }

