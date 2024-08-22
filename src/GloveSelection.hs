module GloveSelection ( combinations, problem ) where

import Data.Set qualified as Set
import Problem

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = (map (x:) ys) <> zs
  where
    ys = combinations (n-1) xs
    zs = combinations n xs

data Color = Black | Brown | Red
  deriving (Eq, Ord, Show)

data Hand = L | R
  deriving (Eq, Ord, Show)

data Glove = Glove
  { color :: Color
  , hand :: Hand
  } deriving (Eq, Ord, Show)

blackL :: Glove
blackL = Glove { color = Black, hand = L }

blackR :: Glove
blackR = Glove { color = Black, hand = R }

brownL :: Glove
brownL = Glove { color = Brown, hand = L }

brownR :: Glove
brownR = Glove { color = Brown, hand = R }

redL :: Glove
redL = Glove { color = Red, hand = L }

redR :: Glove
redR = Glove { color = Red, hand = R }

gloves :: [Glove]
gloves = [
  blackL, blackR,
  blackL, blackR,
  blackL, blackR,
  blackL, blackR,
  blackL, blackR,
  brownL, brownR,
  brownL, brownR,
  brownL, brownR,
  redL, redR,
  redL, redR
  ]

anyPairSolution :: Int
anyPairSolution = go 2
  where
    go n = if all hasPair (combinations n gloves) then n else go (n+1)
    hasPair gs = let s = Set.fromList gs
                 in (Set.member blackL s && Set.member blackR s)
                    || (Set.member brownL s && Set.member brownR s)
                    || (Set.member redL s && Set.member redR s)

pairOfEachSolution :: Int
pairOfEachSolution = go 6
  where
    go n = if all hasEachPair (combinations n gloves) then n else go (n+1)
    hasEachPair gs = let s = Set.fromList gs
                     in (Set.member blackL s && Set.member blackR s)
                        && (Set.member brownL s && Set.member brownR s)
                        && (Set.member redL s && Set.member redR s)

problem :: Problem
problem = Problem {
  label = "glove selection",
  description =
    "Winter is coming and you want to be prepared. There are 5 pairs \
    \of black gloves, 3 pairs of brown gloves, and 2 pairs of red gloves \
    \in a box in your basement. However, it's really dark down there and \
    \you can't clearly see the colors. What is the smallest number of gloves \
    \you need to select from the box to guarentee that you've got at least \
    \one matching pair? What about one matching pair of each color?",
  run = do
    putStrLn $ "selecting " <> (show anyPairSolution)
            <> " gloves guarentees at least one pair"
    putStrLn $ "selecting " <> (show pairOfEachSolution)
            <> " gloves guarantees at least one pair of each color"
}

