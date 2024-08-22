module BridgeAndTorch ( problem ) where

import Data.Map.Strict ((!), Map)
import Data.Map.Strict qualified as Map
import Problem
import Search qualified

type Person = String
type TimeToCross = Int

data ProblemInstance = ProblemInstance
  { people :: Map Person TimeToCross
  , timeLimit :: Int
  } deriving (Eq, Ord, Show)

problemInstance :: ProblemInstance
problemInstance = ProblemInstance
  { people = Map.fromList [("A", 1), ("B", 2), ("C", 5), ("D", 8)]
  , timeLimit = 15
  }

data Location = North | South
  deriving (Eq, Ord, Show)

data State = State
  { people :: Map Person Location
  , torch :: Location
  , remainingTime :: Int
  } deriving (Eq, Ord, Show)

neighbors :: Map Person TimeToCross -> State -> [State]
neighbors m s =
  let peopleList = Map.toList s.people
      opp = if s.torch == North then South else North
      onePersonCrossing =
        [ State { people = Map.insert p opp s.people
                , torch = opp
                , remainingTime = s.remainingTime - (m ! p) }
        | (p, loc) <- peopleList
        , loc == s.torch 
        ]
      twoPeopleCrossing =
        [ State { people = Map.union (Map.fromList [(p1, opp), (p2, opp)]) s.people
                , torch = opp
                , remainingTime = s.remainingTime - max (m ! p1) (m ! p2) }
        | (p1, loc1) <- peopleList
        , (p2, loc2) <- peopleList
        , loc1 == s.torch
        , loc2 == s.torch
        ]
  in filter (\s0 -> s0.remainingTime >= 0)
            (onePersonCrossing <> twoPeopleCrossing)

isGoal :: State -> Bool
isGoal s = s.torch == North && all (\(_,loc) -> loc == North) (Map.toList s.people)

type Solution = [State]

solve :: ProblemInstance -> Maybe Solution
solve ProblemInstance { people, timeLimit } =
  let start = State { people = Map.fromList $ map (, South) (Map.keys people)
                    , torch = South
                    , remainingTime = timeLimit }
  in Search.bfs isGoal (neighbors people) start

problem :: Problem
problem = Problem
  { label = "bridge and torch"
  , description =
      "A group of travelers come upon a rickety old bridge at night. They must \
      \cross to the other side, but due to the bridge's condition no more than \
      \two people can cross at the same time. Additionally, they must use a torch \
      \while crossing, but they have only a single torch which will burn for a fixed \
      \amount of time. Each person takes a different length of time to cross the \
      \bridge. When two peoople cross together, they go at the slower persons rate. \
      \Can the group cross the bridge before their torch burns out?"
  , run = case solve problemInstance of
            Nothing -> putStrLn "not solution found"
            Just solution -> mapM_ print solution
  }

