module Problem where

import Data.Text

data Problem = Problem
  { label :: Text
  , description :: Text
  , run :: IO ()
  }

