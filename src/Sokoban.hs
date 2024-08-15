module Sokoban ( problem ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad (forM_, when)
import Control.Monad.ST
import Data.Foldable (foldl')
import Data.Function ((&))
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (pack)
import Data.Vector.Mutable qualified as MV
import Data.Vector qualified as V
import Prelude hiding (init)
import Problem
import System.Terminal

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = (take n xs) : chunks n (drop n xs)

asMaybe :: (a -> Bool) -> a -> Maybe a
asMaybe p a = if p a then Just a else Nothing

data Dimensions = Dimensions
  { rows :: Int
  , cols :: Int
  } deriving (Eq, Ord, Show)

data Location = Location
  { row :: Int
  , col :: Int
  } deriving (Eq, Ord, Show)

data Puzzle = Puzzle
  { dims :: Dimensions
  , walls :: Set Location
  , blocks :: Set Location
  , goals :: Set Location
  , player :: Location
  } deriving (Eq, Ord)

instance Show Puzzle where
  show Puzzle { dims, walls, blocks, goals, player } =
    runST $ do
      mv <- MV.replicate (dims.rows * dims.cols) ' '
      MV.modify mv (\c -> if c == '.' then 'P' else 'p') (player.row * dims.cols + player.col)
      forM_ walls $ \Location { row, col } ->
         MV.modify mv (\_ -> 'O') (row * dims.cols + col)
      forM_ blocks $ \Location { row, col } ->
         MV.modify mv (\_ -> 'x') (row * dims.cols + col)
      forM_ goals $ \Location { row, col } ->
         MV.modify mv (\case {'x' -> 'X'; 'p' -> 'P'; _ -> '.'}) (row * dims.cols + col)
      v <- V.freeze mv
      V.toList v & chunks dims.cols & intercalate "\n" & pure

mkPuzzle :: String -> Puzzle
mkPuzzle s =
  let ls = lines s
      locations = [ (Location { row, col }, c)
                  | (line, row) <- zip ls [0..]
                  , (c, col) <- zip line [0..] ]
      update acc (loc, c) =
        case c of
          'O' -> acc { walls = Set.insert loc acc.walls }
          '.' -> acc { goals = Set.insert loc acc.goals }
          'p' -> acc { player = loc }
          'P' -> acc { player = loc
                     , goals = Set.insert loc acc.goals }
          'x' -> acc { blocks = Set.insert loc acc.blocks }
          'X' -> acc { blocks = Set.insert loc acc.blocks
                     , goals = Set.insert loc acc.goals }
          _   -> acc
      init = Puzzle { dims = Dimensions { rows = length ls, cols = length $ head ls }
                    , walls = Set.empty
                    , blocks = Set.empty
                    , goals = Set.empty
                    , player = Location { row = 0, col = 0 }
                    }
  in foldl' update init locations

puzzle01 :: Puzzle
puzzle01 = mkPuzzle
  "  OOOOO \n\
  \OOO   O \n\
  \O.px  O \n\
  \OOO x.O \n\
  \O.OOx O \n\
  \O O . OO\n\
  \Ox Xxx.O\n\
  \O   .  O\n\
  \OOOOOOOO"

data Dir = U | D | L | R

applyDirection :: Location -> Dir -> Location
applyDirection loc = \case
  U -> Location { col = loc.col, row = loc.row - 1 }
  D -> Location { col = loc.col, row = loc.row + 1 }
  L -> Location { col = loc.col - 1, row = loc.row }
  R -> Location { col = loc.col + 1, row = loc.row }

movePlayer :: Puzzle -> Dir -> Maybe Puzzle
movePlayer p dir = 
  Puzzle <$> Just p.dims
         <*> Just p.walls
         <*> blocks
         <*> Just p.goals
         <*> player
  where
    isInBounds :: Location -> Dimensions -> Bool
    isInBounds loc dims = loc.row >= 0
                          && loc.col >= 0
                          && loc.row < dims.rows
                          && loc.col < dims.cols

    isValid :: Set Location -> Location -> Bool
    isValid bs loc = isInBounds loc p.dims
                     && Set.notMember loc p.walls
                     && Set.notMember loc bs

    moveBlock :: Location -> Maybe (Set Location)
    moveBlock loc = applyDirection loc dir
                    & asMaybe (isValid p.blocks)
                    & fmap (\newLoc -> Set.insert newLoc (Set.delete loc p.blocks))

    blocks :: Maybe (Set Location)
    blocks = applyDirection p.player dir
             -- if the player moves into a block then attempt to push the block
             -- otherwise, leave the block locations unchanged
             & \loc -> if Set.member loc p.blocks
                       then moveBlock loc
                       else Just p.blocks

    player :: Maybe Location
    player = applyDirection p.player dir
             & \loc -> case blocks of
                         Nothing -> Nothing
                         Just bs -> asMaybe (isValid bs) loc

displayPuzzle ::
  ( MonadMarkupPrinter m
  , MonadFormattingPrinter m
  , MonadInput m
  , MonadScreen m
  ) => Puzzle -> m ()
displayPuzzle p = do
  eraseInDisplay EraseAll
  Size h w <- getWindowSize
  let c = (w - p.dims.cols) `div` 2
      r = (h - p.dims.rows) `div` 2
  when (p.blocks == p.goals) $ do
    setCursorPosition $ Position { row = r - 2, col = c }
    putStringLn "SUCCESS!"
  forM_ (zip [0..] . lines . show $ p) $ \(n, line) -> do
    setCursorPosition $ Position { row = r + n, col = c }
    putStringLn line

doRun ::
  ( MonadMarkupPrinter m
  , MonadFormattingPrinter m
  , MonadInput m
  , MonadScreen m
  ) => m ()
doRun = do
  let handleInput continue reset = do
        event <- awaitEvent
        liftIO $ print event
        case event of
          Left Interrupt -> pure ()
          Right (KeyEvent (ArrowKey Upwards) _) -> continue $ Just U
          Right (KeyEvent (ArrowKey Downwards) _) -> continue $ Just D
          Right (KeyEvent (ArrowKey Leftwards) _) -> continue $ Just L
          Right (KeyEvent (ArrowKey Rightwards) _) -> continue $ Just R
          Right (KeyEvent (CharKey 'r') _) -> reset
          Right (KeyEvent EscapeKey _) -> pure ()
          _ -> handleInput continue reset

      go puzzle move = do
        let p = fromMaybe puzzle (move >>= (movePlayer puzzle))
        displayPuzzle p
        handleInput (go p) (go puzzle01 Nothing)

  hideCursor
  setAlternateScreenBuffer True
  eraseInDisplay EraseAll
  go puzzle01 Nothing
  showCursor
  setAlternateScreenBuffer False
   

problem :: Problem
problem = Problem
  { label = "sokoban"
  , description = ""
  , run = withTerminal $ runTerminalT $ doRun
  }
