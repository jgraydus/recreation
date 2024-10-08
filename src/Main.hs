module Main where

import Data.Foldable (for_)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text (pack, unpack)
import BridgeAndTorch qualified
import FindPath qualified
import GloveSelection qualified
import MissionariesCannibals qualified
import NPuzzle qualified
import Problem
import RowAndColumnExchanges qualified
import Sokoban qualified
import System.Terminal

problems :: Map Int Problem
problems = Map.fromList $ zip [1..]
  [ MissionariesCannibals.problem
  , NPuzzle.problem
  , FindPath.problem
  , BridgeAndTorch.problem
  , Sokoban.problem
  , GloveSelection.problem
  , RowAndColumnExchanges.problem
  ]

menu ::
  ( MonadMarkupPrinter m
  , MonadFormattingPrinter m
  , MonadInput m
  , MonadScreen m) 
  => Int -> Maybe Text -> m Int
menu selected info = do
  eraseInDisplay EraseAll
  Size h w <- getWindowSize
  let r = (h - Map.size problems) `div` 2 - 1
      c = (w - 60) `div` 2
  setCursorPosition $ Position { row = r, col = c }
  resetAttributes
  putTextLn "select problem:"
  let menuItem (i, prob) =
        do
        if i == selected then setAttribute inverted else resetAttributes
        setCursorPosition $ Position { row = r + 1 + i, col = c }
        putTextLn (pack (show i) <> " - " <> prob.label)
  mapM_ menuItem (Map.toList problems)
  resetAttributes
  setCursorPosition $ Position { row = r + 3 + Map.size problems, col = c }
  putTextLn "Enter: select"
  setCursorPosition $ Position { row = r + 4 + Map.size problems, col = c }
  putTextLn "i: display info"
  for_ info $ \i -> do
    setCursorPosition $ Position { row = r + 6 + Map.size problems, col = c }
    putTextLn i
  flush
  let handleInput = do
        event <- awaitEvent
        case event of
          Right (KeyEvent (ArrowKey Upwards) _) -> menu (max 1 (selected - 1)) Nothing
          Right (KeyEvent (ArrowKey Downwards) _) -> menu (min (Map.size problems) (selected + 1)) Nothing
          Right (KeyEvent EnterKey _) -> pure selected
          Right (KeyEvent (CharKey 'i') _) -> case Map.lookup selected problems of
            Nothing -> menu selected Nothing
            Just Problem { label, description } ->
               let desc = label <> " - " <> description
                in menu selected (Just desc)
          _ -> handleInput
  handleInput

getProgram :: IO (IO ())
getProgram = withTerminal $ runTerminalT $ do
  hideCursor
  setAlternateScreenBuffer True
  selection <- menu 1 Nothing
  showCursor
  setAlternateScreenBuffer False
  case Map.lookup selection problems of
    Nothing -> error "program not found"
    Just Problem { label, run } ->
      pure $ do
        putStrLn $ unpack label
        run

main :: IO ()
main = do
  program <- getProgram
  program

