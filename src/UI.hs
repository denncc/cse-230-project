module UI where

import Brick
import Brick.Main (App, defaultMain, simpleMain, neverShowCursor)
import Brick.Widgets.Core (joinBorders, withBorderStyle, (<+>) , (<=>), str)
import Brick.Widgets.Center (center, hCenter)
import Brick.Widgets.Border (borderWithLabel, vBorder)
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Table (table, renderTable)
import qualified Graphics.Vty as V
import Lens.Micro
import Game

styleCursor, styleCellGiven, styleCellInput, styleCellNote :: AttrName
styleSolved, styleUnsolved :: AttrName
styleCursor    = attrName "styleCursor"
styleCellGiven = attrName "styleCellGiven"
styleCellInput = attrName "styleCellInput"
styleCellNote  = attrName "styleCellNote"
styleSolved    = attrName "styleSolved"
styleUnsolved  = attrName "styleUnsolved"

attributes :: AttrMap
attributes = attrMap V.defAttr
  [ (styleCursor    , bg V.brightBlack)
  , (styleCellGiven , V.defAttr)
  , (styleCellInput , fg V.blue)
  , (styleCellNote  , fg V.yellow)
  , (styleSolved    , fg V.green)
  , (styleUnsolved  , fg V.red)
  ]

handleEvent :: BrickEvent () e -> EventM () Game ()
handleEvent (VtyEvent (V.EvKey key [V.MCtrl])) = do
  game <- get
  case key of
    V.KChar 'c' -> halt
    V.KChar 'z' -> maybe continueWithoutRedraw put (previous game)
    V.KChar 'r' -> put $ resetGame game
    _ -> continueWithoutRedraw
handleEvent (VtyEvent (V.EvKey key [])) = do
  game <- get
  put $ case key of
    -- Move by cell
    V.KUp       -> moveCursor Game.Up 1 game
    V.KDown     -> moveCursor Game.Down 1 game
    V.KLeft     -> moveCursor Game.Left 1 game
    V.KRight    -> moveCursor Game.Right 1 game
    -- Modify Cell
    V.KChar '1' -> modifyCell 1 game
    V.KChar '2' -> modifyCell 2 game
    V.KChar '3' -> modifyCell 3 game
    V.KChar '4' -> modifyCell 4 game
    V.KChar '5' -> modifyCell 5 game
    V.KChar '6' -> modifyCell 6 game
    V.KChar '7' -> modifyCell 7 game
    V.KChar '8' -> modifyCell 8 game
    V.KChar '9' -> modifyCell 9 game
    V.KBS       -> modifyCell 0 game
handleEvent _ = continueWithoutRedraw

drawBoard :: Game -> Widget ()
drawBoard game = borderWithLabel (str "Board") $ renderTable $ table $ map (map mapElement) $ grid game
  where
    mapElement :: Element -> Widget ()
    mapElement Nothing = str " "
    mapElement (Just v) = str $ show v

drawInstruction :: Widget ()
drawInstruction = borderWithLabel (str "Instruction") $ hCenter (str "Use ↑, ↓, ←, → to move cursor") <=> hCenter (str "Use 1-9 to fill the selected cell") <=> hCenter (str "Use Backspace to clear number in the selected cell")

drawStatus :: (Int, Int) -> Widget ()
drawStatus cursor = borderWithLabel (str "Status") $ hCenter (str $ "Cursor: " ++ show cursor) <=> hCenter (str "Progress: 10%")

drawResult :: Widget ()
drawResult = borderWithLabel (str "Result") $ hCenter (str "Solution found")

drawUI :: Game -> Widget ()
drawUI game = drawBoard game <+> (drawInstruction <=> drawStatus (cursor game) <=> drawResult)

app :: App Game () ()
app = App
  {
    appDraw = \s -> [drawUI s]
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = return ()
  , appAttrMap      = const attributes
  }

main :: IO ()
main = do
  endGame <- defaultMain app demo
  return ()