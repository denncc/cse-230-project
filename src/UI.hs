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
import Data.Bool (Bool)

styleCursor, styleCellGiven, styleCellInput, styleCellNote :: AttrName
styleSolved, styleUnsolved :: AttrName
styleCursor    = attrName "styleCursor"
styleDiag    = attrName "styleDiag"
styleCellGiven = attrName "styleCellGiven"
styleCellInput = attrName "styleCellInput"
styleCellNote  = attrName "styleCellNote"
styleSolved    = attrName "styleSolved"
styleUnsolved  = attrName "styleUnsolved"

attributes :: AttrMap
attributes = attrMap V.defAttr
  [ (styleCursor    , bg V.brightBlack)
  , (styleDiag      , bg V.blue)
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
    V.KEnter    -> solveGame game
    _           -> game
handleEvent _ = continueWithoutRedraw

createElement :: (Int, Int)-> (Int, Int) -> Element -> Widget ()
createElement pos@(x,y) cursor e
  | pos==cursor     = withAttr styleCursor $ mapElement e
  | x==y || x+y==8  = withAttr styleDiag $ mapElement e
  | otherwise       = mapElement e
drawBoard :: Game -> Widget ()
drawBoard game = borderWithLabel (str "Board") $ renderTable $ table $ map (map mapElementWithCursor) $ grid game
  where
      mapElement :: Element -> Widget ()
      mapElement Nothing = str " "
      mapElement (Just v) = str $ show v


createByCol :: (Int, Int)-> (Int, Int) -> Row -> [Widget ()]
createByCol pos@(row,col) cursor (c:cs) = createElement pos cursor c:createByCol (row,col+1) cursor cs
createByCol _ _ _ = []

createByRow :: Int -> (Int, Int) -> Grid -> [[Widget ()]]
createByRow row cursor (r:rs) = createByCol (row,0) cursor r:createByRow (row+1) cursor rs
createByRow _ _ _ = []

drawBoard :: Game -> Widget ()
drawBoard game = borderWithLabel (str "Board") $ renderTable $ table $ createByRow 0 (cursor game) (grid game)

drawInstruction :: Widget ()
drawInstruction = borderWithLabel (str "Instruction") $ vBox $ map str [
  "Move: ↑, ↓, ←, →",
  "Fill num: 1-9",
  "Erase: Backspace",
  "Solve: Enter",
  "Reset: ^R",
  "Undo: ^Z",
  "Quit: ^C"
  ]

drawStatus :: (Int, Int) -> Int -> Widget ()
drawStatus cursor prog = borderWithLabel (str "Status") $ vBox $ map str [
  "Cursor: " ++ show cursor,
  "Progress: " ++ show prog ++ "%"
  ]

drawResult :: String -> Widget ()
drawResult message = borderWithLabel (str "Result") $ str message

drawUI :: Game -> Widget ()
drawUI game = drawBoard game <+> (drawInstruction <=> drawStatus (cursor game) (progress game) <=> drawResult (message game))

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