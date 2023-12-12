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
handleEvent be = halt 

drawBoard :: Game -> Widget ()
drawBoard game = borderWithLabel (str "Board") $ renderTable $ table $ map (map mapElement) $ grid game
  where
    mapElement :: Element -> Widget ()
    mapElement Nothing = str " "
    mapElement (Just v) = str $ show v

drawInstruction :: Widget ()
drawInstruction = borderWithLabel (str "Instruction") $ hCenter (str "Use ↑, ↓, ←, → to move cursor") <=> hCenter (str "Use 1-9 to fill the selected cell") <=> hCenter (str "Use Backspace to clear number in the selected cell")

drawStatus :: Widget ()
drawStatus = borderWithLabel (str "Status") $ hCenter (str "Cursor: (1,1)") <=> hCenter (str "Progress: 10%")

drawResult :: Widget ()
drawResult = borderWithLabel (str "Result") $ hCenter (str "Solution found")

drawUI :: Game -> Widget ()
drawUI game = drawBoard game <+> (drawInstruction <=> drawStatus <=> drawResult)

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