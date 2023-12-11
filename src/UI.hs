module UI where

import Brick (Widget, (<+>) , (<=>), str, withBorderStyle, joinBorders)
import Brick.Widgets.Center (center, hCenter)
import Brick.Widgets.Border (borderWithLabel, vBorder)
import Brick.Widgets.Border.Style (unicode)
import Game(Game)

drawBoard :: Game -> Widget ()
drawBoard game = borderWithLabel (str "Board") $ center (str "board")

drawInstruction :: Widget ()
drawInstruction = borderWithLabel (str "Instruction") $ hCenter (str "Use ↑, ↓, ←, → to move cursor") <=> hCenter (str "Use 1-9 to fill the selected cell") <=> hCenter (str "Use Backspace to clear number in the selected cell")

drawStatus :: Widget ()
drawStatus = borderWithLabel (str "Status") $ hCenter (str "Cursor: ") <=> hCenter (str "Progress: ") 

drawResult :: Widget ()
drawResult = borderWithLabel (str "Result") $ hCenter (str "Solution found")

ui :: Game -> Widget ()
ui game = drawBoard game <+> (drawInstruction <=> drawStatus <=> drawResult)