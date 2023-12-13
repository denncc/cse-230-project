module Game where
import Prelude hiding (Right, Left)
import Solver

type Element= Maybe Int
type Row=[Element]
type Grid=[Row]

data Game = Game {
  cursor :: (Int, Int),
  grid :: Grid,
  previous :: Maybe Game,
  message :: String
}

data Direction = Up | Down | Left | Right

resetGame :: Game -> Game
resetGame game = Game {
  cursor = cursor game,
  grid = map (map f) $ grid game,
  previous = Just game,
  message = "" }
  where
    f _ = Nothing

wrap :: Int -> Int
wrap n
  | n>=9 = n-9
  | n<0 = n+9
  | otherwise = n

moveCursor :: Direction -> Int -> Game -> Game
moveCursor dir dist game = game { cursor = move dir dist $ cursor game }
  where
    move Up dist (x,y) = (wrap $ x-dist, y)
    move Down dist (x,y) = (wrap $ x+dist, y)
    move Left dist (x,y) = (x, wrap $ y-dist)
    move Right dist (x,y) = (x, wrap $ y+dist)

modifyCell :: Int -> Game -> Game
modifyCell val game = Game {
  cursor = cursor game,
  grid = modifyRow (cursor game) val $ grid game,
  previous = Just game,
  message = message game
}
  where
    modifyRow (x,y) val (r:rs) = case x of
      0 -> modifyCol y val r:rs
      _ -> r:modifyRow (x-1,y) val rs
      where
        modifyCol col val (c:cs) = case col of
          0 -> case val of
            0 -> Nothing
            _ -> Just val
            :cs
          _ -> c:modifyCol (col-1) val cs
        modifyCol _ _ _ = []
    modifyRow _ _ _ = []

solveGame :: Game -> Game
solveGame game = case solver $ S $ grid game of
  S grid -> Game {
    cursor = cursor game,
    grid = grid,
    previous = Just game,
    message = "Solution Found"
  }
  Invalid -> game {message = "Invalid Sudoku"}
  NoSolution -> game {message = "No Solution"}

progress :: Game -> Int
progress game = round (filled (grid game)/81*100)
  where
    filled grid = sum $ map (sum . map cell2int) grid
      where
        cell2int Nothing = 0
        cell2int (Just _) = 1

demoGrid :: Grid
demoGrid = [[Just 3, Just 5, Just 4, Just 8, Just 7, Nothing, Just 6, Nothing, Just 2],
    [Just 9, Just 2, Just 7, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Just 1, Just 3, Just 2, Nothing, Just 5, Nothing, Just 4],
    [Just 8, Just 9, Nothing, Just 7, Nothing, Just 4, Nothing, Nothing, Nothing],
    [Just 4, Nothing, Nothing, Just 2, Nothing, Just 5, Nothing, Nothing, Just 7],
    [Nothing, Just 7, Just 5, Nothing, Just 3, Nothing, Just 2, Just 4, Just 9],
    [Nothing, Nothing, Nothing, Nothing, Just 8, Just 7, Nothing, Nothing, Just 1],
    [Just 2, Just 1, Nothing, Just 4, Nothing, Nothing, Just 7, Just 6, Nothing],
    [Just 7, Nothing, Nothing, Just 1, Just 6, Just 2, Nothing, Nothing, Nothing]]



demo :: Game
demo = Game {
  cursor = (4, 4),
  grid = demoGrid,
  previous = Nothing,
  message = "Start of game"
}