module Game where

type Element= Maybe Int
type Row=[Element]
type Grid=[Row]

data Game = Game {
  cursor :: (Int, Int),
  grid :: Grid,
  previous :: Maybe Game
}

demoGrid :: Grid
demoGrid = [[Just 3, Just 6, Just 5, Just 5, Just 7, Just 1, Just 2, Just 5, Just 1],
    [Just 3, Just 6, Just 5, Just 5,Nothing, Just 1, Just 2, Just 5,Just 5],
    [Just 3, Just 6, Nothing, Just 5,Just 7, Just 1, Just 2, Just 5,Just 5],
    [Just 3, Just 6, Just 5,Just 5,Just 7, Just 1, Just 2, Just 5,Just 5],
    [Just 3, Just 6, Just 5,Just 5,Just 7, Just 1, Just 2, Just 5,Just 5],
    [Just 3, Just 6, Just 5,Just 5,Just 7, Just 1,Just 3, Just 5, Just 5],
    [Just 3, Just 6, Just 5,Just 5,Just 7,Just 1, Just 2, Just 5, Just 5],
    [Just 3, Just 6, Just 5,Just 5,Just 7, Just 1, Just 2, Just 5,Just 5],
    [Just 3, Just 6, Just 5,Just 5,Just 7, Just 1, Just 2, Just 5,Just 5]]

demo :: Game
demo = Game {
  cursor = (4, 4),
  grid = demoGrid,
  previous = Nothing
}