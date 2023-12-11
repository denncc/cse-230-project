import Brick(simpleMain)
import UI (ui)
import Game(Game)

main :: IO ()
main = simpleMain $ ui demo

demo :: Game
demo = let z=0 in [
    z, 6, z, z, z, z, z, 7, 3
  , z, 7, z, z, z, 1, 5, z, 4
  , z, z, z, z, 7, z, 1, z, z
  , 7, 5, z, 8, z, 6, 4, z, z
  , 3, z, 8, 9, 1, 5, 2, z, 7
  , z, z, 2, 7, z, 4, z, 5, 9
  , z, z, 6, z, 9, z, z, z, z
  , 2, z, 7, 5, z, z, z, 1, z
  , 5, 3, z, z, z, z, z, 9, z
  ]