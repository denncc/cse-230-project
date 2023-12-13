module Solver where

import Data.Char
import Data.List
import Data.Maybe ( fromJust, isJust, catMaybes, fromMaybe )
import Test.QuickCheck

-----------------------------------------------------------------------------

data Sudoku = S [[Maybe Int]]
   deriving(Eq, Show)

matrix :: Sudoku -> [[Maybe Int]]
matrix (S mat) = mat

-----------------------------------------------------------------------------

initSudoku :: Sudoku
initSudoku = S (replicate 9 (replicate 9 Nothing))

validSudoku :: Sudoku -> Bool
validSudoku (S mat) = length mat == 9 && all (\r -> length r == 9) mat

noNothing :: Sudoku -> Bool
noNothing (S mat) = all isJust (concat mat)

-----------------------------------------------------------------------------


printS :: Sudoku -> IO ()
printS sudoku = putStr(foldr (\a b -> a ++ "\n" ++ b) "" (map (map mIntToStr) (matrix sudoku)))
  where
    mIntToStr x = case x of
      Nothing  -> '.'
      Just n   -> intToDigit n


readS :: FilePath -> IO Sudoku
readS file =
   do string <- readFile file
      return(strtoSudoku string)


listRows :: String -> [String]
listRows s = lines s


convert :: String -> [Maybe Int]
convert = map strToMInt
  where
    strToMInt c = 
      if c == '.'
        then Nothing
        else if isDigit c
          then Just (digitToInt c)
          else error "Invalid sudoku!"


strtoSudoku :: String -> Sudoku
strtoSudoku str = S (map convert (lines str))


-----------------------------------------------------------------------------

elemnum :: Gen (Maybe Int)
elemnum = frequency[ (9, return Nothing), (1, do {a <- choose (1,9);return (Just a)})]

instance Arbitrary Sudoku where
  arbitrary =
    do mat <- sequence [ sequence [ elemnum | j <- [1..9] ] | i <- [1..9] ]
       return (S mat)

prop_S :: Sudoku -> Bool
prop_S sudoku = validSudoku sudoku

genS :: Gen (Sudoku)
genS = do
  s <- arbitrary
  return s

-- unit test for Sudoku generate
-- main :: IO ()
-- main = quickCheck (forAll genS (\s -> prop_S s))

checkBlock :: [Maybe Int] -> Bool
checkBlock block = nub (catMaybes block) == catMaybes block

allblocks :: Sudoku -> [[Maybe Int]]
allblocks s = rows ++ columns ++ squares++ diagnals
-- allblocks s = rows ++ columns ++ squares
  where 
    rows=matrix s
    columns=transpose (matrix s)
    squares=squaresBlocks s
    diagnals=diagonalBlocks s

squaresBlocks :: Sudoku -> [[Maybe Int]]
squaresBlocks (S mat) = [squaresBlock (S mat) r c | r <- [0, 3, 6], c <- [0, 3, 6]]
  where squaresBlock (S mat) row col = [mat !! (r + row) !! (c + col) | r <- [0..2], c <- [0..2]]

diagonalBlocks :: Sudoku -> [[Maybe Int]]
diagonalBlocks (S mat) = [firstDiagonal, secondDiagonal]
  where firstDiagonal = [mat !! n !! n | n <- [0..8]]
        secondDiagonal = [mat !! n !! (8 - n) | n <- [0..8]]

-- prop_allblocks :: Sudoku -> Bool
-- prop_allblocks s = length (allblocks s) == 29 && elenum (allblocks s) == 261
--   where 
--     elenum []   = 0
--     elenum (x:xs) = length x + elenum xs 

-- genallblocks :: Gen (Sudoku)
-- genallblocks = do
--   s <- arbitrary
--   return s

-- unit test for update
-- main :: IO ()
-- main = quickCheck (forAll genallblocks (\s -> prop_allblocks s))

check :: Sudoku -> Bool  
check s = all checkBlock (allblocks s)


blankPos :: Sudoku -> (Int, Int)
blankPos (S mat) = (row, blankR(mat !! row))
  where row = blankC mat

blankR :: [Maybe Int] -> Int
blankR [] = 0
blankR (x:xs) = case x of
  Just _ -> 1+blankR xs
  _      -> 0
                       
blankC :: [[Maybe Int]] -> Int
blankC []     = 0
blankC (x:xs) = case (blankR x) of
  9 -> 1+blankC xs
  _ -> 0

  
prop_blankPoscheck :: Sudoku -> Bool
prop_blankPoscheck s = let (x, y) = blankPos s 
                        in ((matrix s) !! x !!y) == Nothing

-- unit test for blankPos
-- main :: IO ()
-- main = quickCheck prop_blankPoscheck


insertElem :: [a] -> Int-> a -> [a]
insertElem xs i x = take i xs ++ [x] ++ drop (i + 1) xs


prop_insertElem :: [Int] -> Int-> Int -> Property
prop_insertElem xs i x=  (i>=0 && i<length xs) ==>(insertElem xs i x) !! i == x

-- unit test for insertElem
-- main :: IO ()
-- main = quickCheck prop_insertElem

update :: Sudoku -> (Int, Int) -> Maybe Int -> Sudoku
update (S mat) (j, i) x = S(insertElem mat j (insertElem (mat !! j) i x)) 


prop_update :: Sudoku -> (Int, Int) -> Maybe Int -> Property
prop_update s (j, i) x = (x/=Nothing) ==> ((matrix (update s (j, i) x)) !! j) !! i == x

genupdateInput :: Gen (Sudoku, (Int, Int), Maybe Int)
genupdateInput = do
  s <- arbitrary
  j <- choose (0, 8)
  i <- choose (0, 8)
  x <- arbitrary
  return (s, (j, i), x)

-- unit test for update
-- main :: IO ()
-- main = quickCheck (forAll genupdateInput (\(s, pos, x) -> prop_update s pos x))


-------------------------------------------------------------------------------------------


solver :: Sudoku -> Sudoku
solver s  | check s = x
          | otherwise  = error "Invalid Sudoku"
        where (x:xs) = checkS [s]

extractsquare :: Sudoku -> (Int, Int) -> [Maybe Int]
extractsquare (S s) (j, i) = 
    [s !! r !! c | r <- gridjIndex, c <- gridiIndex]
    where
        gridj = (j `div` 3) * 3
        gridi = (i `div` 3) * 3
        gridjIndex = [gridj .. gridj + 3 - 1]
        gridiIndex = [gridi .. gridi + 3 - 1]

possbleInt :: Sudoku -> (Int, Int) -> [Maybe Int]
possbleInt (S s) pos@(j,i) 
  |(i==j)&&(i+j==8) = [x | x <- allJust, not (x `elem` (row++col++square++diagnal1++diagnal2))]
  |(i==j)           = [x | x <- allJust, not (x `elem` (row++col++square++diagnal1))]
  |(i+j==8)         = [x | x <- allJust, not (x `elem` (row++col++square++diagnal2))]
  |otherwise        = [x | x <- allJust, not (x `elem` (row++col++square))]
  where
    allJust  =[Just x|x<-[1..9]]
    row      =[s !! j !! n | n <- [0..8]]
    col      =[s !! n !! i | n <- [0..8]]
    square   = extractsquare (S s) pos
    -- diagnal1 = []
    -- diagnal2 = []
    diagnal1 = [s !! n !! n | n <- [0..8]]
    diagnal2 = [s !! n !! (8 - n) | n <- [0..8]]

fillblank :: Sudoku -> (Int, Int) -> [Sudoku] 
fillblank s pos = map (\i -> update s pos i) ints
    where 
      ints = possbleInt s pos
                          
checkS :: [Sudoku] -> [Sudoku]
checkS ss@(s:s') 
  | noNothing   s    = ss
  | otherwise        = checkS (foldr (\s acc -> (fillblank s (blankPos s)) ++ acc) [] ss) 
          

main :: IO()
main = do solvebypath "hard100.sud"


solvebypath :: FilePath -> IO()
solvebypath path = do 
  s <- readS path
  if noNothing s
    then printS s
  else printS(solver s)
    
         

answer :: Sudoku -> Sudoku -> Bool
answer s s' = check s' && (s' == solver s)
                         

prop_solver :: Sudoku -> Property
prop_solver s = (check s) ==> answer s s'
  where s' = (solver s)

-- unit test for solver
-- main :: IO ()
-- main = quickCheck prop_solver

