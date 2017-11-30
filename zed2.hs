import Data.List
import Control.Monad

-- row stop = row_stop forth second
-- column stop = column_stop first third

-- | counts the number of trade stops for a given line/column
-- It takes the list and the running sum
trades :: [Int] -> Int -> Int
trades [] _ = 1
trades [a] _ = 1 
trades (a:b:t) sum
    -- deals with case where we have duplicates
    | a == b = -sum
    | a < b = sum + 1 + trades (maximum [a,b]:t) sum
    | otherwise = sum + trades (maximum [a,b]:t) sum

-- | validate row/colum to see if the trade stops are correct
-- It takes the row/column and the the stop pair for the row/column
validate :: [Int] -> (Int,Int) -> Bool
validate list (f_stops,b_stops) 
    | (trades list 0 == f_stops) 
        && (trades (reverse list) 0 == b_stops) = True
    | otherwise = False

-- | get row stop pairs for validation (left, right)
rowStops :: ([Int],[Int]) -> [(Int,Int)]
rowStops (x,y) = zip (reverse x) y

-- | get column stop pairs for validation (top, bottom)
columnStops :: ([Int],[Int]) -> [(Int,Int)]
columnStops (x,y) = zip x (reverse y)

-- Get the column i of a board as a list
getBoardColumn :: [[Int]] -> Int -> [Int]
getBoardColumn rows i = map (\l -> l !! i) rows

-- Get the row i of a board as a list
getBoardRow :: [[Int]] -> Int -> [Int]
getBoardRow rows i = rows !! i

-- generatePossibleBoards n
-- will create a list of all possible boards size n x n while ignoring duplicating rows
-- base on: https://stackoverflow.com/a/35147055
generatePossibleBoards :: Int -> [[[Int]]]
generatePossibleBoards n = concatMap permutations (helper (permutations [1..n]) []) where
    helper []     r = if length r == n then [r] else []
    helper (x:xs) r | length r == n = [r]
                    | otherwise = helper xs (x:r) ++ helper xs r

-- will create a list of all possible boards size n x n but doesn't ignore duplicate rows
generatePossibleBoardsDumb :: Int -> [[[Int]]]
generatePossibleBoardsDumb n = replicateM n (permutations [1..n])

-- Check a board to see whether its valid
validateBoard :: [[Int]] -> Int -> [(Int,Int)] -> [(Int,Int)] -> Bool
validateBoard board index rowStops columnStops
    | index < 0 = True -- based case: all prev indices are good
    | not (validate cRow cRowStop) || not (validate cCol cColStop) = False -- found a row or column that's invalid
    | otherwise = validateBoard board (index - 1) rowStops columnStops -- try the next index
        where -- retrive row, column and stops at the given index
            cRow = getBoardRow board index
            cRowStop = rowStops !! index
            cCol = getBoardColumn board index
            cColStop = columnStops !! index

-- Return all possible valid board for a Zed game when given the outter border value
zed2 :: ([Int],[Int],[Int],[Int]) -> [[Int]]
zed2 (top,right,bottom,left) =
    -- go thru all possible board and find those are satify stop conditions, return the first 1 that does
    head $ filter (\board -> validateBoard board (boardSize - 1) rs cs) possibleBoards
        where
            boardSize = length top
            possibleBoards = generatePossibleBoards boardSize
            rs = rowStops (left,right) -- row stop tupple to iterate thru
            cs = columnStops (top, bottom) -- col stop tupple to iterate thru









