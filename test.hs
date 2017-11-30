import Data.List
import Control.Monad

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

-- row stop = row_stop forth second
-- column stop = column_stop first third

-- | counts the number of trade stops for a given line/column
-- It takes the list and the running sum
trades :: [Int] -> Int -> Int
trades [] _ = 1
trades [a] _ = 1
trades (a:b:t) sum
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
row_stops :: ([Int],[Int]) -> [(Int,Int)]
row_stops (x,y) = zip (reverse x) y

-- | get column stop pairs for validation (top, bottom)
column_stops :: ([Int],[Int]) -> [(Int,Int)]
column_stops (x,y) = zip x (reverse y)

-- Get the column i of a board as a list
getBoardColumn :: [[Int]] -> Int -> [Int]
getBoardColumn rows i = map (\l -> l !! i) rows

-- Get the row i of a board as a list
getBoardRow :: [[Int]] -> Int -> [Int]
getBoardRow rows i = rows !! i

-- Check a board to see whether its valid
validateBoard :: [[Int]] -> Int -> [(Int,Int)] -> [(Int,Int)] -> Bool
validateBoard board index row_stops column_stops
    | index < 0 = True
    | not (validate cRow cRowStop) || not (validate cCol cColStop) = False
    | otherwise = validateBoard board (index - 1) row_stops column_stops
        where
            cRow = getBoardRow board index
            cRowStop = row_stops !! index
            cCol = getBoardColumn board index
            cColStop = column_stops !! index

-- Return all possible valid board for a Zed game when given the outter border value
zed :: ([Int],[Int],[Int],[Int]) -> [[[Int]]]
zed (top,right,bottom,left) =
    let boardSize = length top
        possibleBoards = generatePossibleBoards boardSize
        rs = row_stops (left,right)
        cs = column_stops (top, bottom)
    in filter (\board -> validateBoard board (boardSize - 1) rs cs) possibleBoards









