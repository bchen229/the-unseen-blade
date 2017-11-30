{-|
    An implementation of a Kingdom of Zed (Skyscraper) puzzle solver.
    The rules are the following:
    - The Kingdom of Zed is an n×n grid of counties, and in each county there is one trading post.
    - The only information you need to provide is the ranking of each trading post.
    - The trading posts are ranked from 1 to n, based on how much they will pay for spices. 1 is the lowest, and n is the highest.
    - In each east-west row of Zed, there is one trading post of each rank from 1..n.
    - In each north-south column of Zed, there is one trading post of each rank from 1..n.
    - One merchant visits from each outer border of Zed. So there are n merchants travelling from the north, n from the east, n from the south, and n from the west.
    - Each merchant travels in a straight line until they reach the best trading post. So the merchants in the north travel south before heading home.
    - Along the way, the merchants visit other trading posts in order to lighten their load. 
    - The merchants are not fools. They will not visit a trading post if it is worse than any of the others they have already visited. For example, a merchant will visit the trading posts 1,3,4,n in that order. However, if the 3 trading post preceeded the 1, then they would skip the 1 (and only visit the 3,4,n posts).
    - The only information the merchants will give you is the total number of posts they visit on their route.

Format: The clues are given as a 4-tuple of lists, with one list for each side of the grid. 
The first number corresponds with the top left merchant
(i.e., most western on the northern border), and continue in a clockwise direction. 
The output is a list of lists, where each entry is a row of the map, going from north to south. 
The example function call below corresponds with the visual example below. 

zed ([1,3,2,2],[3,2,1,2],[2,2,1,3],[2,2,3,1]) = [[4,1,3,2],[2,3,4,1],[3,2,1,4],[1,4,2,3]]
([top >], [right v], [bottom <], [left ^]) 
The symbols indicate which direction you're supposed to visualize the numbers

Initial visualization of the puzzle
       1 3 2 2
1                  3
3                  2
2                  1
2                  2
       3 1 2 2

Solution:
       1 3 2 2

1      4 1 3 2     3
3      2 3 4 1     2
2      3 2 1 4     1
2      1 4 2 3     2

       3 1 2 2

-}
import Data.List

-- comes in row form
-- we can get columns with transpose [[Int]]
-- use permutations [Int] to generate rows and check if conform
-- zed ([1,3,2,2],[3,2,1,2],[2,2,1,3],[2,2,3,1]) = [[4,1,3,2],[2,3,4,1],[3,2,1,4],[1,4,2,3]]
zed :: ([Int],[Int],[Int],[Int]) -> IO() 
zed (top,right,bottom,left) = 
    let rc = rowStops (left,right)
        cc = colStops (top,bottom)
        outputMatrix = (appendStops (matrixSolve (matrixGen4 rc) cc) rc cc)
    in 
        putStrLn [ if x == '0' then ' ' else x | x <- (showMatrix outputMatrix)]

-- takes in the matrix, row stops, column stops
-- returns the matrix with the row and column stops appended
appendStops :: [[Int]] -> [(Int,Int)] -> [(Int,Int)] -> [[Int]]
appendStops mx rs cs =
    let 
        mx_trans = transpose mx
    in 
        let 
            mx_imm = transpose [ [fst $ cs!!ind] ++ mx_trans!!ind ++ [snd $ cs!!ind] | ind <- [0..length cs - 1] ]
        in 
            [[0] ++ head mx_imm ++ [0]] ++ 
            [ [fst $ rs!!ind] ++ mx_imm!!(ind + 1) ++ [snd $ rs!!ind] | ind <- [0..length rs - 1] ] ++ 
            [[0] ++ last mx_imm ++ [0]]


-- printMatrix code in reference to author's codepad: http://codepad.org/48Vxg7hZ
-- author: https://stackoverflow.com/users/1106679/david
-- Prints the matrix
showMatrix = destructList " " . (map (map show))

-- converts [[a,a,a],[b,b,b]] where inner list is separated by spaces 
-- whereas the lists are separated by new lines
destructList :: String -> [[String]] -> String
destructList replaceComma = intercalate "\n" . map (intercalate replaceComma)

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
-- It takes the row/column and the the trade stop pair for the row/column
validate :: [Int] -> (Int,Int) -> Bool
validate list (f,b) 
    | (trades list 0 == f) && 
        (trades (reverse list) 0 == b) = True
    | otherwise = False

-- | given a matrix and the coressponding list of stops, 
-- validate that the condition is statisfied
validateMatrix :: [[Int]] -> [(Int,Int)] -> Bool
validateMatrix [] [] = True
validateMatrix (x:xs) (s:ss)
    | validate x s = validateMatrix xs ss
    | otherwise = False

-- | get trade stop pairs for row validation
rowStops :: ([Int],[Int]) -> [(Int,Int)]
rowStops (x,y) = zip (reverse x) y

-- | get trade stop pairs for column validate
colStops :: ([Int],[Int]) -> [(Int,Int)]
colStops (x,y) = zip x $ reverse y

-- | given a list of lists and a trade stop pair, 
-- returns lists that would satisfy the condition
listSelect :: [[Int]] -> (Int,Int) -> [[Int]]
listSelect lists (f,b) = [ x | x <- lists, validate x (f,b) ]

-- | takes in stop pairs generates a list of list of possible
-- selection for each matrix row
matrixGen4 :: [(Int,Int)] -> [[[Int]]]
matrixGen4 pairs =
    let p = [ x | x <- [ listSelect (permutations [1..(length pairs)]) (f,b) | (f,b) <- pairs ] ]
    in [ [a,b,c,d] | a <- p!!0, b <- p!!1, c <- p!!2, d <- p!!3 ]
        
-- | takes in a list of matrices and transposes and checks the stops constraints
-- returns the correct matrix
matrixSolve :: [[[Int]]] -> [(Int,Int)] -> [[Int]]
matrixSolve matrices pairs = head ([ x | x <- matrices, validateMatrix (transpose x) pairs ])

