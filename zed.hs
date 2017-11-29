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
-}
import Data.List

-- comes in row form
-- we can get columns with transpose [[Int]]
-- use permutations [Int] to generate rows and check if conform
zed :: ([Int],[Int],[Int],[Int]) -> [[Int]]
zed ([1,3,2,2],[3,2,1,2],[2,2,1,3],[2,2,3,1]) = [[4,1,3,2],[2,3,4,1],[3,2,1,4],[1,4,2,3]]
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

-- | get row stop pairs for validation
column_stops :: ([Int],[Int]) -> [(Int,Int)]
column_stops (x,y) = zip x $ reverse y

-- | get column stop pairs for validation
row_stop :: ([Int],[Int]) -> [(Int,Int)]
row_stop (x,y) = zip (reverse y) x
