import Data.List

matrixSize = 4

possibleValues = [1..matrixSize]

{- 24 possible permutations for a matrix size of 4
[[1,2,3,4],[2,1,3,4],[3,2,1,4],[2,3,1,4],[3,1,2,4],
[1,3,2,4],[4,3,2,1],[3,4,2,1],[3,2,4,1],[4,2,3,1],
[2,4,3,1],[2,3,4,1],[4,1,2,3],[1,4,2,3],[1,2,4,3],
[4,2,1,3],[2,4,1,3],[2,1,4,3],[4,1,3,2],[1,4,3,2],
[1,3,4,2],[4,3,1,2],[3,4,1,2],[3,1,4,2]]
-}
possibleRows = permutations possibleValues

{- 
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
2      3 2 4 1     1
2      1 4 2 3     2

       3 1 2 2

-}
-- zed ([1,3,2,2],[3,2,1,2],[2,2,1,3],[2,2,3,1]) = [[4,1,3,2],[2,3,4,1],[3,2,1,4],[1,4,2,3]]
zed :: ([Int],[Int],[Int],[Int]) -> [[Int]]
zed (t1, t2, t3, t4) = 
    matrix where 
        matrix = [r1, r2, r3, r4]
        r1 = (findValid (t2!!0) (t4!!3) (filter (filterColumnCheck t1) possibleRows)); -- TODO: validate top row, remove from possibleRows list
        r2 = (findValid (t2!!1) (t4!!2) (filter (filterRowCheck r1) possibleRows)); -- cascade downwards r1
        r3 = (findValid (t2!!2) (t4!!1) (filter (\x -> filterRowCheck r1 x && 
                                                    filterRowCheck r2 x) possibleRows)); -- cascade downwards r1 r2
        r4 = (findValid (t2!!3) (t4!!0) (filter (\x -> filterColumnCheck (reverse t3) x && 
                                                    filterRowCheck r1 x && 
                                                    filterRowCheck r2 x && 
                                                    filterRowCheck r3 x) possibleRows));
                                                     -- cascade downwards r1 r2 r3, reversed t3
        -- TODO transpose and check again

-- firstValid looks at columns on the left and right side
-- (h:t) is a list of possible permutations of the rows
-- TODO: do we need to consider ALL permutations that work?
findValid :: Int -> Int -> [[Int]] -> [Int]
findValid right left (h:t)
    | trades h 0 == left && trades (reverse h) 0 == right = h
    | length (h:t) == 0 = [0,0,0,0]
    | otherwise = findValid right left t

-- FILTER FUNCTIONS THAT CHECKS THAT EACH ROW WORKS WITH THE ROW ABOVE IT

-- checks the first row based on the 4x4 matrix
-- based on the conditional columns above it
filterColumnCheck :: [Int] -> [Int] -> Bool
filterColumnCheck [a,b,c,d] list =
    constrainFirstRow a (list!!0) && 
    constrainFirstRow b (list!!1) && 
    constrainFirstRow c (list!!2) && 
    constrainFirstRow d (list!!3)

filterRowCheck :: [Int] -> [Int] -> Bool
filterRowCheck [r1] [l] = r1 /= l
filterRowCheck (hr1:tr1) (h:t) =
    hr1 /= h && 
    filterRowCheck tr1 t

-- returns true if the value below it is a valid entry
-- used for the FIRST row of the matrix only
constrainFirstRow :: Int -> Int -> Bool
constrainFirstRow top below
    | top == 4 = elem below [1]
    | top == 3 = elem below [1,2]
    | top == 2 = elem below [1,2,3]
    | top == 1 = elem below [4]

-- | counts the number of trade stops for a given line/column
-- It takes the list and the running sum
trades :: [Int] -> Int -> Int
trades [] _ = 1
trades [a] _ = 1
trades (a:b:t) sum
    | a < b = sum + 1 + trades (maximum [a,b]:t) sum
    | otherwise = sum + trades (maximum [a,b]:t) sum

-- while loop in haskell
-- takes a predicate, function, base, returns "a" once predicate fails
while :: (a -> Bool) -> (a -> a) -> a -> a
while p = until (not . p)

zed :: ([Int],[Int],[Int],[Int]) -> IO()
zed (top,right,bottom,left) = 
    let rc = row_stops (left,right)
        cc = col_stops (top,bottom)
    in mapM_ print (matrix_solve (matrix_gen_4 rc) cc)