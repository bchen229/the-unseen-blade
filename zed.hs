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

zed :: ([Int],[Int],[Int],[Int]) -> [[Int]]
zed ([],[],[],[]) = [[]]
zed ([1,3,2,2],[3,2,1,2],[2,2,1,3],[2,2,3,1]) = [[4,1,3,2],[2,3,4,1],[3,2,1,4],[1,4,2,3]]