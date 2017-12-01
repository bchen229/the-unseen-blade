# the-unseen-blade

An implementation of Kingdom of Zed number game (also known as the skyscraper game).

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

Implemented Bonus features:
1) Allow maps of Zed or arbitrary size, i.e. any n×n map for n >= 2
2) Display the output as a grid (not a list)
3) Implement one or more solving strategies, so that your program tries to find a solution intelligently (rather than just brute force). This will allow the program to handle large or difficult puzzles

Further explanation can be found at: http://www.cs.sfu.ca/CourseCentral/383/pjj/a1.html

`zed.hs` generates the solution by first generating rows that match the conditions and then creating matrices of the rows.
Then it would verify the matrices by applying the conditions on the columns. 

`zed2.hs` first generates all permutations of matrices that do not have duplicate rows. Then applies conditions to the rows and columns, which is more of a brute force approach.

```
SAMPLE KINGDOM OF ZED INPUT AND OUTPUT for a 5x5 matrix
*Main> zed([3,1,3,2,2],[2,2,2,1,3],[2,3,2,4,1],[1,5,3,2,2])
  3 1 3 2 2
2 1 5 2 3 4 2
2 4 2 3 5 1 2
3 3 4 5 1 2 2
5 1 2 3 4 5 1
1 5 1 4 2 3 3
  1 4 2 3 2
```
