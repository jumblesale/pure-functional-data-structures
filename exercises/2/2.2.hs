-- a very basic binary tree structure - a node is either Empty, or it has two children
data BinaryTree a = Empty | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Show)

-- member is just a facade to member'
member :: (Ord a) => a -> BinaryTree a -> Bool
member _ Empty = False
-- the value of the initial node will be our candidate to begin with
member x tree@(Node _ y _) = member' x y tree

-- member prime takes the search term as well as a candidate
member' :: (Ord a) => a -> a -> BinaryTree a -> Bool
-- when we get to a leaf, check for equality
member' x y Empty = x == y
member' x y (Node left z right)
-- if x < z then keep the previous candidate - this is from the text of the exercise
  | x < z     = member' x y left
-- x is >= the current node - this is our new candidate
  | otherwise = member' x z right

