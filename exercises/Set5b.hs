-- Exercise set 5b: playing with binary trees

module Set5b where

import Mooc.Todo

-- The next exercises use the binary tree type defined like this:

data Tree a = Empty | Node a (Tree a) (Tree a)
  deriving (Show, Eq)

------------------------------------------------------------------------------
-- Ex 1: implement the function valAtRoot which returns the value at
-- the root (top-most node) of the tree. The return value is Maybe a
-- because the tree might be empty (i.e. just a Empty)

valAtRoot :: Tree a -> Maybe a
valAtRoot t = case t of
  Empty -> Nothing
  Node dat _ _ -> Just dat

------------------------------------------------------------------------------
-- Ex 2: compute the size of a tree, that is, the number of Node
-- constructors in it
--
-- Examples:
--   treeSize (Node 3 (Node 7 Empty Empty) Empty)  ==>  2
--   treeSize (Node 3 (Node 7 Empty Empty) (Node 1 Empty Empty))  ==>  3

treeSize :: Tree a -> Int
treeSize t = case t of
  Empty -> 0
  Node _ l r -> 1 + treeSize l + treeSize r

------------------------------------------------------------------------------
-- Ex 3: get the largest value in a tree of positive Ints. The
-- largest value of an empty tree should be 0.
--
-- Examples:
--   treeMax Empty  ==>  0
--   treeMax (Node 3 (Node 5 Empty Empty) (Node 4 Empty Empty))  ==>  5

treeMax :: Tree Int -> Int
treeMax t = case valAtRoot t of
  Nothing -> findMax 0 t
  Just a -> findMax a t
  where
    findMax n t = case t of
      Empty -> n
      Node m l r ->
        if n >= nl && n >= nr
          then n
          else
            if nr >= n && nr >= nl
              then nr
              else nl
        where
          nl = findMax m l
          nr = findMax m r

------------------------------------------------------------------------------
-- Ex 4: implement a function that checks if all tree values satisfy a
-- condition.
--
-- Examples:
--   allValues (>0) Empty  ==>  True
--   allValues (>0) (Node 1 Empty (Node 2 Empty Empty))  ==>  True
--   allValues (>0) (Node 1 Empty (Node 0 Empty Empty))  ==>  False

allValues :: (a -> Bool) -> Tree a -> Bool
allValues condition tree = foldTree condition True tree
  where
    foldTree condition b t = case t of
      Empty -> b
      Node v l r -> foldTree condition newb l && foldTree condition newb r
        where
          newb = (b && (condition v))

------------------------------------------------------------------------------
-- Ex 5: implement map for trees.
--
-- Examples:
--
-- mapTree (+1) Empty  ==>  Empty
-- mapTree (+2) (Node 0 (Node 1 Empty Empty) (Node 2 Empty Empty))
--   ==> (Node 2 (Node 3 Empty Empty) (Node 4 Empty Empty))

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f t = case t of
  Empty -> Empty
  Node v l r -> Node (f v) (mapTree f l) (mapTree f r)

------------------------------------------------------------------------------
-- Ex 6: given a value and a tree, build a new tree that is the same,
-- except all nodes that contain the value have been removed. Also
-- remove the subnodes of the removed nodes.
--
-- Examples:
--
--     1          1
--    / \   ==>    \
--   2   0          0
--
--  cull 2 (Node 1 (Node 2 Empty Empty)
--                 (Node 0 Empty Empty))
--     ==> (Node 1 Empty
--                 (Node 0 Empty Empty))
--
--      1           1
--     / \           \
--    2   0   ==>     0
--   / \
--  3   4
--
--  cull 2 (Node 1 (Node 2 (Node 3 Empty Empty)
--                         (Node 4 Empty Empty))
--                 (Node 0 Empty Empty))
--     ==> (Node 1 Empty
--                 (Node 0 Empty Empty)
--
--    1              1
--   / \              \
--  0   3    ==>       3
--   \   \
--    2   0
--
--  cull 0 (Node 1 (Node 0 Empty
--                         (Node 2 Empty Empty))
--                 (Node 3 Empty
--                         (Node 0 Empty Empty)))
--     ==> (Node 1 Empty
--                 (Node 3 Empty Empty))

cull :: Eq a => a -> Tree a -> Tree a
cull val tree = case tree of
  Empty -> Empty
  Node v l r -> if v == val then Empty else Node v (cull val l) (cull val r)

------------------------------------------------------------------------------
-- Ex 7: check if a tree is ordered. A tree is ordered if:
--  * all values to the left of the root are smaller than the root value
--  * all of the values to the right of the root are larger than the root value
--  * and the left and right subtrees are ordered.
--
-- Hint: allValues will help you here!
--
-- Examples:
--         1
--        / \   is ordered:
--       0   2
--   isOrdered (Node 1 (Node 0 Empty Empty)
--                     (Node 2 Empty Empty))   ==>   True
--
--         1
--        / \   is not ordered:
--       2   3
--   isOrdered (Node 1 (Node 2 Empty Empty)
--                     (Node 3 Empty Empty))   ==>   False
--
--           2
--         /   \
--        1     3   is not ordered:
--         \
--          0
--   isOrdered (Node 2 (Node 1 Empty
--                             (Node 0 Empty Empty))
--                     (Node 3 Empty Empty))   ==>   False
--
--           2
--         /   \
--        0     3   is ordered:
--         \
--          1
--   isOrdered (Node 2 (Node 0 Empty
--                             (Node 1 Empty Empty))
--                     (Node 3 Empty Empty))   ==>   True

isOrdered :: Ord a => Tree a -> Bool
isOrdered t = case t of
  Empty -> True
  Node v l r -> case (l, r) of
    (Empty, Empty) -> True
    (Empty, _) -> if vr > v then isOrdered r else False
    (_, Empty) -> if v > vl then isOrdered l else False
    (_, _) -> if v > vl && vr > v then isOrdered l && isOrdered r else False
    where
      vl = case valAtRoot l of
        Nothing -> v
        Just v' -> v'
      vr = case valAtRoot r of
        Nothing -> v
        Just v' -> v'

------------------------------------------------------------------------------
-- Ex 8: a path in a tree can be represented as a list of steps that
-- go either left or right.

data Step = StepL | StepR
  deriving (Show, Eq)

-- Define a function walk that takes a tree and a list of steps, and
-- returns the value at that point. Return Nothing if you fall of the
-- tree (i.e. hit a Empty).
--
-- Examples:
--   walk [] (Node 1 (Node 2 Empty Empty) Empty)       ==>  Just 1
--   walk [StepL] (Node 1 (Node 2 Empty Empty) Empty)  ==>  Just 2
--   walk [StepL,StepL] (Node 1 (Node 2 Empty Empty) Empty)  ==>  Nothing

walk :: [Step] -> Tree a -> Maybe a
walk steps t = case steps of
  [] -> valAtRoot t
  (x : xs) -> case t of
    Empty -> Nothing
    Node v l r -> case x of
      StepL -> walk xs l
      StepR -> walk xs r

------------------------------------------------------------------------------
-- Ex 9: given a tree, a path and a value, set the value at the end of
-- the path to the given value. Since Haskell datastructures are
-- immutable, you'll need to build a new tree.
--
-- If the path falls off the tree, do nothing.
--
-- Examples:
--   set [] 1 (Node 0 Empty Empty)  ==>  (Node 1 Empty Empty)
--   set [StepL,StepL] 1 (Node 0 (Node 0 (Node 0 Empty Empty)
--                                       (Node 0 Empty Empty))
--                               (Node 0 Empty Empty))
--                  ==>  (Node 0 (Node 0 (Node 1 Empty Empty)
--                                       (Node 0 Empty Empty))
--                               (Node 0 Empty Empty))
--
--   set [StepL,StepR] 1 (Node 0 Empty Empty)  ==>  (Node 0 Empty Empty)

set :: [Step] -> a -> Tree a -> Tree a
set steps val t = case steps of
  [] -> case t of
    Empty -> Empty
    Node v l r -> Node val l r
  (x : xs) -> case t of
    Empty -> Empty
    Node v l r -> case x of
      StepL -> Node v (set xs val l) r
      StepR -> Node v l (set xs val r)

------------------------------------------------------------------------------
-- Ex 10: given a value and a tree, return a path that goes from the
-- root to the value. If the value doesn't exist in the tree, return Nothing.
--
-- You may assume the value occurs in the tree at most once.
--
-- Examples:
--   search 1 (Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty))  ==>  Just [StepL]
--   search 1 (Node 2 (Node 4 Empty Empty) (Node 3 Empty Empty))  ==>  Nothing
--   search 1 (Node 2 (Node 3 (Node 4 Empty Empty)
--                            (Node 1 Empty Empty))
--                    (Node 5 Empty Empty))                     ==>  Just [StepL,StepR]

-- search :: Eq a => a -> Tree a -> Maybe [Step]
-- search val t = case t of
-- Empty -> Nothing
-- Node value l r ->
-- if value == val
-- then Just []
-- else
-- if null lst1
-- then
-- if null lst2
-- then Nothing
-- else Just lst2
-- else Just lst1
-- where
-- lst1 = searchM val l [StepL]
-- lst2 = searchM val r [StepR]
-- searchM v t1 lst1 = case t1 of
-- Empty -> []
-- Node v1 l r ->
-- if v1 == v
-- then lst1
-- else
-- if null left
-- then
-- if null right
-- then []
-- else lst1 ++ right
-- else lst1 ++ left
-- where
-- left = searchM v l [StepL]
-- right = searchM v r [StepR]
search :: Eq a => a -> Tree a -> Maybe [Step]
search val tree = searchM val tree []
  where
    searchM v Empty path = Nothing
    searchM v (Node value l r) path
      | value == v = Just path
      | otherwise = case (searchM v l (path ++ [StepL]), searchM v r (path ++ [StepR])) of
          (Nothing, Nothing) -> Nothing
          (Just lst, _) -> Just lst
          (_, Just lst) -> Just lst
